module Function.PatternSplitter where

-- External Imports
import System.FilePath ((</>))
import System.Directory (removeDirectoryRecursive)
import Language.Haskell.Her.HaLay 
import Data.List.Split (splitOn)
import Data.List (intercalate, elem, isSuffixOf, intersperse, cycle, subsequences, null)
import Text.ParserCombinators.Parsec (parse, manyTill, anyChar, try, string, manyTill)
import Data.Maybe (mapMaybe)
import Data.Char
import Debug.Trace (trace)
import System.IO

-- Project Imports
import GHCIProc 
import FileUtils (splitPath) 
import TokenUtils 
import ListUtils 
import Type
import Function.TypeFooler
import Function.Scope


splitIdentifier :: Tok
splitIdentifier = Com "{-SPLIT-}"

splitPatterns :: FilePath -> FilePath -> [[Tok]] -> IO [[Tok]]
splitPatterns ghci file tokens = do mapSplit ghci file (findLinesWithToken splitIdentifier tokens) tokens

mapSplit :: FilePath -> FilePath -> [[Tok]] -> [[Tok]] -> IO [[Tok]]
mapSplit ghci file [] tokens     = return tokens
mapSplit ghci file (t:ts) tokens = do 
    tokens <- splitIt ghci file t tokens
    mapSplit ghci file (findLinesWithToken splitIdentifier tokens) tokens

{-|
  This function splits a pattern varibles in a fucntion into all of its constructors 
-}
splitIt :: FilePath -> FilePath -> [Tok] -> [[Tok]] -> IO [[Tok]]
splitIt ghci file line tokens = do 
    let (filePath, fileName) = splitPath file
    -- 1) Extract Function Name from line
    case extractFunctionNameFromLine line of
      Nothing -> return tokens -- Insert Error Message?
      Just fnName -> do 
        let (str, fn, end) = extractFunction fnName tokens
        fn <- ensureFunctionHasType ghci fnName fn file
        let vName = getVariableNameToSplit fn
        case vName of 
          Nothing -> errOut "Couldn't parse variable name to split" splitIdentifier (str, fn, end) 
          Just vName -> do 
            mType <- findTypeOfVarAtTok ghci file splitIdentifier (str, fn, end) vName
            case mType of
              Nothing -> errOut "GHCI Failed to parse the file" splitIdentifier (str, fn, end) 
              Just mType -> do 
                pType <- resolveType ghci mType tokens file
                case pType of 
                  Left err -> errOut "Failed to resolve the variables type" splitIdentifier (str, fn, end) 
                  Right types -> do 
                      hPutStrLn stderr ("Type lookup >> " ++ show mType ++ " >> " ++ show types)
                      let scope = inScope splitIdentifier fn 
                      hPutStrLn stderr ("Current scope >> " ++ show scope)
                      let pats = map (stringifyPattern vName scope) types
                      hPutStrLn stderr ("Patterns >> " ++ show pats)
                      let bindings = bindPatterns fn pats 
                      hPutStrLn stderr ("Binding >> " ++ show bindings)
                      return (str ++ bindings ++ end) 

bindPatterns :: [[Tok]] -> [String] -> [[Tok]]
bindPatterns [] [] = [] 
bindPatterns (t:ts) ns = case elemToken splitIdentifier t of 
  False -> t : bindPatterns ts ns
  True  -> if isNested t then (seekL t) : ts else 
      (intersperse [(NL (".hers", 0))] (map (generateLine t) ns)) ++ ts  
    where 
      isNested [] = True
      isNested (B _ ls : ts) = if elemToken splitIdentifier ls then False else isNested ts
      isNested (T _ ls : ts) = if elemToken splitIdentifier ls then False else isNested ts
      isNested (t:ts) = if t == splitIdentifier then False else isNested ts
      getBefore [] = [] 
      getBefore (t:tt:ts) = if elemToken splitIdentifier tt then [] else t : getBefore (tt:ts)
      getBefore (t:ts) = if elemToken splitIdentifier t then [] else t : getBefore ts
      getNested [] = [] 
      getNested (t:tt:ts) = if elemToken splitIdentifier tt then t : tt : [] else getNested (tt:ts)
      getNested (t:ts) = if elemToken splitIdentifier t then t : [] else getNested ts
      getAfter [] = [] 
      getAfter (t:tt:ts) = if elemToken splitIdentifier tt then ts else getAfter (tt:ts)
      getAfter (t:ts) = if elemToken splitIdentifier t then ts else getAfter ts
      doInsert ts = getBefore ts ++ (insert (getNested ts)) ++ getAfter ts
      seekL [] = [] 
      seekL (L l ls : ts) = if elemTokenArr splitIdentifier ls then L l (doInsert ls) : ts else L l ls : seekL ts
      seekL (t:ts) = t : seekL ts
      insert ts = map (generateLine (concat ts)) ns

generateLine :: [Tok] -> String -> [Tok]
generateLine line pat = fSplit line
    where
      insert [] = []
      insert ((Spc x) : ts) = (Spc x) : insert ts
      insert ((Lid _) : ts) = (concat $ tokeniseString pat) ++ ts
      insert (t:ts) = t : insert ts
      fSplit [] = []
      fSplit ((B b rs):ts)    = case elemToken splitIdentifier rs of 
        True  -> (B b (fSplit rs)) : ts 
        False -> (B b rs) : fSplit ts 
      fSplit ((T t rs):ts)    = case elemToken splitIdentifier rs of 
        True  -> (T t (fSplit rs)) : ts
        False -> (T t rs) : fSplit ts
      fSplit ((Com "{-SPLIT-}") : ts) = insert ts
      fSplit (t:ts) = t : fSplit ts

stringifyPattern :: String -> [String] -> Type -> String
stringifyPattern vn scope (iden, lay, []) = iden
stringifyPattern vn scope (iden, lay, ps) = brackets (layout lay (genSensibleNames vn (filter (/=vn) scope) ps))
    where 
      brackets [] = []
      brackets ('(':bs) = '(' : bs
      brackets ('[':bs) = '[' : bs
      brackets (' ':bs) = brackets bs
      brackets bs = "(" ++ bs ++ ")"
      layout [] _ = []
      layout ('{': '?' : '}' : ls) (n:ns) = n ++ (layout ls ns)
      layout (l:ls) ns = [l] ++ (layout ls ns)

{-|
  
-}
genSensibleNames :: String -> [String] -> [Parameter] -> [String]
genSensibleNames vname _ [] = [] 
genSensibleNames vname scope ((st, Nothing) : ps) = mkName : genSensibleNames vname (mkName : scope) ps
  where 
    mkName = if elem vname scope then modName 1 else vname
    modName i = if elem (vname ++ (show i)) scope then modName (i+1) else (vname ++ (show i))
genSensibleNames vname scope ((_, Just str) : ps)  = mkName : genSensibleNames vname (mkName : scope) ps
  where 
    mkName = if elem str scope then modName 1 else str
    modName i = if elem (str ++ (show i)) scope then modName (i+1) else (str ++ (show i))
    
{-|
  getVariableNameToSplit will traverse the tokens and find the name of
  the variable after {-SPLIT-}
-}
getVariableNameToSplit :: [[Tok]] -> Maybe String
getVariableNameToSplit []     = Nothing
getVariableNameToSplit (l:ls) = case elemToken splitIdentifier l of 
    True  -> (fSplit l)
    False -> getVariableNameToSplit ls
    where 
      fSplit [] = Nothing
      fSplit ((L _ ls) : ts) = case getVariableNameToSplit ls of 
        Just name  -> Just name
        Nothing -> fSplit ts
      fSplit ((B _ rs):ts)    = case fSplit rs of 
        Just name  -> Just name
        Nothing -> fSplit ts
      fSplit ((T _ rs):ts)    = case fSplit rs of 
        Just name  -> Just name
        Nothing -> fSplit ts
      fSplit ((Com "{-SPLIT-}") : ts) = gName ts
      fSplit (t:ts) = fSplit ts
      gName [] = Nothing
      gName ((Lid name) : ts) = Just name
      gName (t : ts) = gName ts


getConstructor :: FilePath -> Type -> [[Tok]] -> FilePath -> IO (Maybe Data)
getConstructor ghci (name, layout, params) tokens file = do 
    case lookupType (name, layout, params) of 
        Just cons -> return (Just cons)
        Nothing -> do 
            let resp = getConstructorFromTok name tokens
            case resp of 
              Just cons -> return (Just cons)
              Nothing -> do 
                resp <- getConstructorFromGHCI ghci name file
                return resp

getConstructorFromTok :: String -> [[Tok]] -> Maybe Data
getConstructorFromTok _ _ = Nothing 

getConstructorFromGHCI :: FilePath -> String -> FilePath -> IO (Maybe Data)
getConstructorFromGHCI ghci name file = do 
    let cmd = INFO name
    resp <- runCommandList ghci file [cmd]
    case lookup cmd resp of 
        Nothing -> return Nothing
        Just resp -> do 
            case readGhciOutput resp of 
                [] -> return Nothing  
                resp -> return $ toDataFromGhci resp

resolveType :: FilePath -> String -> [[Tok]] -> FilePath -> IO (Either String [Type]) 
resolveType ghci name tokens file = do 
    case isPrimitive name of 
      True  -> return (Left "Type is primitive")
      False -> do 
        let uType = unwrapType name
        case uType of 
          Nothing -> return (Left ("Couldn't parse type returned from GHCI: " ++ name)) 
          Just uType -> do 
            case isSplittable uType of 
                True  -> return (Right [uType])
                False -> do 
                  types <- getConstructor ghci uType tokens file
                  case types of 
                    Nothing    -> return (Left "Couldnt find a valid constructor")
                    Just ((iden, lay, ps), types) -> do 
                      case lookupType (head types) of 
                        Just (_, dat) -> return (Right dat)
                        Nothing  -> return (Right types)

isSplittable :: Type -> Bool
isSplittable (i, l, ps) = isTuple i

isTuple :: String -> Bool
isTuple [] = False
isTuple (')' : []) = True
isTuple ('(' : xs) = isTuple xs
isTuple (',' : xs) = isTuple xs
isTuple (' ' : xs) = isTuple xs
isTuple _ = False

{-|
  Only attempt to split things that aren't primitives
-}
isPrimitive :: String -> Bool
isPrimitive t = t `elem`
    ["Char", "Double", "Float", "Int", "Int8", "Int16", "Int32",
     "Int64", "Word", "Word8", "Word16", "Word32", "Word64", "Addr"]

