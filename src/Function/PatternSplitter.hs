module Function.PatternSplitter where

-- External Imports
import System.FilePath ((</>))
import System.Directory (removeDirectoryRecursive)
import Language.Haskell.Her.HaLay 
import Data.List.Split (splitOn)
import Data.List (intercalate, elem, isSuffixOf, intersperse)
import Text.ParserCombinators.Parsec (parse, manyTill, anyChar, try, string, manyTill)
import Data.Maybe (mapMaybe)
import Debug.Trace (trace)
import System.IO

-- Project Imports
import GHCIProc 
import FileUtils (splitPath, findFile) 
import TokenUtils 
import ListUtils 
import Type
import Function.TypeFooler
import Function.Scope

names :: [String]
names = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o"]

splitIdentifier :: Tok
splitIdentifier = Com "{-SPLIT-}"

splitPatterns :: FilePath -> [[Tok]] -> IO [[Tok]]
splitPatterns file tokens = do mapSplit file (findLinesWithToken splitIdentifier tokens) tokens

mapSplit :: FilePath -> [[Tok]] -> [[Tok]] -> IO [[Tok]]
mapSplit file [] tokens     = return tokens
mapSplit file (t:ts) tokens = do 
    tokens <- splitIt file t tokens
    mapSplit file ts tokens

splitIt :: FilePath -> [Tok] -> [[Tok]] -> IO [[Tok]]
splitIt file line tokens = do 
    let (filePath, fileName) = splitPath file
    -- 1) Extract Function Name from line
    case extractFunctionNameFromLine line of
      Nothing -> return tokens -- Insert Error Message?
      Just fnName -> do 
        let (str, fn, end) = extractFunction fnName tokens
        fn <- ensureFunctionHasType fnName fn file
        let vName = getVariableNameToSplit fn
        case vName of 
          Nothing -> errOut "Couldn't parse variable name to split" splitIdentifier (str, fn, end) 
          Just vName -> do 
            mType <- findTypeOfVarAtTok file splitIdentifier (str, fn, end) vName
            case mType of
              Nothing -> errOut "GHCI Failed to parse the file" splitIdentifier (str, fn, end) 
              Just mType -> do 
                pType <- resolveType mType tokens file
                hPutStrLn stderr ("Type: " ++ show pType)
                case pType of 
                  Left err -> errOut "Failed to resolve the variables type" splitIdentifier (str, fn, end) 
                  Right types -> do 
                      let pats = concat $ map (stringifyType vName) types
                      let fn2 = insertPatterns fn pats
                      return (str ++ fn2 ++ end) 

insertPatterns :: [[Tok]] -> [String] -> [[Tok]]
insertPatterns fn []  = fn
insertPatterns (l:ls) ns = case elemToken splitIdentifier l of 
    True -> (intersperse [(NL (".hers", 0))] (map (generateLine l) ns)) ++ ls
    False -> l : insertPatterns ls ns

generateLine :: [Tok] -> String -> [Tok]
generateLine line pat = fSplit line
    where
      insert [] = []
      insert ((Spc x) : ts) = (Spc x) : insert ts
      insert ((Lid _) : ts) = (concat $ tokeniseString pat) ++ ts
      insert (t:ts) = t : insert ts
      fSplit [] = []
      fSplit ((L l ls) : ts) = case elemTokenArr splitIdentifier ls of 
        True  -> (L l (insertPatterns ls [pat])) : ts
        False -> (L l ls) : fSplit ts
      fSplit ((B b rs):ts)    = case elemToken splitIdentifier rs of 
        True  -> (B b (fSplit rs)) : ts 
        False -> (B b rs) : fSplit ts 
      fSplit ((T t rs):ts)    = case elemToken splitIdentifier rs of 
        True  -> (T t (fSplit rs)) : ts
        False -> (T t rs) : fSplit ts
      fSplit ((Com "{-SPLIT-}") : ts) = insert ts
      fSplit (t:ts) = t : fSplit ts

stringifyType :: String -> Type -> [String]
stringifyType vn ("[]", lay, ps) = stringifyArray vn 
stringifyType vn (iden, lay, []) = [iden]
stringifyType vn (iden, lay, ps) = [layout lay mknames]
    where 
      mknames = take (length ps) names
      layout [] _ = []
      layout ls [] = ls
      layout ('{': '?' : '}' : ls) (n:ns) = n ++ (layout ls ns)
      layout (l:ls) ns = [l] ++ (layout ls ns)


stringifyArray :: String -> [String]
stringifyArray vname = ["[]", "(x : xs)"]

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


{- Code to grab a constructor -}

getConstructor :: Type -> [[Tok]] -> FilePath -> IO (Maybe Data)
getConstructor (name, layout, params) tokens file = do 
    case lookupType (name, layout, params) of 
        Just cons -> return (Just cons)
        Nothing -> do 
            let resp = getConstructorFromTok name tokens
            case resp of 
              Just cons -> return (Just cons)
              Nothing -> do 
                resp <- getConstructorFromGHCI name file
                return resp

getConstructorFromTok :: String -> [[Tok]] -> Maybe Data
getConstructorFromTok name []     = Nothing 
getConstructorFromTok name (t:ts) = case findCons (trimSpaceToken t) of 
    Nothing -> getConstructorFromTok name ts
    Just _ -> toData t 
    where 
      findCons [] = Nothing
      findCons (Sym "=" : xs)   = Nothing 
      findCons (Sym "::" : xs)   = Nothing 
      findCons (KW "data" : xs) = findUid $ trimSpaceToken xs
      findCons (KW "type" : xs) = findUid $ trimSpaceToken xs
      findCons _ = Nothing
      findUid (Sym "=" : _)   = Nothing 
      findUid (Sym "::" : _)   = Nothing 
      findUid (Uid n : _) = if n == name then Just () else Nothing
      findUid (_:xs) = Nothing

getConstructorFromGHCI :: String -> FilePath -> IO (Maybe Data)
getConstructorFromGHCI name file = do 
    let cmd = INFO name
    resp <- runCommandList file [cmd]
    case lookup cmd resp of 
        Nothing -> return Nothing
        Just resp -> do 
            case readGhciOutput resp of 
                [] -> return Nothing  
                resp -> return $ toDataFromGhci resp


resolveType :: String -> [[Tok]] -> FilePath -> IO (Either String [Type]) 
resolveType name tokens file = do 
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
                  types <- getConstructor uType tokens file
                  case types of 
                    Nothing    -> return (Left "Couldnt find a valid constructor")
                    Just (typ, types) -> return (Right types)

isSplittable :: Type -> Bool
isSplittable (i, l, ps) = isArray i || isTuple i

isArray :: String -> Bool
isArray "[]" = True
isArray _    = False

isTuple :: String -> Bool
isTuple [] = False
isTuple (')' : []) = True
isTuple ('(' : xs) = isTuple xs
isTuple (',' : xs) = isTuple xs
isTuple _ = False

{-|
  Only attempt to split things that aren't primitives
-}
isPrimitive :: String -> Bool
isPrimitive t = t `elem`
    ["Char", "Double", "Float", "Int", "Int8", "Int16", "Int32",
     "Int64", "Word", "Word8", "Word16", "Word32", "Word64", "Addr"]

