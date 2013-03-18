module Function.Scope where 


-- External Imports
import Debug.Trace (trace)
import Language.Haskell.Her.HaLay
import Data.List
import Control.Monad

-- Project Imports
import GHCIProc 
import FileUtils (splitPath) 
import TokenUtils 
import ListUtils 
import Function.TypeFooler

scopeIdentifier :: Tok
scopeIdentifier = Com "{-SCOPE-}"

insertScopes :: FilePath -> [[Tok]] -> IO [[Tok]]
insertScopes file tokens = do mapScope file (findLinesWithToken scopeIdentifier tokens) tokens

mapScope :: FilePath -> [[Tok]] -> [[Tok]] -> IO [[Tok]]
mapScope file [] tokens     = return tokens
mapScope file (t:ts) tokens = do 
    tokens <- insertScope file t tokens
    mapScope file ts tokens

insertScope :: FilePath -> [Tok] -> [[Tok]] -> IO [[Tok]]
insertScope file line tokens = do 
    let (filePath, fileName) = splitPath file
    case extractFunctionNameFromLine line of
      Nothing     -> return tokens 
      Just fnName -> do 
        let (str, fn, end) = extractFunction fnName tokens
        let scp = inScope scopeIdentifier fn
        case scp of 
          Nothing  -> errOut "Failed to get scope" scopeIdentifier (str, fn, end) 
          Just scp -> do
            let scope = filter (/="undefined") $ filter (/=fnName) scp
            mapping <- mapM (findTypeOfVarAtTok file scopeIdentifier (str, fn, end)) scope
            let typeMap = makeScopeComment scope mapping
            let mappingCom = Com ("{- In Scope [ " ++ (intercalate ", " typeMap) ++ " ] -}")
            let fnmapped = replaceFirstTokenOcc scopeIdentifier mappingCom fn
            return (str ++ fnmapped ++ end)


makeScopeComment :: [String] -> [Maybe String] -> [String]
makeScopeComment [] [] = [] 
makeScopeComment (n:ns) (Nothing : ts) = (n ++ " :: Unknown") : makeScopeComment ns ts
makeScopeComment (n:ns) (Just t : ts)  = (n ++ " :: " ++ t) : makeScopeComment ns ts


inScope :: Tok -> [[Tok]] -> Maybe [String]
inScope tok []     = Nothing
inScope tok (t:ts) = case elemToken tok t of 
    False -> inScope tok ts
    True  -> case inLineFunctionNames t of 
      Nothing    -> Just (nub (scope t))
      Just names -> Just (nub ((scope t) ++ (filter (/="") names)))
    where 
      scope [] = [] 
      scope ((L _ lss) : ts)  = case elemTokenArr tok lss of 
        True  -> concat (map scope lss) 
        False -> case elemToken tok ts of 
          True  -> scope ts
          False -> []
      scope ((B _ rs) : ts)   = takeLid rs ++ scope ts
      scope ((T _ rs) : ts)   = scope rs ++ scope ts
      scope ((Lid var) : ts)  = var : scope ts
      scope ((Sym "=>") : ts) = []
      scope ((Sym "::") : ts) = []
      scope (t:ts)            = case t == tok of 
        True  -> case elemToken (Sym "=") ts of 
          True  -> takeLid ts
          False -> [] 
        False -> case elemToken tok ts of 
          True  -> scope ts
          False -> [] 
      takeLid [] = []
      takeLid ((Lid name) : ts) = name : takeLid ts
      takeLid (t:ts) = takeLid ts

inLineFunctionNames :: [Tok] -> Maybe [String]
inLineFunctionNames [] = Nothing
inLineFunctionNames t = case getLWhere t of 
  Nothing -> Nothing
  Just ts -> Just (nub (getFnNames ts))
  where 
    getLWhere []             = Nothing
    getLWhere ((L a b) : _ ) = Just b
    getLWhere (t:ts)         = getLWhere ts
    getFnNames []     = [] 
    getFnNames (t:ts) = getIFns t : getFnNames ts 
    getIFns []                 = [] 
    getIFns ((Spc _ ):ts)      = getIFns ts 
    getIFns ((Lid fnName) :ts) = fnName
    getIFns ((Sym "=") : _ )   = []
    getIFns (t:ts)             = getIFns ts
