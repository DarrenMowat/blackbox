module Function.TypeLineInsert where

-- External Imports
import Language.Haskell.Her.HaLay 
import Debug.Trace (trace)

-- Project Imports
import GHCIProc 
import FileUtils (splitPath) 
import TokenUtils 
import ListUtils 

tlIdentifier :: Tok
tlIdentifier = Com "{-TYPELINE-}"

insertTypeLines :: FilePath -> [[Tok]] -> IO [[Tok]]
insertTypeLines file tokens = do mapInsert file (findLinesWithToken tlIdentifier tokens) tokens

mapInsert :: FilePath -> [[Tok]] -> [[Tok]] -> IO [[Tok]]
mapInsert file [] tokens     = return tokens
mapInsert file (t:ts) tokens = do 
    tokens <- insertType file t tokens
    mapInsert file ts tokens

insertType :: FilePath -> [Tok] -> [[Tok]] -> IO [[Tok]]
insertType file line tokens = do 
    let (filePath, fileName) = splitPath file
    -- 1) Extract Function Name from line
    case extractFunctionNameFromLine line of
      Nothing -> return tokens 
      Just fnName -> do 
        let (str, fn, end) = extractFunction fnName tokens
        fn <- ensureFunctionHasType fnName fn file
        let fn2 = deleteFirstTokenOcc tlIdentifier fn
        return (str ++ fn2 ++ end)

{-|
  Make sure that the set of tokens for the function passed in contains a 
  high level type line.
  If it doesn't attempt to get one from GHCI
-}
ensureFunctionHasType :: String -> [[Tok]] -> FilePath -> IO [[Tok]]
ensureFunctionHasType name tokens file = case head tokens of 
    [] -> return []
    xs -> case elemToken (Sym "::") xs of 
      True  -> return tokens
      False -> do 
        fntype <- getFunctionTypeFromGHCI file name
        case fntype of 
          Nothing -> return tokens
          Just fntype -> do 
            let typeTokens = tokeniseString fntype -- Need to check if the line doesnt end with this already
            return $ typeTokens ++ ([NL ("", 0)] : tokens)

{-| 
  Query GHCI for the type of a function
-}
getFunctionTypeFromGHCI :: FilePath -> String -> IO (Maybe String)
getFunctionTypeFromGHCI file name = do 
    let cmd = TYPEINFO name 
    resp <- runCommandList file [cmd]
    case lookup cmd resp of 
        Nothing   -> return Nothing 
        Just resp -> do 
            return $ Just (readGhciOutput resp)

