module Function.TypeLineInsert where

-- External Imports
import Debug.Trace (trace)
import Language.Haskell.Her.HaLay

-- Project Imports
import GHCIProc 
import FileUtils (splitPath) 
import TokenUtils 
import ListUtils 

tlIdentifier :: Tok
tlIdentifier = Com "{-TYPELINE-}"

insertTypeLines :: FilePath -> FilePath -> [[Tok]] -> IO [[Tok]]
insertTypeLines ghci file tokens = mapInsert ghci file (findLinesWithToken tlIdentifier tokens) tokens

mapInsert :: FilePath -> FilePath -> [[Tok]] -> [[Tok]] -> IO [[Tok]]
mapInsert ghci file [] tokens     = return tokens
mapInsert ghci file (t:ts) tokens = do 
    tokens <- insertType ghci file t tokens
    mapInsert ghci file ts tokens

{-|
  Insert a functions Type Signature at the start of a function if its missing
-}
insertType :: FilePath -> FilePath -> [Tok] -> [[Tok]] -> IO [[Tok]]
insertType ghci file line tokens = do 
    let (filePath, fileName) = splitPath file
    -- 1) Extract Function Name from line
    case extractFunctionNameFromLine line of
      Nothing -> return tokens 
      Just fnName -> do 
        let (str, fn, end) = extractFunction fnName tokens
        fn <- ensureFunctionHasType ghci fnName fn file
        let fn2 = deleteFirstTokenOcc tlIdentifier fn
        return (str ++ fn2 ++ end)

{-|
  Make sure that the set of tokens for the function passed in contains a 
  high level type line.
  If it doesn't attempt to get one from GHCI
-}
ensureFunctionHasType :: FilePath -> String -> [[Tok]] -> FilePath -> IO [[Tok]]
ensureFunctionHasType ghci name tokens file = case head tokens of 
    [] -> return []
    xs -> case elemToken (Sym "::") xs of 
      True  -> return tokens
      False -> do 
        fntype <- getFunctionTypeFromGHCI ghci file name
        case fntype of 
          Nothing -> return tokens
          Just fntype -> do 
            let typeTokens = tokeniseString fntype -- Need to check if the line doesnt end with this already
            return $ typeTokens ++ ([NL ("", 0)] : tokens)

{-| 
  Query GHCI for the type of a function
-}
getFunctionTypeFromGHCI :: FilePath -> FilePath -> String -> IO (Maybe String)
getFunctionTypeFromGHCI ghci file name = do 
    let cmd = TYPEINFO name 
    resp <- runCommandList ghci file [cmd]
    case lookup cmd resp of 
        Nothing   -> return Nothing 
        Just resp -> return $ Just (readGhciOutput resp)

