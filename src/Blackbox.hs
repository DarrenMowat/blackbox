module Blackbox where

-- External Imports
import System.FilePath ((</>))
import System.Directory (removeDirectoryRecursive, getPermissions, executable, doesFileExist)

-- Project Imports
import FileUtils
import TokenUtils (tokeniseFile, untokeniseFile)
import Paths_blackbox (getDataFileName)
import GHCIProc


-- Project Functions
import Function.PatternSplitter (splitPatterns)
import Function.TypeLineInsert (insertTypeLines)
import Function.Scope (insertScopes)

-- Actually do some stuff

runBlackbox :: FilePath -> FilePath -> FilePath -> IO (Either String String)  
runBlackbox file mfile ghci = do
    -- 0) Before we do anything, we must ensure that we can gain access
    --    to out annotate.sh script.
    bash <- getDataFileName "annotate.sh"
    p <- getPermissions bash 
    case executable p of 
      False -> return (Left ("Blackbox uses a bash script to interact with GHCI, however the script is currently not executable. Please make the following file executable: " ++ bash))
      True -> do 
        -- 1) Make full copy of CWD, Don't want to damage the users files
        let (dir, name) = splitPath file
        newdir <- copyToNewTempDir dir
        let infile = newdir </> name
        -- 2) Tokenise the file before we pass it through Mirage.
        --    Mirage may change stuff in the file to coerce GHCi to load it
        --    Therfore we take a copy of the file, as tokens, before mirage 
        --    Does its stuff. Explain this in the report.
        tokens <- tokeniseFile mfile
        -- 3) Ensure the file doesn't contain any errors
        isComp <- isCompilable ghci infile
        case isComp of 
            Left errors -> return (Left errors)
            Right _     -> do 
              -- 4) Do something interesting to the file
              -- Split Patterns {-SPLIT-}
              postSplitTokens <- splitPatterns ghci infile tokens
              -- Insert Type Lines {-TYPELINE-}
              postInsertTypeTokens <- insertTypeLines ghci infile postSplitTokens
              -- Insert Scopes {-SCOPE-}
              postInsertScopes <- insertScopes ghci infile postInsertTypeTokens
              -- 4) Tidy up after ourselves, we're running in a temp dir but it's still nice to be tidy (OCD)
              removeDirectoryRecursive newdir
              return (Right (untokeniseFile postInsertScopes))


isCompilable :: FilePath -> FilePath -> IO (Either String ())
isCompilable ghci file = do 
    let (dir, name) = splitPath file 
    exists <- doesFileExist file
    case exists of 
        False -> return (mirageError name "Couldn't load file")
        True  -> do 
            response <- runCommandList ghci file [] 
            case lookup (LOAD name) response of 
                 Nothing -> return (mirageError name "Unknown Error")
                 Just loadResp -> case readGhciError loadResp of 
                        [] -> return (Right ())
                        es -> return (mirageError name (readFullResponse loadResp))

mirageError :: String -> String -> Either String ()
mirageError name error = Left ("Couldn't load " ++ name ++ " into GHCI - " ++ error)
