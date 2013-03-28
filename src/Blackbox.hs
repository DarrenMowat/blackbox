module Blackbox (runBlackbox) where

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


runBlackbox :: FilePath -> FilePath -> FilePath -> IO (Either String String)  
runBlackbox file mfile ghci = do
    -- 1) Before we do anything, we must ensure that we can gain access
    --    to out annotate.sh script. IF we can't execute it we'll need to get the user to 
    --    make it exectuable 
    bash <- getDataFileName "annotate.sh"
    p <- getPermissions bash 
    case executable p of 
      False -> return (Left ("Blackbox uses a bash script to interact with GHCI, however the script is currently not executable. Please make the following file executable: " ++ bash))
      True -> do 
        -- 2) Make copy of all the Haskell files in CWD, Don't want to damage the users files
        let (dir, name) = splitPath file
        newdir <- copyToNewTempDir dir
        let infile = newdir </> name
        tokens <- tokeniseFile mfile
        -- 3) Ensure the file doesn't contain any errors
        isComp <- isCompilable ghci infile
        case isComp of 
            Left errors -> return (Left errors)
            Right _     -> do 
              -- 4) Do something interesting to the file
              -- Insert Scopes {-SCOPE-}
              postInsertScopes <- insertScopes ghci infile tokens
              -- Split Patterns {-SPLIT-}
              postSplitTokens <- splitPatterns ghci infile postInsertScopes
              -- Insert Type Lines {-TYPELINE-}
              postInsertTypeTokens <- insertTypeLines ghci infile postSplitTokens
              -- 5) Tidy up after ourselves, we're running in a temp dir but it's still nice to be tidy
              removeDirectoryRecursive newdir
              return (Right (untokeniseFile postInsertTypeTokens))


{-|
    Function to test if a Haskell file is compilable.
-}
isCompilable :: FilePath -> FilePath -> IO (Either String ())
isCompilable ghci file = do 
    let (dir, name) = splitPath file 
    exists <- doesFileExist file
    case exists of 
        False -> return (Left ("Couldn't load " ++ name ++ " into GHCI - " ++ "Couldn't load file"))
        True  -> do 
            response <- runCommandList ghci file [] 
            case lookup (LOAD name) response of 
                 Nothing -> return (Left ("Couldn't load " ++ name ++ " into GHCI - " ++ "Unknown Error"))
                 Just loadResp -> case didFileLoad loadResp of 
                        True  -> return (Right ())
                        False -> return (Left ("Couldn't load " ++ name ++ " into GHCI - " ++ (readFullResponse loadResp)))

