module Blackbox where

-- External Imports
import System.FilePath ((</>))
import System.Directory (removeDirectoryRecursive)

-- Project Imports
import Mirage
import FileUtils
import TokenUtils (tokeniseFile, untokeniseArr)

-- Project Functions
import Function.PatternSplitter (splitPatterns)
import Function.TypeLineInsert (insertTypeLines)

-- Actually do some stuff

runBlackbox :: FilePath -> FilePath -> IO () 
runBlackbox file mfile = do
	-- 1) Make full copy of CWD, Don't want to damage the users files
    let (dir, name) = splitPath file
    newdir <- copyToNewTempDir dir
    let infile = (newdir </> name)
    -- 2) okenise the file
    tokens <- tokeniseFile mfile
    -- Split Patterns {-SPLIT-}
    postSplitTokens <- splitPatterns infile tokens
    -- Insert Type Lines {-TYPELINE-}
    postInsertTypeTokens <- insertTypeLines infile postSplitTokens
    -- 3) Print our file
    let newFile = untokeniseArr postInsertTypeTokens
    putStrLn newFile
    -- 4) Tidy up after ourselves, don't want all our crap to end up in some VCS
    removeDirectoryRecursive newdir
    return ()



