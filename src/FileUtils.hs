module FileUtils (copyDirRecursive, splitPath, copyToNewTempDir, findFile) where

import Control.Monad (forM, filterM)
import System.Directory (doesDirectoryExist, getDirectoryContents, createDirectoryIfMissing, copyFile, getTemporaryDirectory, doesFileExist)
import System.FilePath ((</>))
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Control.Monad.Random (evalRandIO, getRandomR, RandomGen, Rand)

getDirContents :: FilePath -> IO [(FilePath, String)]
getDirContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    return [(topdir, name)]
  return (concat paths)


copyDirRecursive :: FilePath -> FilePath -> IO ()
copyDirRecursive src dst = do
  createDirectoryIfMissing True dst
  files <- getDirContents src
  forM files $ \(topdir, name) -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then copyDirRecursive path (dst </> name)
      else copyFile path (dst </> name)
  return ()

copyToNewTempDir :: FilePath -> IO FilePath
copyToNewTempDir src = do 
    temp <- getTemporaryDirectory
    -- The directory returned to us above might not exist, create it
    createDirectoryIfMissing True temp
    randNum <- evalRandIO randInt
    let dest = temp </> ("blackbox-temp-" ++ show randNum)
    copyDirRecursive src dest
    return dest

-- Function to take a file path and split it into the directory path & the file name
-- splitPath "/Users/darren/project/sample/F1.hs" = ("/Users/darren/project/sample/", "F1.hs")
splitPath :: String -> (String, String)
splitPath path = (filePath, fileName)
          where parts = splitOn "/" path
                filePath = intercalate "/" (init parts)
                fileName = last parts
            
randInt :: (RandomGen g) => Rand g Int
randInt = getRandomR (1,9999999)


getNestedDirs :: FilePath -> IO [FilePath]
getNestedDirs dir = do 
  names <- getDirectoryContents dir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> return [dir </> name]
  let names = concat paths
  filterM doesDirectoryExist names

findFile :: FilePath -> String -> IO (Maybe FilePath)
findFile dir name = do 
    exists <- doesFileExist (dir </> name)
    if exists then return (Just (dir </> name)) else return Nothing







