
module Mirage (runMirage) where

import Data.List.Split
import Data.List
import System.Process    
import System.IO    
import GHCIProc
import Language.Haskell.Her.HaLay
import Text.ParserCombinators.Parsec
import FileUtils
import System.Directory
import System.FilePath ((</>))


--                  File:Line:Column
type ErrPosition = (String, Int, Int)
type Err = (ErrPosition, [String])

runMirage :: FilePath -> IO (Either String ())
runMirage file = runMirageR file 5

runMirageR :: FilePath -> Int -> IO (Either String ())
runMirageR file 0     = do 
    let (dir, name) = splitPath file 
    exists <- doesFileExist file
    case exists of 
        False -> return (mirageError name "Couldn't load file")
        True  -> do 
            response <- runCommandList file [] 
            case lookup (LOAD name) response of 
                 Nothing -> return (mirageError name "Unknown Error")
                 Just loadResp -> case readGhciError loadResp of 
                        [] -> return (Right ())
                        es -> return (mirageError name (show es))
runMirageR file count = do 
    let (dir, name) = splitPath file 
    exists <- doesFileExist file
    case exists of 
        False -> return (mirageError name "Couldn't load file")
        True  -> do 
            response <- runCommandList file [] 
            case lookup (LOAD name) response of 
                 Nothing -> return (mirageError name "Unknown Error")
                 Just loadResp -> case readGhciError loadResp of 
                        [] -> return (Right ())
                        es -> do 
                            let ces = parseErrorPosition (lines es)
                            let otherfiles = getFilesWithErrors name ces
                            case otherfiles of 
                                [] -> do 
                                    let ourerrs = errsFromFile name ces
                                    contents <- readFile file
                                    let clines = lines contents
                                    fixErrors file name count clines ourerrs
                                xs -> do 
                                    otherResults <- mapMirage (map (dir </>) otherfiles)
                                    case otherResults of 
                                        Right _ -> runMirageR file (count - 1)
                                        Left e  -> return (Left e)

mapMirage :: [FilePath] -> IO (Either String ())
mapMirage []     = return (Right ())
mapMirage (f:fs) = do 
    oMirage <- runMirage f
    case oMirage of 
        Left e  -> return (Left e)
        Right _ -> mapMirage fs

mirageError :: String -> String -> Either String ()
mirageError name error = Left ("Couldn't load " ++ name ++ " into GHCI - " ++ error)


fixErrors :: FilePath -> String -> Int -> [String] -> [Err] -> IO (Either String ())
fixErrors file name count lines [] = return (mirageError name "Not Implemented")
fixErrors file name count lines ((pos, es):ess) = do 
    let repl = parseScopeError es
    putStrLn (show repl)
    return (mirageError name "Fixing Errors")

parseScopeError :: [String] -> Maybe (String, String)
parseScopeError []     = Nothing
parseScopeError (x:[]) = Nothing
parseScopeError (x:y:s) = case parse (parseBetween "Not in scope: `" "'") "parseScopeError" x of
    Left err -> Nothing
    Right m  -> case parse (parseBetween "Perhaps you meant `" "'") "parseScopeError" y of
        Right r -> Just (m, r)
        Left err  -> Nothing 

parseErrorPosition :: [String] -> [Err]
parseErrorPosition []     = []
parseErrorPosition (s:ss) = case toErrPosition s of 
    Nothing -> parseErrorPosition ss
    Just pos -> (pos, (takeNotErrs ss)) : parseErrorPosition ss

takeNotErrs :: [String] -> [String]
takeNotErrs [] = []
takeNotErrs (x:xs) = case toErrPosition x of
    Nothing -> x : takeNotErrs xs
    Just _  -> []
 
toErrPosition :: String -> Maybe ErrPosition 
toErrPosition [] = Nothing
toErrPosition s  = if ln /= 4 then Nothing else go ts 
	where
		ts = splitOn [':'] s
		ln = length ts
		ri i = (read (ts !! i)) :: Int
		go t = Just (ts !! 0, ri 1, ri 2)

getFilesWithErrors :: String -> [Err] -> [String]
getFilesWithErrors name es = nub (go name es)
    where 
        go name [] = [] 
        go name (((file, _, _), _):xs) = case file /= name of 
            True -> file : go name xs
            False -> go name xs 

errsFromFile :: String -> [Err] -> [Err]
errsFromFile name []     = []
errsFromFile name (((file, a, b), c):es) = case file == name of 
    True  -> ((file, a, b), c) : errsFromFile name es
    False -> errsFromFile name es
 
parseBetween s e = do { manyTill anyChar (try (string s))
                       ; manyTill anyChar (try (string e))
                       } 

