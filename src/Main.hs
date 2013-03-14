
module Main where

-- External Imports
import System.Environment( getArgs )
import System.Console.GetOpt
import Control.Applicative
import System.Exit
import System.IO

-- Project Imports
import Blackbox

{- 
	This file parses the arguments passed to the executable
	and passes control over to the blackbox module if the user
	has supplied valid arguments
	Else it will die.

	TODO: Print usage on incorerect paramaters

	Some code from http://leiffrenzel.de/papers/commandline-options-in-haskell.html
	Heavily modified though
-}


-- |The 'main' entry point to the appliction, requires some command line arguments
main = do
  args <- getArgs
  case getOpt RequireOrder options args of
    (flags, [],      [])     -> processFlags flags
    (_,     nonOpts, [])     -> error $ "Invalid arguments: " ++ unwords nonOpts
    (_,     _,       msgs)   -> error $ concat msgs ++ usageInfo header options

data Flag = Version | Infile String | Markup String deriving (Show, Eq)

options :: [OptDescr Flag]
options = [
    Option "v" ["version"] (NoArg Version)         "show version number",
    Option "f" ["file"]    (ReqArg Infile "FILE")  "file to be processed, this won't be edited",
    Option "m" ["markup"]  (ReqArg Markup "FILE")  "marked up copy of the file to be processed, this may be edited"
  ]

showVersion = do
    putStrLn "Blackbox 0.1 by Darren Mowat"
    exitSuccess

die = do 
	putStrLn header
	exitFailure
 
header = "Usage: blackbox [OPTION...]"

processFlags :: [Flag] -> IO()
processFlags f = do 
    let ver = lookupFlag "Version" f
    let inf = lookupFlag "Infile" f
    let mar = lookupFlag "Markup" f
    case lookupFlag "Version" f of
        Just v -> showVersion
        Nothing -> case (inf, mar) of 
       	               (Just (Infile ifile), Just (Markup mfile)) -> runBlackbox ifile mfile
       	               (_,_) -> die

lookupFlag :: String -> [Flag] -> Maybe Flag
lookupFlag _ [] = Nothing
lookupFlag "Version" (Version : fs)  = Just Version
lookupFlag "Infile"  (Infile f : fs) = Just (Infile f)
lookupFlag "Markup"  (Markup f : fs) = Just (Markup f)
lookupFlag x (f:fs) = lookupFlag x fs


