
module GHCIProc (GHCIResponse (..), GHCICommand (..), HLine (..), runCommandList, readGhciOutput, readGhciError) where

import Data.List (intercalate, nub)
import System.Process (readProcess)
import Text.ParserCombinators.Parsec (parse, manyTill, anyChar, try, string, manyTill)
import System.IO

import Paths_blackbox (getDataFileName)
import FileUtils (splitPath)

-- Types

type GHCIResponse = (GHCICommand, [HLine])

data HLine = Out String | Err String deriving (Show, Eq)

data GHCICommand = PROMPT String | CD String | LOAD String | QUIT |
                   RELOAD | ECHO String | TYPEINFO String | INFO String | 
                   LET String String | RUN String | BROWSE String deriving (Eq)

-- Derive a show instance for GHCICommand
-- This will convert our mappings to a 
-- command whcih ghci can consume

instance Show GHCICommand where
	show (PROMPT s)   = unwords [":set prompt ", "\"", s, "\""] 
	show (CD dir)     = unwords [":cd", dir] -- TODO: Spaces in path names?
	show (LOAD f)     = unwords [":l", f]
	show QUIT         = ":q"
	show RELOAD       = ":r"
	show (ECHO s)     = concat [":! echo ", "\"", s, "\""] -- TODO: Windows Compliant as this will fork out a terminal command
	show (TYPEINFO t) = unwords [":t", t]
	show (INFO x)     = unwords [":i", x]
	show (LET x b)    = unwords [x, "=", b]
	show (RUN x)      = x 
	show (BROWSE m)   = unwords [":browse!", m] 

-- Function to wrap standard load and quit commands around the command list
wrapStandardCommands :: String -> [GHCICommand] -> [GHCICommand]
wrapStandardCommands file cmds = [PROMPT "", CD filePath, LOAD fileName] ++ cmds ++ [QUIT]
                                 where (filePath, fileName) = splitPath file

wrapCommandList :: [(Int, GHCICommand)] -> [GHCICommand]
wrapCommandList []                 = []
wrapCommandList ((id, cmd):cmds) = startCommand cmd : cmd : endCommand cmd : wrapCommandList cmds 
    where
      startCommand c = ECHO (intercalate "" ["{-S", show id, "-}"])
      endCommand c = ECHO (intercalate "" ["{-E", show id, "-}"])

buildAL :: Int -> [GHCICommand] -> [(Int, GHCICommand)]
buildAL _ []     = []
buildAL n (c:cs) = (n, c) : buildAL (n + 1) cs

runCommandList :: FilePath -> [GHCICommand] -> IO [GHCIResponse] 
runCommandList f cs = do
	let al = buildAL 1 $ wrapStandardCommands f (nub cs)
	let cmds = wrapCommandList al
	let ins = intercalate "\n" (map show cmds)
	bash <- getDataFileName "annotate.sh"
	out <- readProcess bash ["ghci"] ins
	let resp = splitResponses al out 
	-- hPutStrLn stderr (show resp)
	return resp

splitResponses :: [(Int, GHCICommand)] -> String -> [GHCIResponse] 
splitResponses [] _ = [] 
splitResponses (_:[]) _ = [] -- This is always a quit command, we don't really care about it. Also causes parse errors
splitResponses ((id, cmd) : cmds) str = case getCommandOutput str id of 
    Nothing    -> {- I think I'll just ignore this command id -} splitResponses cmds str
    Just lines -> (cmd, lines) : splitResponses cmds str

-- Dedicated Parsers
-- Kit for grabbing the response for a particular command

-- We return the raw string output from GHCI & some lines
getCommandOutput :: String -> Int -> Maybe [HLine]
getCommandOutput str id = case searchResponse str id of 
    Nothing  -> Nothing
    Just str -> Just (filterEmptyHLines (map toLine (lines str)))

filterEmptyHLines :: [HLine] -> [HLine]
filterEmptyHLines []     = [] 
filterEmptyHLines (x:xs) = if isEmpty x then filterEmptyHLines xs else
    x : filterEmptyHLines xs

isEmpty :: HLine -> Bool
isEmpty (Out "") = True
isEmpty (Err "") = True
isEmpty e = False

toLine :: String -> HLine
toLine line
    | lineStartsWith line "O:" = Out (drop 3 line)
    | lineStartsWith line "E:" = Err (drop 3 line)
    | otherwise = Err line

searchResponse :: String -> Int -> Maybe String
searchResponse str id = case parse (returnResponse id) ("searchResponse:" ++ show id) str of
     Left err -> Nothing
     Right val -> Just val

returnResponse i  = parseBetween ("{-S" ++ show i ++ "-}") ("{-E" ++ show i ++ "-}")

parseBetween s e = do { manyTill anyChar (try (string s))
                       ; manyTill anyChar (try (string e))
                       } 

lineStartsWith :: String -> String -> Bool
lineStartsWith str a = case parse (string a) ("lineStartsWith:" ++ show a) str of
     Left err -> False
     Right val -> True

{-|
  readGhciOutput takes a list of HLine returned from GHCI & collates all
  the lines which which were transmitted over STDOUT
-}
readGhciOutput :: [HLine] -> String 
readGhciOutput [] = [] 
readGhciOutput (Out s : ss) = s ++ "\n" ++ readGhciOutput ss  
readGhciOutput (_ : ss) = readGhciOutput ss  

{-|
  readGhciError takes a list of HLine returned from GHCI & collates all
  the lines which which were transmitted over STDERR
-}
readGhciError :: [HLine] -> String 
readGhciError [] = [] 
readGhciError (Err s : ss) = s ++ "\n" ++ readGhciError ss  
readGhciError (_ : ss) = readGhciError ss  


