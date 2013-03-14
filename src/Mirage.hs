
module Mirage where

import Control.Applicative
import Data.List.Split
import Data.List
import System.Process    
import System.IO    
import GHCIProc
import Language.Haskell.Her.HaLay

data HError = HError FilePath  deriving (Eq, Show)

findErrors :: FilePath -> Maybe [HError]
findErrors file = undefined

{- Modifictation Kit -}

-- Function to map a chucker over some tokens
chuck :: ([Tok] -> [Tok]) -> [[Tok]] -> [[Tok]] 
chuck f tokens = map f tokens

chuckComments :: [[Tok]] -> [[Tok]] 
chuckComments = chuck chuckCommentLine

chuckCommentLine :: [Tok] -> [Tok] 
chuckCommentLine []           = []
chuckCommentLine ((Com _):xs) = chuckCommentLine xs
chuckCommentLine (x:xs)       = x : chuckCommentLine xs

chuckKind :: [[Tok]] -> [[Tok]] 
chuckKind = chuck chuckKindLine

chuckKindLine :: [Tok] -> [Tok] 
chuckKindLine []     = []
chuckKindLine ((Sym "="):xs) = case (findNewLineSymbol xs) of
	Nothing -> {- TODO: Oh shit, lets make up a new line terminator. This will probably break stuff! -} lineTerminator ++ [(NL ("", 0))]
	Just nl -> lineTerminator ++ [nl]
chuckKindLine (x:xs) = x : chuckKindLine xs
    
lineTerminator :: [Tok]
lineTerminator = [Sym "=", Spc " ", Lid "undefined"]

findNewLineSymbol :: [Tok] -> Maybe Tok
findNewLineSymbol []                 = Nothing -- Hopefully we'll never reach this case
findNewLineSymbol ((NL (s, i)) : xs) = Just (NL (s, i))
findNewLineSymbol (_ : xs)           = findNewLineSymbol xs

test :: FilePath -> Maybe [HError]
test file = findErrors file
