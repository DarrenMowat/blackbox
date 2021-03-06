module ListUtils where

import Language.Haskell.Her.HaLay
import Data.Char(isSpace)

{-|
  Function to trim anything that satisfys the passed in fucntions from the start and
  end of the list. 

  Made generic from https://code.google.com/p/haskellnotebook/wiki/HaskellTrimString
-}
trim :: (a -> Bool) -> [a] -> [a]
trim x = f . f where f = reverse . dropWhile x

trimString :: String -> String
trimString = trim isSpace

trimToken :: Tok -> [Tok] -> [Tok]
trimToken t = trim (==t) 

trimSpaceToken :: [Tok] -> [Tok]
trimSpaceToken = trim isSpaceToken

isSpaceToken :: Tok -> Bool
isSpaceToken (Spc _) = True
isSpaceToken _ = False