module Type where 

import Language.Haskell.Her.HaLay
import ListUtils
import Data.Maybe (mapMaybe)
import Data.List.Split (splitOn)
import Data.List (intersperse)


import Debug.Trace (trace)


type Identifier = String
type Layout = String
data Paramater =  TV String -- Type Variable (a,b,cats)
                | RV String -- Rigid Type Variable (Int, Char, Cat)
                | CV String -- Complex Type Variable ((Int, String), [User])
                deriving (Show, Eq)

-- Types allow for us to 
type Type = (Identifier, Layout, [Paramater])

-- Data is a one to many mapping of a given type
-- To all the valid types within it
-- data Bool = True | False 
--      => (("Bool","Bool",[]),[("True","True",[]), ("False","False",[])])
-- data [] a = [] | a : [a] 
--      => (("[]","[] ?",[TV "a"]), [("[]","[]",[]), (":","? : ?",[TV "a", CV "[a]"])])
-- data Either a b = Left a | Right b 
--      => (("Either", "Either ? ?", [TV "a", TV "b"]), [("Left", "Left ?", [TV "a"]), ("Right", "Right ?", [TV "b"])])
type Data = (Type, [Type])

{-|
  Sometimes when GHCI returns a type it will be in the format
    [Char] | [Int] | (Int, String) | Tree Int
  However we can't process types like this. This fucntion unwraps 
  the type into a more manageable form. For example 
    ("[]", "[?]",[RV "Char"]) |  ... | ("Tree", "Tree ?", [RV "Int"])
-}
unwrapType :: String -> Maybe Type
unwrapType [] = Nothing
unwrapType str = unwrapTypeToks $ concat $ ready "" str

unwrapTypeToks :: [Tok] -> Maybe Type
unwrapTypeToks [] = Nothing
unwrapTypeToks toks = {- trace ("unwrapTypeToks: " ++ show toks) -} (go $ trimSpaceToken $ seekToDef toks)
    where 
        go [] = Nothing
        go (Uid i : ts) = Just (i, i ++ concat (replicate (length ps) " ?"), ps)
            where ps = pParams ts
        -- Lists are hard to parse, we'll cheat a wee bit
        go (B Sqr [] : tts) = Just ("[]", "[]", [])
        go (B Sqr ts : tts) = Just ("[]", "[?]", (pParams ts ++ pParams tts))
        -- Mostly for parsing tuples
        go (B Rnd ts : tts) = trace (show ts) (Just ((filter (/='?') us), us, (pParams ts ++ pParams tts)))
            where us = '(' : pTuple ts ++ ")"
        go _ = Nothing
        pParams [] = [] 
        pParams (Spc _ : ts) = pParams ts 
        pParams (Sym _ : ts) = pParams ts 
        pParams (Uid i : ts) = RV i : pParams ts 
        pParams (Lid i : ts) = TV i : pParams ts 
        pParams (s : ts) = CV (tokOut s) : pParams ts 
        pTuple [] = [] 
        pTuple (Spc s : ts) = pTuple ts 
        pTuple (Sym s : ts) = s ++ pTuple ts 
        pTuple (Uid i : ts) = "?" ++ pTuple ts 
        pTuple (Lid i : ts) = "?" ++ pTuple ts 
        pTuple (s : ts) = "?" ++ pTuple ts
        seekToDef [] = []
        seekToDef (KW _ : ts) = seekToDef ts
        seekToDef (T Ty ts : _) = ts 
        seekToDef (Spc _ : ts) = seekToDef ts
        seekToDef ts = ts

{-|
  Ensure data is snaitized before running this!
-}
toData :: [Tok] -> Maybe Data
toData [] = Nothing
toData ts = case left ts of 
        Nothing -> Nothing
        Just tl -> Just (tl, right ts)
    where 
        split ts    = (left ts, right ts)
        left ts     = unwrapTypeToks $ trimSpaceToken $ head $ splitEq ts
        right ts    = mapMaybe unwrapTypeToks $ map trimSpaceToken $ splitSym $ concat $ tail $ splitEq ts
        splitEq ts  = splitOn [Sym "="] ts
        splitSym ts = splitOn [Sym "|"] ts

lookupType :: Type -> Maybe Data
lookupType t = {-trace (show dataLib)-} (lookupType' t dataLib)

lookupType' :: Type -> [Data] -> Maybe Data
lookupType' _ [] = Nothing
lookupType' i (t:ts) = if getId i == getIdFromType t then Just t else lookupType' i ts 
    where 
        getId (i, _, _) = i 
        getType (t, _) = t
        getIdFromType d = getId (getType d)

dataLib :: [Data]
dataLib = mapMaybe tType [
    "data Bool = False | True",
    "data Either a b = Left a | Right b",
    "data Maybe a = Nothing | Just a",
    "type String = [Char]"]
    where tType s = toData $ head $ ready "" s 


