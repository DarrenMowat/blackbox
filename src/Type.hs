module Type  where 


import Language.Haskell.Her.HaLay
import ListUtils
import Data.Maybe (mapMaybe, isJust)
import Data.List.Split (splitOn)
import Text.ParserCombinators.Parsec (parse, manyTill, anyChar, try, string, manyTill)
import Data.List (intersperse, isPrefixOf)
import TokenUtils
import Data.Char

type Identifier = String
type Layout = String
type Parameter = (String, Maybe String)

type Type = (Identifier, Layout, [Parameter])

type Data = (Type, [Type])

unwrapType :: String -> Maybe Type
unwrapType [] = Nothing
unwrapType str = unwrapTypeToks $ concat $ ready "" str

unwrapTypeToks :: [Tok] -> Maybe Type
unwrapTypeToks ts = if isInfixCons stripped then toTypeInfix stripped else toType stripped
    where 
    	stripped = trimSpaceToken ts
        isInfixCons []           = False
        isInfixCons (Sym t : ts) = True
        isInfixCons (t:ts)       = isInfixCons ts

{-|
   Convert a token stream to a Type
   This function can fail
-}
toType :: [Tok] -> Maybe Type 
toType [] = Nothing
toType ts = Just (iden toks, concat (layout toks), (maybeZipNames (params toks) (mapParams (toksOut toks))))
    where 
        -- Zip default names into the type, if they types and names lists have the same length
        maybeZipNames ps ns = if length ps == length ns then zipParamNames ps ns else ps
        zipParamNames [] [] = []
        zipParamNames ((name, _): ps) (n:ns) = (name, (Just n)) : zipParamNames ps ns
        -- Attempt to extract default names for variabls
        mapParams [] = [] 
        mapParams ('{':'-':xs) = (toEndCom xs) : mapParams xs
        mapParams (t:ts) = mapParams ts
        toEndCom [] = [] 
        toEndCom ('-':'}':_) = [] 
        toEndCom (t:ts) = t : toEndCom ts 
    	toks = trimSpaceToken ts
        -- Grab the types identifier, its an Uppercase string at the start of a data definition
        iden (Uid idn : ts) = idn
        iden ts = filter (not . isSpace) $ concat $ filter (/="{?}") $ layout (trimSpaceToken ts)
        -- Get the layout 
        -- Some types start with an Uppercase constructor name so we pull that first
        layout []     = []
        layout (Uid cName : ts) = cName : toLayout ts
        layout ts = toLayout ts
        -- Then process the rest of the laout by replacing the Upper & Lowecase
        -- identifiers with {?} 
    	toLayout [] = []
    	toLayout (B Sqr ts : tss) = ["["] ++ toLayout ts ++ ["]"] ++ toLayout tss
    	toLayout (B Rnd ts : tss) = ["("] ++ toLayout ts ++ [")"] ++ toLayout tss
    	toLayout (T Ty ts : tss)   = toLayout ts ++ toLayout tss
    	toLayout (Lid a : ts) = "{?}" : toLayout ts
    	toLayout (Uid a : ts) = "{?}" : toLayout (takeLid ts) -- Sometimes UId's are followed by type arguments 
    	toLayout (Com _ : ts) = toLayout ts
    	toLayout (NL _ : ts) = toLayout ts
        toLayout (KW "deriving" : _) = []
        toLayout (KW "instance" : _) = []
        toLayout (t:ts) = tokOut t : toLayout ts
        -- Strip Lowercase identifiers & spaces
        takeLid [] = []
        takeLid (Lid _ : ts) = takeLid ts
        takeLid (Spc _ : ts) = takeLid ts
        takeLid ts = ts
        -- Extract paramaters from a type string
        params (Uid _ : ts) = toParams ts
        params ts = toParams ts
    	toParams [] = []
        toParams (KW "deriving" : _) = []
        toParams (KW "instance" : _) = []
    	toParams (B _ ts : tss) = toParams ts ++ toParams tss
        toParams (Lid a : ts) = (a, Nothing) : toParams ts
        toParams (Com n : Lid a : ts) = (a, Just n) : toParams ts
        toParams (Uid a : ts) = (a, Nothing) : toParams ts
        toParams (Com n : Uid a : ts) = (a, Just n) : toParams ts
    	toParams (T Ty ts : tss)   = toParams ts ++ toParams tss
        toParams (Com _ : ts) = toParams ts
        toParams (t:ts) = toParams ts


{-|
   Convert a token stream with an inline constructor to a Type
   This function can fail
-}
toTypeInfix :: [Tok] -> Maybe Type 
toTypeInfix [] = Nothing
toTypeInfix ts = case getInfixCons ts of 
        	Nothing -> Nothing 
        	Just i  -> Just (i, "{?} " ++ i ++ " {?}", params i (trimSpaceToken ts))
    where 
        getInfixCons []           = Nothing
        getInfixCons (Sym t : ts) = Just t
        getInfixCons (t:ts)       = getInfixCons ts     
        params i ts = map toParam $ splitOn [Sym i] ts
        toParam t = (trimString (toksOut t), Nothing)


{-|
  Convert a constructor, either from ghci or internal, to Data 
-}
toData :: [Tok] -> Maybe Data
toData [] = Nothing
toData ts = case left ts of 
        Nothing -> Nothing
        Just (iden, layout, ps) -> Just ((iden, layout, ps), right ts)
    where 
        left ts          = unwrapTypeToks $ trimSpaceToken $ seekToDef $ head $ splitEq ts
        right ts         = mapMaybe (unwrapTypeToks . trimSpaceToken) (splitSym $ concat $ tail $ splitEq ts)
        splitEq          = splitOn [Sym "="] 
        splitSym         = splitOn [Sym "|"] 
        seekToDef (KW _ : ts) = seekToDef ts
        seekToDef (T Ty ts : _) = ts 
        seekToDef (Spc _ : ts) = seekToDef ts
        seekToDef ts = ts 

{-|
  Process a constructor received from GHCI
-}
toDataFromGhci :: String -> Maybe Data
toDataFromGhci str = toData $ concat $ ready "" (clean str) 
   where 
     clean ss = concat $ map dropDashDash $ filter (not . (isPrefixOf "instance")) $ lines ss 
     dropDashDash [] = [] 
     dropDashDash ('-' : '-' : _) = [] 
     dropDashDash (t:ts) = t : dropDashDash ts

toDataFromTokens :: [[Tok]] -> Maybe Data
toDataFromTokens toks = undefined

lookupType :: Type -> Maybe Data
lookupType t = lookupType' t dataLib

lookupType' :: Type -> [Data] -> Maybe Data
lookupType' _ [] = Nothing
lookupType' i (t:ts) = if getId i == getIdFromType t then Just t else lookupType' i ts 
    where 
        getId (i, _, _) = i 
        getType (t, _) = t
        getIdFromType d = getId (getType d)

{-|
    Standard library of common data types
-}
dataLib :: [Data]
dataLib = mapMaybe toDataFromGhci [
    "data [] a = [] | {-x-}a : {-xs-}[a]",
    "data Bool = False | True",
    "data Either a b = Left {-l-}a | Right {-r-}b",
    "data Maybe a = Nothing | Just {-x-}a"
    ]

