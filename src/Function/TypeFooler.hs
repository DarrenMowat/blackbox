module Function.TypeFooler where

-- External Imports
import Language.Haskell.Her.HaLay 
import Data.List.Split (splitOn)
import Data.List (intercalate, elem, isSuffixOf, intersperse)
import Text.ParserCombinators.Parsec (parse, manyTill, anyChar, try, string, manyTill)
import Debug.Trace (trace)
import System.IO

-- Project Imports
import GHCIProc 
import FileUtils (splitPath) 
import TokenUtils 
import ListUtils 

-- Either String String

{-|
  Find the type of a variable by inserting a tactical type error
-}
findTypeOfVarAtTok :: FilePath -> FilePath -> Tok -> ([[Tok]], [[Tok]], [[Tok]]) -> String -> IO (Maybe String)
findTypeOfVarAtTok ghci file tok (str, fn, end) vname = do 
    case extractFunctionNameFromLine (head fn) of
      Nothing -> return Nothing
      Just fnName -> do 
        let (filePath, fileName) = splitPath file
        nfn <- ensureFunctionHasType ghci fnName fn file
        let newFn = insertTacticalTypeError tok (typeFoolerCaller vname) nfn 
        case newFn == fn of 
          True  -> return (Just "Couldnt insert type fooler")
          False -> do
                let newEnd = insertTypeFooler end
                let newFile = untokeniseArr (str ++ newFn ++ newEnd)
                writeFile file newFile
                response <- runCommandList ghci file [] 
                writeFile file (tokssOut (str ++ fn ++ end))
                case lookup (LOAD fileName) response of 
                  Nothing -> return Nothing
                  Just loadErrResp -> do 
                    case parseTacticalTypeError errorStrings (readGhciError loadErrResp) of 
                      Nothing -> return Nothing
                      Just mType -> return (Just mType)

{-|
  Find the return type of a function by inserting a tactical type error
-}
findReturnTypeAtTok :: FilePath -> FilePath -> Tok -> ([[Tok]], [[Tok]], [[Tok]]) -> IO (Maybe String)
findReturnTypeAtTok ghci file tok (str, fn, end) = do 
    case extractFunctionNameFromLine (head fn) of
      Nothing -> return Nothing
      Just fnName -> do 
        let (filePath, fileName) = splitPath file
        nfn <- ensureFunctionHasType ghci fnName fn file
        let newFn = insertTacticalTypeError tok typeFoolerReturnType nfn 
        case newFn == fn of 
          True  -> return (Just "Couldnt insert type fooler")
          False -> do
                let newEnd = insertTypeFooler end
                let newFile = untokeniseArr (str ++ newFn ++ newEnd)
                writeFile file newFile
                response <- runCommandList ghci file [] 
                writeFile file (tokssOut (str ++ fn ++ end))
                case lookup (LOAD fileName) response of 
                  Nothing -> return Nothing
                  Just loadErrResp -> do 
                    case parseTacticalTypeError errorStrings (readGhciError loadErrResp) of 
                      Nothing -> return Nothing
                      Just mType -> return (Just mType)

{-|
  insertTacticalTypeError will look for the line containing {-SPLIT-} and insert
  our type error onto the end of the file, deleting the existing right hand side
  of the binding if it exists
-}
insertTacticalTypeError :: Tok -> [Tok] -> [[Tok]] -> [[Tok]]
insertTacticalTypeError tok er []     = []
insertTacticalTypeError tok er (t:ts) = case elemToken tok t of 
    False -> t : insertTacticalTypeError tok er ts
    True  -> scope t : ts
    where 
      scope [] = [] 
      scope ((L x lss) : ts)  = case elemTokenArr tok lss of 
        True  -> (L x (insertTacticalTypeError tok er lss)) : ts
        False -> (L x lss) : scope ts
      scope ((B x rs) : ts)   = case elemToken tok rs of 
        True  -> (B x rs) : insert ts 
        False -> (B x rs) : scope ts 
      scope ((T x rs) : ts)   = case elemToken tok rs of 
        True  -> (T x rs) : insert ts 
        False -> (T x rs) : scope ts 
      scope ((Sym "=") : ts)  = case elemOuter tok ts of 
        True  -> er
        False -> (Sym "=") : scope ts
      scope (t:ts)            = case t == tok of 
        True  -> t : insert ts
        False -> t : scope ts
      insert [] = []
      insert ((Sym "=") : ts) = er
      insert (t:ts) = t : insert ts
      elemOuter tok [] = False
      elemOuter tok (t:ts) = case t == tok of 
        True  -> True 
        False -> elemOuter tok ts

{-|
  Add Type Fooler will tag our BlackboxGHCITypeFooler class & plzTellMeTheTypeGHCI onto the end of the 
  class
-}
insertTypeFooler :: [[Tok]] -> [[Tok]]
insertTypeFooler tokens = tokens ++ typeFoolerDataType

typeFoolerCaller :: String -> [Tok]
typeFoolerCaller varName = head $ tokeniseString ("= plzTellMeTheTypeGHCI " ++ varName)

typeFoolerReturnType :: [Tok]
typeFoolerReturnType = head $ tokeniseString ("= BlackboxGHCITypeFooler")

-- These are Strings GHCI wraps type errors in,
-- Add more below to account for more errors, diffrent versions etc
errorStrings :: [(String, String)]
errorStrings = [ ("with actual type `", "'")
               , ("Couldn't match expected type `BlackboxGHCITypeFooler'\nwith actual type `", "'")
               , ("Couldn't match type `", "' with `BlackboxGHCITypeFooler'")
               , ("Couldn't match expected type `", "'\nwith actual type `BlackboxGHCITypeFooler'")
               , ("Couldn't match expected type `", "'with actual type `BlackboxGHCITypeFooler'")
               , ("Couldn't match expected type `", "' \nwith actual type `BlackboxGHCITypeFooler'")
               , ("Couldn't match expected type `", "' with actual type `BlackboxGHCITypeFooler'")
               , ("`","' is a rigid type variable bound by")]

{-|
  Given a List of GHCi error strings and our tactical type error attempt
  to parse the expected type of the binding. Wrapped in Maybe to denote failure
-}
parseTacticalTypeError :: [(String, String)] -> String -> Maybe String
parseTacticalTypeError [] err           = Nothing
parseTacticalTypeError ((s, e): es) err = case parse (parseBetween s e) ("parseTacticalTypeError:" ++ (s ++ "->" ++ e)) err of
     Left _ -> parseTacticalTypeError es err
     Right val -> case val == "BlackboxGHCITypeFooler" of -- Some types end up being swapped about
      True  -> parseTacticalTypeError es err
      False -> Just val

parseBetween s e = do { manyTill anyChar (try (string s))
                       ; manyTill anyChar (try (string e))
                       } 

{-|
  Make sure that the set of tokens for the function passed in contains a 
  high level type line.
  If it doesn't attempt to get one from GHCI
-}
ensureFunctionHasType :: FilePath -> String -> [[Tok]] -> FilePath -> IO [[Tok]]
ensureFunctionHasType ghci name tokens file = case head tokens of 
    [] -> return []
    xs -> case elemToken (Sym "::") xs of 
      True  -> return tokens
      False -> do 
        fntype <- getFunctionTypeFromGHCI ghci file name
        case fntype of 
          Nothing -> return tokens
          Just fntype -> do 
            let typeTokens = ready "" fntype -- Need to check if the line doesnt end with this already
            return $ typeTokens ++ ([NL ("", 0)] : tokens)

{-| 
  Query GHCI for the type of a function
-}
getFunctionTypeFromGHCI :: FilePath -> FilePath -> String -> IO (Maybe String)
getFunctionTypeFromGHCI ghci file name = do 
    let cmd = TYPEINFO name 
    resp <- runCommandList ghci file [cmd]
    case lookup cmd resp of 
        Nothing   -> return Nothing 
        Just resp -> do 
            return $ Just (readGhciOutput resp)

typeFoolerDataType :: [[Tok]]
typeFoolerDataType = tokeniseString $ unlines dt

{-|
  Datatype used for type fooling
-}
dt :: [String]
dt = [  ""
      , "{-# LANGUAGE ScopedTypeVariables #-}"
      , "data BlackboxGHCITypeFooler = BlackboxGHCITypeFooler"
      , "plzTellMeTheTypeGHCI :: BlackboxGHCITypeFooler -> a"
      , "plzTellMeTheTypeGHCI = undefined"
      , "" ]
