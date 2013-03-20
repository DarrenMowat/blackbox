module TokenUtils where 

import Language.Haskell.Her.HaLay
import Debug.Trace

tokeniseFile :: FilePath -> IO [[Tok]]
tokeniseFile file = do 
  contents <- readFile file
  let processed = unlines $ map appendSpc $ lines contents ++ ["blackboxEOFKeeper = 0"]
  return $ ready ".hers" processed

tokeniseString :: String -> [[Tok]] 
tokeniseString = ready ".hers"

appendSpc :: String -> String 
appendSpc s = s ++ " "

unappendSpc :: String -> String 
unappendSpc [] = [] 
unappendSpc (' ' : []) = []
unappendSpc (s:ss) = s : unappendSpc ss

untokeniseFile :: [[Tok]] -> String 
untokeniseFile toks = unlines $ init $ map unappendSpc (lines (tokssOut toks))

untokeniseArr :: [[Tok]] -> String 
untokeniseArr = tokssOut

untokeniseLine :: [Tok] -> String
untokeniseLine = toksOut

errOut :: String -> Tok -> ([[Tok]], [[Tok]], [[Tok]]) -> IO [[Tok]] 
errOut s t (str, fn, end) = return (str ++ replaceFirstTokenOcc t (mkComment s) fn ++ end)
    where 
      mkComment s = Com $ "{- " ++ s ++ " -}"

{-|
  The 'extractFunctionNameFromLine' a line of tokenised haskell and
  attempts to return the function name declared in the line.
  Wrapped in Maybe to handle failures
-}
extractFunctionNameFromLine :: [Tok] -> Maybe String
extractFunctionNameFromLine [] = Nothing
extractFunctionNameFromLine (NL _ : xs) = extractFunctionNameFromLine xs
extractFunctionNameFromLine (Spc _ : xs) = extractFunctionNameFromLine xs
extractFunctionNameFromLine (Lid name : xs) = Just name
extractFunctionNameFromLine (x:xs) = Nothing 

{-|
  The 'extractFunction' takes the name of a fucntion present in the file
  and a tokenised haskell file and splits it into 3 parts:
   * Everythign before the function
   * The function, including trailing whitespace
   * Everything after the function, minus leading whitespace
  This function could probably be improved to save parsing the stream several times
-}
extractFunction :: String -> [[Tok]] -> ([[Tok]], [[Tok]], [[Tok]])
extractFunction name tokens = (bf tokens, fn tokens, af tokens)
    where 
        bf = takeWhile bft
        bft [] = True -- We take empty lists
        bft x  = head x /= n 
        fn xs  = takeWhile fnt (dropWhile bft xs)
        fnt [] = True -- An empty tok list could be in a fucntion, right? 
        fnt x  = case head x of 
        	(Lid s) -> Lid s == n
        	(NL x)  -> True
        	(Spc x) -> True
        	_       -> False
        af xs  = dropWhile fnt (dropWhile bft xs)
        n = Lid name 	

deleteFirstTokenOcc :: Tok -> [[Tok]] -> [[Tok]]
deleteFirstTokenOcc tok []     = []
deleteFirstTokenOcc tok (t:ts) = if elemToken tok t then delCom t : ts else
    t : deleteFirstTokenOcc tok ts
    where 
      delCom [] = [] 
      delCom (L l ls : ts) = if elemTokenArr tok ls then
        L l (deleteFirstTokenOcc tok ls) : delCom ts else
        L l ls : delCom ts
      delCom (B b rs : ts)    = if elemToken tok rs then 
        B b (delCom rs) : ts else
        B b rs : delCom ts 
      delCom (T t rs : ts)    = if elemToken tok rs then 
        T t (delCom rs) : ts else
        T t rs : delCom ts
      delCom (t : ts) = if t == tok then ts else t : delCom ts

replaceFirstTokenOcc :: Tok -> Tok -> [[Tok]] -> [[Tok]]
replaceFirstTokenOcc tok rep []     = []
replaceFirstTokenOcc tok rep (t:ts) = if elemToken tok t then delCom t : ts else
    t : replaceFirstTokenOcc tok rep ts
    where 
      delCom [] = [] 
      delCom (L l ls : ts) = if elemTokenArr tok ls then
        L l (replaceFirstTokenOcc tok rep ls) : delCom ts else
        L l ls : delCom ts
      delCom (B b rs : ts)    = if elemToken tok rs then 
        B b (delCom rs) : ts else
        B b rs : delCom ts 
      delCom (T t rs : ts)    = if elemToken tok rs then 
        T t (delCom rs) : ts else
        T t rs : delCom ts
      delCom (t : ts) = if t == tok then rep : ts else t : delCom ts


{-|
  Collect all lines which contain a given token
-}
findLinesWithToken :: Tok -> [[Tok]] -> [[Tok]]
findLinesWithToken t []       = []
findLinesWithToken t (x : xs) = if elemToken t x then x : findLinesWithToken t xs else
    findLinesWithToken t xs

{-|
  
-}
elemTokenArr :: Tok -> [[Tok]] -> Bool
elemTokenArr tok []     = False
elemTokenArr tok (x:xs) = elemToken tok x || elemTokenArr tok xs  

peek :: [Tok] -> [Tok]
peek (T Ty ts : _) = ts
peek (_: ts) = peek ts
peek [] = []

{-|
  
-}
elemToken :: Tok -> [Tok] -> Bool
elemToken _ []                  = False
elemToken tok (B _ rts : ts)    = elemToken tok rts || elemToken tok ts -- Some elements have inner lists of tokens, check them
elemToken tok (T _ rts : ts)    = elemToken tok rts || elemToken tok ts 
elemToken tok (L s ls : ts)     = elemTokenArr tok ls || elemToken tok ts 
elemToken tok (t:ts)            = tok==t || elemToken tok ts

