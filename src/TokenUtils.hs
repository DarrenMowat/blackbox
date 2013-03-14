module TokenUtils where 

import Language.Haskell.Her.HaLay
import Debug.Trace

tokeniseFile :: FilePath -> IO [[Tok]]
tokeniseFile file = do 
	contents <- readFile file
	return $ tokeniseString contents

tokeniseString :: String -> [[Tok]] 
tokeniseString = ready ".hers"

untokeniseArr :: [[Tok]] -> String 
untokeniseArr = tokssOut

errOut :: String -> Tok -> ([[Tok]], [[Tok]], [[Tok]]) -> IO [[Tok]] 
errOut s t (str, fn, end) = return (str ++ (replaceFirstTokenOcc t (mkComment s) fn) ++ end) 
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
deleteFirstTokenOcc tok (t:ts) = case elemToken tok t of 
    False -> t : deleteFirstTokenOcc tok ts
    True  -> delCom t : ts
    where 
      delCom [] = [] 
      delCom ((L l ls) : ts) = case elemTokenArr tok ls of 
        True  -> (L l (deleteFirstTokenOcc tok ls)) : delCom ts
        False -> (L l ls) : delCom ts 
      delCom ((B b rs):ts)    = case elemToken tok rs of 
        True  -> (B b (delCom rs)) : ts 
        False -> (B b rs) : delCom ts 
      delCom ((T t rs):ts)    = case elemToken tok rs of 
        True  -> (T t (delCom rs)) : ts
        False -> (T t rs) : delCom ts
      delCom (t : ts) = case t == tok of 
        True  -> trace (show t) ts
        False -> trace (show t) (t : delCom ts)

replaceFirstTokenOcc :: Tok -> Tok -> [[Tok]] -> [[Tok]]
replaceFirstTokenOcc tok rep []     = []
replaceFirstTokenOcc tok rep (t:ts) = case elemToken tok t of 
    False -> t : replaceFirstTokenOcc tok rep ts
    True  -> delCom t : ts
    where 
      delCom [] = [] 
      delCom ((L l ls) : ts) = case elemTokenArr tok ls of 
        True  -> (L l (replaceFirstTokenOcc tok rep ls)) : delCom ts
        False -> (L l ls) : delCom ts 
      delCom ((B b rs):ts)    = case elemToken tok rs of 
        True  -> (B b (delCom rs)) : ts 
        False -> (B b rs) : delCom ts 
      delCom ((T t rs):ts)    = case elemToken tok rs of 
        True  -> (T t (delCom rs)) : ts
        False -> (T t rs) : delCom ts
      delCom (t : ts) = case t == tok of 
        True  -> rep : ts
        False -> t : delCom ts


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



























