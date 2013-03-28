module Main where 

import Types.Cards
import Types.Lists
import Types.Trees
import Types.User

main :: IO ()
main = putStrLn "ScopeTests compiled"

{-  
    Test Purpose: Very basic test of top level scope

    Expected Scope [ ss :: String ]
    Expected Return Type [ Return Type :: String ]
-}
test0 :: String -> String
test0 ss = {-SCOPE-}undefined

{-  
    Test Purpose: Testing fucntion with several lines

    Expected Scope [ s :: String, i :: Int, s2 :: String ]
    Expected Return Type [ Return Type :: String ]
-}
test1 :: User -> String
test1 (Student s i s2) = {-SCOPE-} ""
test1 (Admin s i) = ""

{- 
    Test Purpose: Basic test of top level scope with types inside tuples

    Expected Scope [ ss :: String, i :: Int, i2 :: Int, sss :: [String] ]
    Expected Return Type [ Return Type :: Bool ]
-}
test2 :: String -> Int -> (Int, [String]) -> Bool
test2 ss i (i2, sss) = {-SCOPE-}undefined

{-
    Test Purpose: Basic test of top level scope with several diffrent types

    Expected Scope [ ss :: String, i :: Int, ts :: (Int, [String]) ]
    Expected Return Type [ Return Type :: Bool ]
-}
test3 :: String -> Int -> (Int, [String]) -> Bool
test3 ss i ts = {-SCOPE-}undefined

{-
    Test Purpose: Testing return type with a curried function

    Expected Scope [ ss :: String, i :: Int ]
    Expected Return Type [ Return Type :: (Int, [String]) -> Bool ]
-}
test4 :: String -> Int -> (Int, [String]) -> Bool
test4 ss i  = {-SCOPE-}undefined

{-
    Test Purpose: Testing return type with a curried function, with SCOPE after the definition 

    Expected Scope [ ss :: String, i :: Int ]
    Expected Return Type [ Return Type :: (Int, [String]) -> Bool ]
-}
test5 :: String -> Int -> (Int, [String]) -> Bool
test5 ss i  = undefined {-SCOPE-}

{- 
    NOTE: Testing on inline functions may return more generic types than expected ([a] instead of [String] etc...)
-}

{-
    Test Purpose: Testing simple in line function
    Expected Scope [ s :: String, is :: [Int], s1 :: Int, ss :: [Int]]
    Expected Return Type [ Return Type :: Int ]
-}
test6 :: String -> [Int] -> Bool
test6 s is = t6inner is == length s 
    where 
      t6inner [] = 0
      t6inner (s1:ss) = {-SCOPE-}1 + t6inner ss

{-
    Test Purpose: Testing inline function where the inline fn variable 's'
                  shadows the toplevel variable s
    Expected Scope [ is :: [Int], s :: Int, ss :: [Int]]
    Expected Return Type [ Return Type :: Int ]
-}
test7 :: String -> [Int] -> Bool
test7 s is = t7inner is == length s 
    where 
      t7inner [] = 0
      t7inner (s:ss) = {-SCOPE-}1 + t7inner ss

{-
    Test Purpose: Testing inline function where there is more than one inline 
                  fucntion
    Expected Scope [ s :: String, is :: [Int], s1 :: Int, ss :: [Int]]
    Expected Return Type [ Return Type :: Int ]
-}
test8 :: String -> [Int] -> (Bool, Bool)
test8 s is = (t8inner1 is == length s, t8inner2 is == length s)
    where 
      t8inner1 [] = 0
      t8inner1 (s1:ss) = {-SCOPE-}1 + t8inner1 ss
      t8inner2 xs = length xs

{-
    Test Purpose: Testing inline function where there is more than one inline 
                  fucntion
    Expected Scope [ s :: String, is :: [Int], xs :: [Int]]
    Expected Return Type [ Return Type :: Int ]
-}
test9 :: String -> [Int] -> (Bool, Bool)
test9 s is = (t9inner1 is == length s, t9inner2 is == length s)
    where 
      t9inner1 [] = 0
      t9inner1 (s1:ss) = 1 + t9inner1 ss
      t9inner2 xs = {-SCOPE-}length xs

{-
   Sometimes compiler warnings are sent over STDERR
   We don't care so this tests that they are ignored when
   testing if a file loaded
-}
testCompilerWarningIgnoring a = a
testCompilerWarningIgnoring [] = []