module Main where 

import Types.Cards
import Types.Lists
import Types.Trees
import Types.User

main :: IO ()
main = putStrLn "SplitTests compiled"

{- 
    ? marks are used to show where a variable will be
    in the expected patterns
-}

{-
    Testing Maybe - From internal type db

    Expected [ Nothing & Just ? ]
-}
test0 :: Maybe Int -> Int
test0 {-SPLIT-}xs = 0

{-
    Testing List - From internal type db

    Expected [ [] & (x:xs) ]
-}
test1 :: [Int] -> Int
test1 {-SPLIT-}xs = 0

{-
    Testing Either - From internal type db

    Expected [ Left ? & Right ? ]
-}
test2 :: Either Int Int -> Int
test2 {-SPLIT-}xs = 0

{-
    Testing Bool - From internal type db

    Expected [ True & False ]
-}
test3 :: Bool -> Int
test3 {-SPLIT-}xs = 0



{-
    Test using a datatype declared in this module
    Expected [ (Square ?) & (Rectangle ? ?) & (Circle ?) ]
-}

data Shape = Square Int 
           | Rectangle Int Int
           | Circle Int 
           deriving (Eq, Show)

test4 :: Shape -> Int 
test4 {-SPLIT-}s = 0


{-
    Test using a datatype declared in another file
    Expected [ (Student ? ? ?) | (Admin ? ?) ]
-}

test5 :: User -> String 
test5 {-SPLIT-}us = ""

{-
    Test multiple splits using crazy huge constructors (Pack of cards)
    Expected [A constructor for every card in the pack, huge list]
-}

test6 :: Card -> Int 
test6 ({-SPLIT-}suit, {-SPLIT-}val) = 0


{-
    Test splitting a type which has an inline constructor
    Expected [B0 & (? :< ?)]
-}
test7 :: Bwd Int -> Int 
test7 {-SPLIT-}bs = 0


{-
    Test splitting 2 lists
    Expected [[] [] & (?:?) [] & [] (?:?) & (?:?) (?:?)]
-}
test8 :: String -> String -> String 
test8 {-SPLIT-}xs {-SPLIT-}ys = ""


{-  
    Test splitting a pattern on an inline function
    Expected [ [] & (? : ?)]
-}
test9 :: String -> String 
test9 ss = foo ss
    where 
      foo {-SPLIT-}xs = ""

{-  
    Test splitting a pattern on an inline function with surrounding functions
    Expected [ [] & (? : ?)]
-}
test10 :: String -> String 
test10 ss = foo ss ++ (show $ bar ss)
    where 
        bar xs = length xs
        foo {-SPLIT-}xs = ""

{-  
    Test splitting a pattern on an inline function with surrounding functions
    Expected [ [] & (? : ?)]
-}
test11 :: String -> String 
test11 ss = foo ss ++ (show $ bar ss)
    where 
        foo {-SPLIT-}xs = ""
        bar xs = length xs


{-  
    Test splitting a pattern on an inline function with surrounding functions
    Expected [ [] & (? : ?)]
-}
test12 :: String -> String 
test12 ss = foo ss ++ (show $ bar ss)
    where 
        fuz xs = xs ++ xs
        foo {-SPLIT-}xs = ""
        bar xs = length xs

{-  
    Test splitting 2 patterns in an inline function block
    Expected [ [] & (? : ?)] & [ [] & (? : ?)]
-}
test13 :: String -> String 
test13 ss = foo ss ++ (show $ bar $ fuz ss)
    where 
        fuz xs = xs ++ xs
        foo {-SPLIT-}xs = ""
        bar {-SPLIT-}xs = 0

{-  
    Test a complex type in an inline function
    Expected [ (Student ? ? ?) | (Admin ? ?) ]
-}
test14 :: User -> String 
test14 user = foo user
    where 
        foo user = undefined

{-  
    Test splitting a primitive 
    Expected [ An error message explaining that primitives can't be split will be inserted ]
-}
test15 :: Char -> String 
test15 {-SPLIT-}c = ""

{-  
    Test splitting a tree defined in Types.Trees 
    Expected [ (Branch ? ?) & (Leaf ?) ]
-}
test16 :: Tree Int -> Int 
test16 {-SPLIT-}t = 0

