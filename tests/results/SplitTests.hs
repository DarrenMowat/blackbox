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
test0 Nothing = 0
test0 (Just x) = 0

{-
    Testing List - From internal type db

    Expected [ [] & (x:xs) ]
-}
test1 :: [Int] -> Int
test1 [] = 0
test1 (xs : xs1) = 0

{-
    Testing Either - From internal type db

    Expected [ Left ? & Right ? ]
-}
test2 :: Either Int Int -> Int
test2 (Left l) = 0
test2 (Right r) = 0

{-
    Testing Bool - From internal type db

    Expected [ True & False ]
-}
test3 :: Bool -> Int
test3 False = 0
test3 True = 0



{-
    Test using a datatype declared in this module
    Expected [ (Square ?) & (Rectangle ? ?) & (Circle ?) ]
-}

data Shape = Square Int 
           | Rectangle Int Int
           | Circle Int 
           deriving (Eq, Show)

test4 :: Shape -> Int 
test4 (Square s) = 0
test4 (Rectangle s s1) = 0
test4 (Circle s) = 0


{-
    Test using a datatype declared in another file
    Expected [ (Student ? ? ?) | (Admin ? ?) ]
-}

test5 :: User -> String 
test5 (Student us us1 us2) = ""
test5 (Admin us us1) = ""

{-
    Test multiple splits using crazy huge constructors (Pack of cards)
    Expected [A constructor for every card in the pack, huge list]
-}

test6 :: Card -> Int 
test6 (Club, Two) = 0
test6 (Club, Three) = 0
test6 (Club, Four) = 0
test6 (Club, Five) = 0
test6 (Club, Six) = 0
test6 (Club, Seven) = 0
test6 (Club, Eight) = 0
test6 (Club, Nine) = 0
test6 (Club, Ten) = 0
test6 (Club, Jack) = 0
test6 (Club, Queen) = 0
test6 (Club, King) = 0
test6 (Club, Ace) = 0
test6 (Diamond, Two) = 0
test6 (Diamond, Three) = 0
test6 (Diamond, Four) = 0
test6 (Diamond, Five) = 0
test6 (Diamond, Six) = 0
test6 (Diamond, Seven) = 0
test6 (Diamond, Eight) = 0
test6 (Diamond, Nine) = 0
test6 (Diamond, Ten) = 0
test6 (Diamond, Jack) = 0
test6 (Diamond, Queen) = 0
test6 (Diamond, King) = 0
test6 (Diamond, Ace) = 0
test6 (Heart, Two) = 0
test6 (Heart, Three) = 0
test6 (Heart, Four) = 0
test6 (Heart, Five) = 0
test6 (Heart, Six) = 0
test6 (Heart, Seven) = 0
test6 (Heart, Eight) = 0
test6 (Heart, Nine) = 0
test6 (Heart, Ten) = 0
test6 (Heart, Jack) = 0
test6 (Heart, Queen) = 0
test6 (Heart, King) = 0
test6 (Heart, Ace) = 0
test6 (Spade, Two) = 0
test6 (Spade, Three) = 0
test6 (Spade, Four) = 0
test6 (Spade, Five) = 0
test6 (Spade, Six) = 0
test6 (Spade, Seven) = 0
test6 (Spade, Eight) = 0
test6 (Spade, Nine) = 0
test6 (Spade, Ten) = 0
test6 (Spade, Jack) = 0
test6 (Spade, Queen) = 0
test6 (Spade, King) = 0
test6 (Spade, Ace) = 0


{-
    Test splitting a type which has an inline constructor
    Expected [B0 & (? :< ?)]
-}
test7 :: Bwd Int -> Int 
test7 B0 = 0
test7 (bs :< bs1) = 0


{-
    Test splitting 2 lists
    Expected [[] [] & (?:?) [] & [] (?:?) & (?:?) (?:?)]
-}
test8 :: String -> String -> String 
test8 [] [] = ""
test8 [] (ys : ys1) = ""
test8 (xs : xs1) [] = ""
test8 (xs : xs1) (ys : ys1) = ""


{-  
    Test splitting a pattern on an inline function
    Expected [ [] & (? : ?)]
-}
test9 :: String -> String 
test9 ss = foo ss
    where 
      foo [] = ""
      foo (xs : xs1) = ""

{-  
    Test splitting a pattern on an inline function with surrounding functions
    Expected [ [] & (? : ?)]
-}
test10 :: String -> String 
test10 ss = foo ss ++ (show $ bar ss)
    where 
        bar xs = length xs
        foo [] = ""
        foo (xs : xs1) = ""

{-  
    Test splitting a pattern on an inline function with surrounding functions
    Expected [ [] & (? : ?)]
-}
test11 :: String -> String 
test11 ss = foo ss ++ (show $ bar ss)
    where 
        foo [] = ""
        foo (xs : xs1) = ""
        bar xs = length xs


{-  
    Test splitting a pattern on an inline function with surrounding functions
    Expected [ [] & (? : ?)]
-}
test12 :: String -> String 
test12 ss = foo ss ++ (show $ bar ss)
    where 
        fuz xs = xs ++ xs
        foo [] = ""
        foo (xs : xs1) = ""
        bar xs = length xs

{-  
    Test splitting 2 patterns in an inline function block
    Expected [ [] & (? : ?)] & [ [] & (? : ?)]
-}
test13 :: String -> String 
test13 ss = foo ss ++ (show $ bar $ fuz ss)
    where 
        fuz xs = xs ++ xs
        foo [] = ""
        foo (xs : xs1) = ""
        bar [] = 0
        bar (xs : xs1) = 0

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
test15 {- Failed to resolve the variables type -}c = ""

{-  
    Test splitting a tree defined in Types.Trees 
    Expected [ (Branch ? ?) & (Leaf ?) ]
-}
test16 :: Tree Int -> Int 
test16 (Branch (t) (t1)) = 0
test16 (Leaf t) = 0


