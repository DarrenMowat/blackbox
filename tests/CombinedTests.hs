module Main where 

import Types.Cards
import Types.Lists
import Types.Trees
import Types.User

main :: IO ()
main = putStrLn "CombinedTests compiled"

{- 
    Combined tests are a way of testing that diffrent commands can be run on a file
    in one session
-}

{-  
    Test Purpose: Testing fucntion with several lines

    Expected Scope [ s :: String, i :: Int, s2 :: String ]
    Expected Return Type [ Return Type :: String ]
-}
test0 :: User -> String
test0 (Student s i s2) = {-SCOPE-} ""
test0 (Admin s i) = ""



{-Exected Type :: User -> String || User -> [Char] -}

test1{-TYPELINE-} (Student s i ss) = s ++ ss 
test1 (Admin s i) = s ++ (show i)

{-Exected Type :: User -> [String] -}

test2{-TYPELINE-} (Student s i ss) = s : [ss] 
test2 (Admin s i) = s : [(show i)]

{-
    Testing List - From internal type db

    Expected [ [] & (x:xs) ]
-}
test3 :: [Int] -> Int
test3 {-SPLIT-}xs = 0

{-
    Testing Either - From internal type db

    Expected [ Left ? & Right ? ]
-}
test4 :: Either Int Int -> Int
test4 {-SPLIT-}xs = 0
