module Main where 

import Types.Cards
import Types.Lists
import Types.Trees
import Types.User

main :: IO ()
main = putStrLn "TypeLineTests compiled"

-- Testing Type Signature Insertion is a bit thin
-- As Blackbox doesn't really do much work in this for this at all
-- All that needs tested is the ability to extract the function name
-- And put a signature on the front of it in the token stream

{-Exected Type :: User -> String -}

test0{-TYPELINE-} (Student s i ss) = s ++ ss 
test0 (Admin s i) = s ++ (show i)

{-Exected Type :: User -> [String] -}

test1{-TYPELINE-} (Student s i ss) = s : [ss] 
test1 (Admin s i) = s : [show i]

{-Exected Type :: x -> x -}

test2{-TYPELINE-} x = x

{- Expected Type :: Maybe Int -> Int -}

test3{-TYPELINE-} Nothing = 0
test3 (Just i) = i

{-
   Sometimes compiler warnings are sent over STDERR
   We don't care so this tests that they are ignored when
   testing if a file loaded
-}
testCompilerWarningIgnoring a = a
testCompilerWarningIgnoring [] = []