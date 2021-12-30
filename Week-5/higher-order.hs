{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import GHC (HsSplice(XSplice))

-- a function which takes another function as a parameter 

even :: Int -> Bool -- no higher order function
even n = n `mod` 2 == 0 

-- SOME EXAMPLES
-- map even [1,2,3,4,5] => map will be a higer order function and takes two parameters
-- filter even [1,2,3,4,5] = [2,4] => filter remove elements in the list


-- FILTER => removes the elements from a list that do not satisfy some predicate
-- try filter even [1,2,3,4,5]
filter1 :: (a -> Bool) -> [a] -> [a] -- receive a function and a list 
filter1 p [] = []
filter1 p (x:xs) 
    | p x = x : filter1 p xs   
    | otherwise = filter1 p xs

-- MAP => transform each element in the list by that function 
maping :: (a -> b) -> [a] -> [b]
maping f [] = []
maping f (x:xs) = f x : maping f xs   

--eg. map square :: [Int] -> [Int] currying 

-- take elements from a list while a condition is checked
takeLine :: String -> String 
takeLine [] = []
takeLine (x:xs) 
        | x/='\n' = x:takeLine xs -- the condition is that the element is not \n
        | otherwise = []

takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 p [] = []
takeWhile1 p (x:xs)
          | p x = x:takeWhile1 p xs 
          | otherwise = []

dropWhile1 :: (a -> Bool) -> [a] -> [a]
dropWhile1 _[] = []
dropWhile1 p (x:xs) 
          | p x = x:dropWhile1 p xs 
          | otherwise = xs




