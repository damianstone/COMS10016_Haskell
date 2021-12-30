--import Prelude hiding (foldr, foldl)

-- sum list
sum1 :: [Int] -> Int   
sum1 [] = 0
sum1 (x:xs) = x + sum1 xs -- this is a pattern matching 

-- using foldr replacing 0 and + parameters   
sum :: [Int] -> Int 
sum = foldr (+) 0


-- USE FOLDR (AS A LIBRARY) FOR LISTS => another good library for this is FOLDL

-- product 
product :: [Int] -> Int 
product = foldr (*) 1
d
-- And 
and :: [Int] -> Int
and = foldr (&&) True

-- concat
concat :: [Int] -> [Int] -> [Int]
concat = foldr (++) []

-- maximum   
maximum :: Ord b => [b] -> b
maximum (x:xs) = foldr max x xs
