{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

--Check is the list is empty
isEmpty :: [a] -> Bool 
isEmpty [] = True 
isEmpty (x:xs) = False 

--test if a list has a single element
isSingle :: [a] -> Bool 
isSingle [] = False 
isSingle [x] = True 
isSingle (x:xs) = False

--return the first element of the list
head :: [a] -> a 
head [] = undefined 
head (x:xs) = x

-- return the last element of the list
tails' :: [a] -> [[a]]
tails' [] = []
tails' (x:xs) = xs : tails' xs

--length of a list is a recursive function 
length'' :: (Num b) => [a] -> b
length'' [] = 0
length'' (_:xs) = 1 + length'' xs




