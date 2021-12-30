--high order functions 


-- GROUPBY
-- breaks a list into segments, such that the property is satisfied
-- by the elements in the segments
--groupBy :: (a->a->Bool) ->[a]->[[a]]

--FOLDR => you can use in any next operations
foldr :: (a->b->b) ->b->[a]->b
foldr f k [] = k 
foldr f k (x:xs) = f x (foldr f k xs)

-- SUM a list intergers with recursion
sum' :: [Integer] -> Integer
sum' [] = 0
sum' (x:xs) = x + sum xs

foldSum :: [Integer] -> Integer
foldSum = foldr (+) 0 

-- PRODUCT of a list inteteger with recursion
product' :: [Integer] -> Integer
product' [] = 1
product' (x:xs) = x * product'(xs)

foldProduct :: [Integer] -> Integer 
foldProduct = foldr (*) 1

and' :: [Bool] -> Bool
and' = foldr (&&) True

-- "hello world" = "hEllo wOrld"
spongeBob :: String -> String
spongeBob = f k
   where 
       f :: Char -> String -> String
       f x (y:ys)
         | isUpper y = toLower x++ (y:ys)
         | isLower y = toUpper x++ (y:ys)
       k :: String 
       k = []





