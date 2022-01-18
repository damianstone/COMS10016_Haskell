import Prelude hiding (sum, product, and, or, all, any, length, foldr, foldl,reverse, filter)

-- 1 FOLDING 

-- 1 recursion in the following functions
sum :: [Integer] -> Integer
sum [] = 0
sum (x:xs) = x + sum xs

product :: [Integer] -> Integer
product [] = 0
product (x:xs) = x * product xs

-- FOLDR DEFINITION
-- Foldr definition
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f k [] = k
foldr f k (x:xs) = f x (foldr f k xs)

-- USE FOLDR
--sum all the elements in the list 
sumFoldr :: [Integer] -> Integer
sumFoldr xs = foldr (+) 0 xs -- foldr + (sign of operation) default return value and list === foldr (+) 0 xs
-- for example sumFoldr [1,2,3] = 6

-- multiply all the elements in the list
productFoldr :: [Integer] -> Integer
productFoldr xs = foldr (*) 1 xs

-- return true all the elements are true in the list
and :: [Bool] -> Bool 
and xs = foldr (&&) True xs

-- return true if at least one element is true in the list 
or :: [Bool] -> Bool
or xs = foldr (||) False xs

-- return True if for all elements x in xs, p x is True
allFoldr :: (a -> Bool) -> [a] -> Bool
allFoldr p = foldr ((&&) . p) True
-- example ghci> allFoldr (>0) [1,2,3] // True because all elements are greater than 0
-- example ghci> allFoldr (<5) [6,7,8,9,10] // False because all are greater than 5

-- return true if there exists an element x in xs where p x is True
anyFoldr :: (a -> Bool) -> [a] -> Bool
anyFoldr p = foldr((||) . p) False
-- example: anyFoldr (>0) [1,2,3] // True because there is an element greater than 0

lengthFoldr :: [a] -> Int
lengthFoldr = foldr(const (+1)) 0
-- example: ghci> lengthFoldr [1,2,3] // 3

-- Reverse with snoc
snoc :: [a] -> a -> [a]
snoc xs x = xs ++ [x]

reverseFoldr :: [a] -> [a]
reverseFoldr = foldr(flip snoc) []
-- example ghci> reverse [1,2,3] // [3,2,1]

-- Filter using foldr
filterFoldr :: (a -> Bool) -> [a] -> [a]
filterFoldr p xs = foldr (consIf p) []xs
   where
      consIf p x xs
        | p x = x : xs
        | otherwise = xs
-- example ghci> filterFoldr (>1) [1,2,3] // [2,3] - filter all elements greater than 1
-- example ghci> filterFoldr (>5) [1,2,3,4,5,6,7,8,9,10] - filter all elements greater than 5 

-- Groups elements that are repeated in the original list
group :: Eq a => [a] -> [[a]]
group = foldr f [ ]
    where
      f x [ ] = [[x]]
      f x ((y : ys) : yss)
          | x == y = ((x : y : ys) : yss)
          | otherwise = [x] : ((y : ys) : yss)
-- example ghci> group [1, 2, 3, 3, 2] // [[1], [2], [3, 3], [2]]


repeat' :: a -> [a]
repeat' x = x : repeat x

-- Transpose => takes a rectangular list of lists and turns rows to columns
transpose :: [[a]] -> [[a]]
transpose = foldr (zipWith (:)) (repeat' [ ])
-- example ghci> transpose [[1,2,3],[4,5,6],[7,8,9]] // [[1,4,7],[2,5,8],[3,6,9]]

-- permutes all the elements in a list using foldr
permute :: [a] -> [[a]] 
permute = foldr f k
   where
    f x yss = concat (map (sprinkle x) yss)
    k = [[ ]]

sprinkle :: a -> [a] -> [[a]]
sprinkle x = foldr f k where
    k = [[x]]
    f x ((y : ys) : yss) = (y : x : ys) : map (x:) ((y : ys) : yss)
-- example ghci> permute [1,2,3] // [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]


-- Define foldl using foldr
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl g l xs = foldr f k xs l
    where
        f x k l = k (g l x)
        k l = l

-- example ghci> foldl (+) 0 [1,2,3] // 6
-- example ghci> foldl (*) 1 [1,2,3,6,67,7,5,4,3] // 1013040
