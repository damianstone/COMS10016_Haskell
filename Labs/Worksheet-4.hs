-- 1 PATTERN MATCHING ON LISTS --------------------------------------------------------------------------

-- Generate an ascending list of integers
ascendingList :: Int -> Int -> [Int]
ascendingList n m
       | m < n = []
       | m == n = [m]
       | m > n = n : ascendingList (n+1) m
-- example ghci> ascendingList 1 10 // [1,2,3,4,5,6,7,8,9,10]

-- length function => returns the number of elements in a given list
length :: [a] -> Int 
length [] = 0
length (x:xs) = 1 + length xs

-- sum of all the elements in a list
sum :: [Int] -> Int
sum [] = 0
sum (x:xs) = xs + sum xs

-- fromJusts xs contains all the values that were wrapped in a Just constructor
fromJusts :: [Maybe a] -> [a]
fromJusts [] = []
fromJusts (Nothing:xs) = fromJusts xs
fromJusts ((Just x):xs) = x : fromJusts xs

-- list xs sorted in ascending order
-- insert an element in a list in the correct place 
insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (y:ys) 
         | x > y = y : insert x ys
         | otherwise = x : y : ys

-- sort a list in ascending order
isort :: [Int] -> [Int]
isort [] = []
isort (x:xs) = insert x (isort xs)

-- merge two lists in acending order
merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
         | x <= y = x : merge xs (y:ys)
         | otherwise = y : merge (x:xs) ys

-- the result of split xs is the pair of lists (ys, zs)
split :: [a] -> ([a],[a])
split [] = ([],[])
split (x:xs) = (x:ys,zs)
         where (ys,zs) = split xs


-- 2 PROPERTIES AND TESTING --------------------------------------------------------------------------
data List a = Empty
        | Cons a (List a)
    deriving Eq

toList :: [a] -> List a
toList [ ] = Empty
toList (x : xs) = Cons x (toList xs)

-- enverse of toList
fromList :: List a -> [a]
fromList Empty = []
fromList (Cons x xs) = x : (fromList xs)


repeat :: a -> [a]
repeat x = x : repeat x
--  length (take n (repeat x)) = n

-- 3 REVERSE

(++) :: [a] -> [a] -> [a]
[ ] ++ ys = ys
(x : xs) ++ ys = x : (xs ++ ys)


reverse :: [a] -> [a]
reverse [ ] = [ ]
reverse (x : xs) = reverse xs ++ [x]


