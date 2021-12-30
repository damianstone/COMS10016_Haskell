-- 1 PATTERN MATCHING ON LISTS

length :: [a] -> Int 
length [] = 0
length (x:xs) = 1 + length xs

sum :: [Int] -> Int
sum [] = 0
sum (x:xs) = xs + sum xs

fromJusts :: [Maybe a] -> [a]
fromJusts [] = []
fromJusts (Nothing:xs) = fromJusts xs
fromJusts ((Just x):xs) = x : fromJusts xs


insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (y:ys) 
         | x > y = y : insert x ys
         | otherwise = x : y : ys

isort :: [Int] -> [Int]
isort [] = []
isort (x:xs) = insert x (isort xs)

merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
         | x <= y = x : merge xs (y:ys)
         | otherwise = y : merge (x:xs) ys

split :: [a] -> ([a],[a])
split [] = ([],[])
split (x:xs) = (x:ys,zs)
         where (ys,zs) = split xs


-- 2 PROPERTIES AND TESTING
data List a = Empty
        | Cons a (List a)
    deriving Eq

toList :: [a] -> List a
toList [ ] = Empty
toList (x : xs) = Cons x (toList xs)

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


