
data Tree a  = Empty | Fork a (Tree a) (Tree a)
   deriving (Show, Eq)
-- Show => prints elements with type tree a
-- Eq => compare equality

--SPLIT => take a list with size A and slip into 2 equals lists
split :: [a] -> ([a], [a])
split [] = ([], [])
split (x:xs) = (x:ys, zs)
   where (zs, ys) = split xs

--PLANT => take a list and transform into a tree
plant :: [a] -> Tree a  
plant[] = Empty
plan (x:xs) = Fork 2 (plan zs) (plant ys)
   where (z:zs, ys) = split xs

--FLATTEN => go trough the tree and return the element of the fork
flatten :: Tree a -> [a]
flatten Empty = []
flatten (Fork z lt rt) = flatten lt ++ [z] ++ flatten rt

-- BINARY SEARCH TREES (BST)
-- Fork z lt(left) rt(right) is a binary search tree whenever 
-- 1. everything in flatten lt is less than (o equal) z 
-- 2. everything in flatten rt is greater than z 
-- 3. lt, and rt, are both BST

insert :: Ord a => a -> Tree a -> Tree a
insert x Empty = Fork x Empty Empty
insert x (Fork z lt rt)
    | x <= z = Fork z (insert x lt) rt 
    | otherwise = Fork z lt (insert x rt)


example = plant (reverse['z', 'f', 'k', 'b', 'a', 'd'])

-- insert elements one by one in the binary search tree
plantBST :: Ord a => [a] -> Tree a
plantBST = foldr insert Empty

lookupBST :: Ord a => a -> Tree a -> Bool
lookupBST x Empty = False
lookupBST x (Fork z lt rt)
   | x == z = True
   | x < z = lookupBST x lt 
   | otherwise = lookupBST x rt 

sort :: Ord a => [a] -> [a] 
sort =  flatten . plantBST 



-- Test the software
instance Arbitrary a => Arbitrary (Tree a) where
   arbitrary = sized arbTree 

arbTree :: Arbitrary a => Int -> Gen (Tree a)
arbTree 0 = do
   return Empty
arbTree n = do
   lt <- arbTree (n `div` 2)
   rt <- arbTree (n `div` 2)
   x <- arbTree 
   return (Fork x lt rt)

