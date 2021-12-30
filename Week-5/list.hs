import Data.Char (toLower, toUpper, toTitle)
import Prelude hiding (splitAt) -- solve the prelude and ambigous errors

sumOddElements :: [Int] -> Int
sumOddElements [] = 0
sumOddElements (x:xs)
            | odd x = x + sumOddElements xs -- odd is a haskell function that verify a odd number
            | otherwise = sumOddElements xs

-- another way
sumOddFilter :: [Int] -> Int
sumOddFilter = sum . filter odd -- sumOddFilter [1,2,3,4,5,6,7,8] = 16


-- take the first n elements of a list
take' :: Int -> [a] -> [a]
take' n [] = []
take' n (x:xs) 
      | n <= 0 = []
      | otherwise = x : take' (n-1) xs
-- take' 2 [a,b,c,d]
-- a : take 1 xs [b,c,d] 
-- a : b : take 0 xs [c,b]
-- result = [a,b]

-- remove th first n elements of the list
drop' :: Int -> [a] -> [a]
drop' n [] = []
drop' n (x:xs)
      | n <= 0 = x:xs
      | otherwise = drop' (n-1) xs
-- drop' 1 [2,3,4] // this case drop one element
-- drop' 0 xs [3,4] return = [3,4]



-- splits a list after n elements
splitAt' :: Int -> [a] -> ([a],[a])
splitAt' n xs = (take' n xs, drop' n xs)
-- splitAt' 2 [1,2,3,4,5,6,7,8,9,10]


-- example: replicateElements 3 [1,2,3] = [1,1,1,2,2,2,3,3,3]:
replicateElements :: Int -> [a] -> [a]
replicateElements n[] = []
replicateElements n (x:xs) = replicate n x ++ replicateElements n xs

-- lower ['A', 'B', 'C'] / return = 'a'
lower :: String -> String
lower [] = []
lower (x:xs) = toLower x : lower xs

-- another way 
lower2 :: String -> String
lower2 = map toLower


upper :: String -> String
upper = error "Please implement"

mapString :: (Char -> Char) -> String -> String
mapString = error "Please implement"
