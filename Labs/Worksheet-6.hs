import Prelude hiding (fromInteger)
import Test.QuickCheck

-- 1 LIST COMPREHENSIONS

-- a) using nats define square that retins the square of all integers
nats :: [Integer] 
nats = [1,2,3,4,5,6]

squared :: [Integer]
squared = [x^2 | x <- nats]
-- example ghci> squared // = [1,4,9,16,25,36]

-- b) transforming one list containing values of type a into one containing bs
map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]
-- example ghci> map' (^2) [1,2,3] // = [1,4,9]

-- c) 
filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = [x | x <- xs, p x]
-- example ghci> filter' even [1,2,3,4,5] // = [2,4]

-- d) returns the cartesian product of two finite lists
cartesian :: [a] -> [a] -> [(a,a)]
cartesian xs ys = [(x,y) | x <- xs, y <- ys]
-- example ghci> cartesian [1,2,3] [4,5,6] // = [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]

-- BIT STRING

-- a) Write a function that will output all possible bit strings of length n
bitString :: Int -> [String]
bitString 0 = [""]
bitString n = [x ++ y | x <- ["0", "1"], y <- bitString (n - 1)]
-- example ghci> bitString 3 // = ["000","001","010","011","100","101","110","111"]

-- b) Define a function bitStrings that outputs all bit Strings up to length n in lexicographic order
bitStrings :: Int -> [String]
bitStrings n = concat (map bitString [0..n])
-- example ghci> bitStrings 3 // = ["000","001","010","011","100","101","110","111"]


-- 2 TYPE CLASSES
class Pretty a where
    pretty :: a -> String


-- a) define the pretty instance for this data structure
data Reaction = Happy | Sad | Excited | Angry | Indifferent
    deriving (Show, Eq)

instance Pretty Reaction where 
    pretty Happy = ":)"
    pretty Sad = ":("
    pretty Excited = ":D"
    pretty Angry = ">:("
    pretty Indifferent = ":|"

-- b) Discuss the best practice for writing Show instances with respect to the Read instance
-- It is best to have the string that show outputs match the constructor because it allows
-- the constructor to more easily have a Read instance. read·show ideally should act as the identity
-- function.

-- c) define the eq instance
--instance Eq a => Eq [a] where
--    [] == [] = True
--    (x:xs) == (y:ys) = (x == y) && (xs == ys)
--    _ == _ = False


data Suit = Heart | Diamond | Club | Spade deriving (Show, Eq)
data Face = Ace | Two | Three | Four | Five | Six | Seven
           | Eight | Nine | Ten | Jack | Queen | King
    deriving (Show, Eq, Ord)
data Card = Joker | Card (Face, Suit) 
    deriving (Show, Eq)

instance Ord Card where 
    _ <= Joker = True
    Joker <= _ = False
    (Card(f,s)) <= (Card(f',s'))
        | s' == Spade && s /= Spade = True
        | s' /= Spade && s == Spade = False
        | s' == Heart && s /= Heart = True
        | s' /= Heart && s == Heart = False
        | otherwise = f <= f'

-- d) function zipWith
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f (x:xs) (y:ys) = (f x y) : (zipWith' f xs ys)
zipWith' _ _ _ = []
-- example ghci> zipWith' (+) [1,2,3] [4,5,6] // = [5,7,9]


-- e) Instances for num using zipWith
--class Num' a where
--    (+) :: a -> a -> a
--    --(-) :: a -> a -> a
--    (*) :: a -> a -> a
--    (/) :: a -> a -> a
--    abs' :: a -> a                -- the absolute value of a number
--    signum' :: a -> a              -- the sign of a number, either −1, 0, or 1
--    fromInteger' :: Integer -> a   -- conversion from an integer into a number
--    fromRational' :: Integer -> a
--
--instance Num' a => Num' [a] where
--    (+) = zipWith (+)
--    --(-) = zipWith (-)
--    (*) = zipWith (*)
--    (/) = zipWith (/)
--    abs' = map abs'
--    signum' = map signum'
--    fromInteger' x = repeat (fromInteger' x)
--    fromRational' x = repeat (fromRational' x)


-- f) 
--nats' :: [Double]
--nats' = [1,2,3,4,5,6]
--
--ns' :: [Double]
--ns' = 2 * nats' + 1
--
--signs' :: [Double]
--sings' = cycle [1, -1]

--pis' :: [Double]
--pis' = 4/ns' * signs'

--pi' :: Int -> Double
--pi' n = sum (take n pis')

-- 3 TREES

data Tree a = Leaf a
            | Fork (Tree a) (Tree a)
    deriving Show

data Bush a = Tip
            | Node (Bush a) a (Bush a)
    deriving Show

data Tush a = TLeaf a
            | TNode (Tush a) a (Tush a)
    deriving Show

data RoseTree a = RoseLeaf a
                | RoseFork [RoseTree a]
    deriving Show

data RoseBush a = RoseBush a [RoseBush a]
    deriving Show

-- b) which of these can be instantianed without any data?
-- Bush and RoseTree

-- c) How could you instantiate the rest without data?
-- by wrapping in a Maybe

-- d) 

data Tree' a = Leaf' a
            | Split (Tree' a) (Tree' a)
    deriving Show

-- returns all integer in a given tree as a list, in the same order as they occur in the tree
collapse :: Tree' Int -> [Int]
collapse (Leaf' n) = [n]
collapse (Split l r) = collapse l ++ collapse r
-- example ghci> collapse (Split (Leaf' 1) (Leaf' 2)) // = [1,2]

mirror :: Tree' a -> Tree' a
mirror (Split l r) = Split (mirror r) (mirror l)
mirror t  = t
-- example ghci> mirror (Split (Leaf' 1) (Leaf' 2)) // = Split (Leaf' 2) (Leaf' 1)

foldTree :: (a -> b) -> (b -> b -> b) -> Tree' a -> b
foldTree tleaf tsplit (Leaf' x) = tleaf x
foldTree tleaf tsplit (Split l r) = tsplit (foldTree tleaf tsplit l) (foldTree tleaf tsplit r)
-- example ghci> foldTree (+) (*) (Split (Leaf' 1) (Leaf' 2)) // = 3

-- redefine collapse function using foldTree
collapse' :: Tree' Int -> [Int]
collapse' = foldTree (: []) (++)
-- example ghci> collapse' (Split (Leaf' 1) (Leaf' 2)) // = [1,2]


-- PROPERTIES OF TREES

--instance Arbitrary a => Arbitrary (Tree a) where
--    arbitrary = sized genTree
--genTree :: Arbitrary a => Int -> Gen (Tree a)
--genTree s = frequency
--    [(s, do 
--        l <- genTree s'
--        r <- genTree s'
--        return (Split l r))
--    ,(1, do 
--          a <- arbitrary
--          return (Leaf a))
--    ]
--   where
--      s' = s `div` 2
--
---- Write a generator that generates random lists of integers that are sorted (ordered)
--orderedList :: Gen [Int]
--orderedList = 
--        do 
--        xs <- arbitrary
--            return (sort xs)
---- example ghci> sample orderedList // = [1,2,3,4,5,6,7,8,9,10]
--
--listOfLength :: Int -> Gen a -> Gen [a]
--listOfLength n gen = sequence [gen | i <- [1..n]]
---- example ghci> sample (listOfLength 10 arbitrary) // = [1,2,3,4,5,6,7,8,9,10]



