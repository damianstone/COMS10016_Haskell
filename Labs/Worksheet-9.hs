{-# language InstanceSigs #-}
{-# language ConstrainedClassMethods #-}
import Prelude hiding (Monad(..), (*>))

-- MONADS

class Applicative m => Monad m where
    (>>=) :: m a -> (a -> m b) -> m b
    (>>) :: m a -> m b -> m b
    return :: Monad m => a -> m a
    return = pure

-- 1 MAYBE MONAD ---------------------------------------------------------------------------


-- lawful Monad instance for maybe 
instance Monad Maybe where 
    Just x >>= f = f x
    Nothing >>= _ = Nothing
    return = Just
-- example ghci> Just 3 >>= (\x -> Just (x+1)) // Just 4

 -- UNLAWFUL Monad instance for maybe
-- instance Monad Maybe where 
--    Just x >>= f = Nothing
--    Nothing >>= _ = Nothing
-- example ghci> Just 3 >>= (\x -> Nothing) // Nothing

-- 2 EITHER MONAD ---------------------------------------------------------------------------

-- Either monad
instance Monad (Either e) where
    Left a >>= f = Left a
    Right b >>= f = f b
-- example ghci> Left "error" >>= (\x -> Right (x+1)) // Left "error"
-- example ghci> Right 3 >>= (\x -> Left (x+1)) // Left 4


-- safeDiv calculate x / y but return an error message if y is 0
safeDiv :: Int -> Int -> Either String Int
safeDiv x 0 = Left ("Tried to divide " ++ show x ++ " by 0")
safeDiv x y = Right (div x y)


-- safeDiv2 calculate ((x/y)/z)
safeDiv2 :: Int -> Int -> Int -> Either String Int
safeDiv2 x y z = do
    u <- safeDiv x y
    safeDiv u z
-- example ghci> safeDiv2 3 0 0 // Left "Tried to divide 3 by 0"
-- example ghci> safeDiv2 123 23 2 // Right 2

-- safeDiv2' desugaring do notation
safeDiv2' :: Int -> Int -> Int -> Either String Int
safeDiv2' x y z = safeDiv x y >>= (\u -> safeDiv u z)
-- example ghci> safeDiv2' 3 0 0 // Left "Tried to divide 3 by 0"
-- example ghci> safeDiv2' 123 23 2 // Right 2


-- safeDivAll devide all numbers in a list
safeDivAll :: Int -> [Int] -> Either String Int
safeDivAll x [] = Right x
safeDivAll x (y:ys) = do
    u <- safeDiv x y
    safeDivAll u ys
-- example ghci> safeDivAll 123 [0,0,0] // Left "Tried to divide 123 by 0"
-- example ghci> safeDivAll 123 [1,2,3] // Right 20

-- safeDivAll' desugaring do notation
safeDivAll' :: Int -> [Int] -> Either String Int
safeDivAll' x [] = Right x
safeDivAll' x (y:ys) = safeDiv x y >>= (\u -> safeDivAll' u ys)
-- example ghci> safeDivAll' 123 [0,0,0] // Left "Tried to divide 123 by 0"
-- example ghci> safeDivAll' 123 [1,2,3] // Right 20

-- 4 TREE MONAD ---------------------------------------------------------------------------

data Tree a = Leaf a | Branch (Tree a) (Tree a)
    deriving (Eq, Show)

-- functor tree
instance Functor Tree where
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Branch l r) = Branch (fmap f l) (fmap f r)

-- applicative tree
instance Applicative Tree where
    pure x = Leaf x
    (Leaf f) <*> (Leaf x) = Leaf (f x)
    (Branch l r) <*> (Branch l' r') = Branch (l <*> l') (r <*> r')

-- lowful monad instance for tree
instance Monad Tree where
    Leaf x >>= f = f x
    Branch l r >>= f = Branch (l >>= f) (r >>= f)
-- example ghci> Branch (Leaf 1) (Leaf 2) >>= (\x -> Branch (Leaf x) (Leaf (x+1))) // Branch (Branch (Leaf 1) (Leaf 2)) (Branch (Leaf 3) (Leaf 4))


-- 5 LIST MONAD ---------------------------------------------------------------------------

-- monad list
instance Monad [] where
    return x = [x]
    xs >>= f = concat (map f xs)
-- example ghci> [1,2,3] >>= (\x -> [x,x]) // [1,1,2,2,3,3]

cartesian :: [a] -> [b] -> [(a,b)]
cartesian xs ys = do 
    x <- xs
    y <- ys
    return (x,y)
-- example ghci> cartesian [1,2,3] [4,5,6] // [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]


-- 6 THE READER MONAD ----------------------------------------------------------------------

newtype Reader a b = MkReader (a -> b)
instance Monad (Reader d) where
    return x = MkReader (\_ -> x)
    (MkReader f) >>= g = MkReader (\x -> let MkReader h = g (f x) in h x)
-- example ghci> MkReader (\x -> x+1) >>= (\x -> MkReader (\y -> x+y)) // MkReader (\x -> x+1+1)


-- 7 THE STATE MONAD -----------------------------------------------------------------------

instance Monad ((->) s) where
    return x = \s -> x
    f >>= g = \s -> g (f s) s
-- example ghci> (\x -> x+1) >>= (\x -> (\y -> x+y)) // \x -> (\y -> x+y) (\x -> x+1)


-- 8 THE IO MONAD --------------------------------------------------------------------------

instance Monda IO where
    return x = IO (\s -> (x, s))
    (IO f) >>= g = IO (\s -> let (x, s') = f s in let (IO f') = g x in f' s')
-- example ghci> (putStrLn "hello" >>= (\x -> getLine >>= (\y -> return (x ++ y)))) // hello


