{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Applicatives where
import Functors
import Prelude hiding (Functor(..), (<$>), Applicative(..))

    -- for applicatives <*> means f (a -> b) -> f a -> f b
    -- for functor <$> means f a -> f (a -> b)


-- 1 FUNCTORS

class Functor f where
    fmap :: (a -> b) -> f a -> f b

-- instance for lists
instance Functor [] where 
  fmap = map


-- a) Instance for maybe
instance Functor Maybe where
    fmap f (Just x) = Just (f x)
    fmap f Nothing = Nothing
-- example ghci> fmap (+1) (Just' 2) // Just' 3


-- b) instantance for Either
instance Functor (Either a) where
    fmap f (Right x) = Right (f x)
    fmap f (Left x) = Left x
--example ghci> fmap (*2) (Right' 3) // Right' 6


-- c) instance for Tree
data Tree a = Leaf a | Branch (Tree a) (Tree a)
    deriving (Show)

instance Functor Tree where
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Branch l r) = Branch (fmap f l) (fmap f r)
-- example ghci> fmap (+1) (Branch (Leaf 1) (Leaf 2)) // Branch (Leaf 2) (Leaf 3)


-- d) instance for newtype
newtype Reader a b = MkReader (a -> b) 

instance Functor (Reader d) where
    fmap :: (b -> c) -> Reader a b -> Reader a c      
    fmap f (MkReader g) = MkReader (f . g)
-- example ghci> fmap (+1) (MkReader (\x -> x + 1)) // MkReader (+1)

-- IO
instance Functor IO where  
    fmap f action = do  
        result <- action  
        return (f result)  
-- example ghci> fmap (+1) (putStrLn "Hello") // putStrLn "Hello"



--  2 APPLICATIVES

-- all applicatives are also functors

-- applicative type class
class Functor f => Applicative f where
    (<$>) :: (a -> b) -> f a -> f b
    (<*>) :: f (a -> b) -> f a -> f b --applying functions within a context
    pure :: a -> f a -- lifting values into a contexto

-- LIST
instance Applicative [] where 
    (<*>) = zipWith ($)
    pure x = [x]
-- example ghci> pure (+1) <*> [1,2,3] // [2,3,4]

-- MAYBE
instance Applicative Maybe where
    (Just f) <*> (Just x) = Just (f x)
    _ <*> _ = Nothing
    pure = Just
-- example ghci> pure (+1) <*> Just 2 // Just 3

-- EITHER
instance Applicative (Either e) where
    (Right f) <*> (Right x) = Right (f x)
    (Left x) <*> _ = Left x
    _ <*> (Left y) = Left y
    pure = Right
-- example ghci> pure (+1) <*> Right 2 // Right 3

-- Tree
instance Applicative Tree where
    (Leaf f) <*> oak = fmap f oak
    (Branch l r) <*> oak = Branch (l <*> oak) (r <*> oak)
    pure = Leaf
-- example ghci> pure (+1) <*> (Branch (Leaf 1) (Leaf 2)) // Branch (Leaf 2) (Leaf 3)

-- Reader a
instance Applicative (Reader a) where
    (<*>) :: Reader a (b -> c) -> Reader a b -> Reader a c
    (MkReader fabc) <*> (MkReader fab) = MkReader (\x -> fabc x (fab x))
    pure x = MkReader (\_ -> x)
--example ghci> pure (+1) <*> (MkReader (\x -> x + 1)) // MkReader (+1)


-- instance for applicative IO
instance Applicative IO where











