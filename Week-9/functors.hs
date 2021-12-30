{-# LANGUAGE InstanceSigs #-}
module Functors where

import Prelude hiding (Functor(..))
    --Goal: Apply functions into a context 
    --      applying a function to every element of a container

mapList :: (a -> b) -> [a] -> [b]
mapList f [] = []
mapList f (x:xs) = f x : mapList f xs

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f Nothing = Nothing 
mapMaybe f (Just x) = Just (f x)

data Tree a = Empty | Fork a (Tree a) (Tree a) 
    deriving (Show, Eq)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Empty = Empty
mapTree f (Fork x l r) = Fork (f x) (mapTree f l) (mapTree f r)
-- l and r => the variables of the tree


mapList'  :: (a -> b) -> []    a -> []    b
mapMaybe' :: (a -> b) -> Maybe a -> Maybe b
mapTree' :: (a -> b) -> Tree a -> Tree b
mapList' = mapList
mapMaybe' = mapMaybe
mapTree' = mapTree


-- WHAT ARE INSTANCES? 
-- indivisual type that belongs to a class
-- More info abt Functor in Google

class Functor f where
    fmap :: (a -> b) -> f a -> f b

instance Functor Maybe where
    fmap :: (a -> b) -> Maybe a -> Maybe b
    fmap = mapMaybe

instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap = mapTree

instance Functor [] where 
    fmap = map

-- TRY THE CODE
-- fmap (+1) [1,2,3]
-- fmap (+1) (Just 1)
-- fmap (+1) Empty
-- fmap (+1) (Fork 2 (Fork 1 Empty Empty) (Fork 3 Empty Empty))

(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap

-- Factor Laws:
-- 1. Identity: fmap id == id  
-- >>> fmap id [1,2,3] == id [1,2,3]
-- True

-- 2. Composition: fmap (g . h) == fmap g . fmap h
-- >>> fmap ((*2) . (+1)) [1,2,3] == (fmap (*2) . fmap (+1)) [1,2,3]
-- True


-- Additional exercises to test your understanding

-- 1. Test whether all of our Functor instances are lawful

-- 2. Write a functor instance for Either.
-- Be careful, there's a subtlety involved here

-- instance Functor Either where  -- Invalid. Why? Hint: Try expanding the fmap type
                                  -- when `f = Either` vs. `f = Either c`
instance Functor (Either c) where 
    fmap :: (a -> b) -> Either c a -> Either c b
    fmap f (Left x) = Left x
    fmap f (Right x) = Right (f x)
