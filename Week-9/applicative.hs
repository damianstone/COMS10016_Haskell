{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Applicatives where

    -- for applicatives <*> means f (a -> b) -> f a -> f b
    -- for functor <$> means f a -> f (a -> b)



import Prelude hiding (Functor(..), (<$>), Applicative(..))
import Functors -- This imports our local Functors.hs file
                -- Just for illustration. In practice, we would
                -- use the predefined classes in the Prelude


-- Goal: Apply functions within a context,
--       creating structure based on structure,
--       which will allow us to combine results

-- I can use function as well as Applicative
class Functor f => Applicative f where
    --fmap :: (a -> b) -> f a -> f b
    (<*>) :: f (a -> b) -> f a -> f b
    pure :: a -> f a

instance Applicative Maybe where
    (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
    Just f  <*> Just x   = Just (f x)
    Nothing <*> Just x   = Nothing 
    Just f  <*> Nothing  = Nothing
    Nothing <*> Nothing  = Nothing

    pure :: a -> Maybe a
    pure = Just

-- >>> Just (+1) <*> Just 2
-- Just 3
-- >>> Just (+1) <*> Nothing
-- Nothing
-- >>> Nothing <*> Just 2
-- Nothing
-- >>> Nothing <*> Nothing
-- Nothing

instance Applicative [] where 
    (<*>) :: [a -> b] -> [a] -> [b]
    [] <*> xs = []
    fs <*> [] = []
    -- (1) (f:fs) <*> xs = map f xs ++ (fs <*> xs)
    -- (2)
    (f:fs) <*> (x:xs) = f x : (fs <*> xs)

    pure :: a -> [a]
    pure x = [x]

-- >>> (1) [(+1), (*3)] <*> [-2,-1,0,1,2]
-- [-1,0,1,2,3,-6,-3,0,3,6]
-- >>> [] <*> [1,2,3]
-- []
-- >>> [1,2,3] <*> []
-- []
-- >>> (2) [(+1), (*3)] <*> [1,2,3]
-- [2,6]

instance Applicative (Either e) where 
    (<*>) :: Either e (a -> b) -> Either e a -> Either e b
    Right f <*> Right x = Right (f x)
    _       <*> Left y  = Left y
    Left x  <*> _       = Left x

    pure = Right

-- >>> pure (+1) <*> [1,2,3] 
-- [2,3,4]


-- undercore in the left hand side means I dont care about the value
-- in the right hand side means I dont know the type 
addOne :: Applicative f => f Int -> f Int 
addOne mx = fmap (+1) mx

fmapAp :: Applicative f => (a -> b) -> f a -> f b
fmapAp f mx = pure f <*> mx

data User = MkUser String Int   
  deriving Show

createUser :: Maybe String -> Maybe Int -> Maybe User 
createUser maybeName maybeAge 
    = MkUser <$> maybeName <*> maybeAge

-- >>> createUser (Just "John") (Just 20)
-- Just (MkUser "John" 20)


-- ADDITIONAL EXERCISES

liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f mx my = error "Implement me!"

-- What does this do for Maybe? What does it do for list?
doTwice :: Applicative f => f a -> f (a,a)
doTwice mx = error "Implement me!"

sequenceAp :: Applicative f => [f a] -> f [a]
sequenceAp = error "Implement me!"

-- >>> sequenceAp [Just 3, Just 2, Just 1]
-- Just [3,2,1]
-- >>> sequenceAp [Just 3, Nothing, Just 1]
-- Nothing

mapAp :: Applicative f => (a -> f b) -> [a] -> f [b]
mapAp f xs = error "Implement me!"

-- >>> mapAp justOdd [1,2,3]
-- Nothing
-- >>> mapAp justOdd [1,3]
-- Just [1,3]

justOdd :: Int -> Maybe Int
justOdd x = if odd x then Just x else Nothing
