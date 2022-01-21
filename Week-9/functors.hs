module Functors where

import Prelude hiding (Functor(..), (**))

-- Functor
class Functor f where 
    fmap :: (a -> b) -> f a -> f b
    (<$) :: a -> f b -> f a

-- INSTANCE FOR MAYBE
instance Functor Maybe where
    fmap f (Just x) = Just (f x)
    fmap f Nothing = Nothing

-- INSTANCE FOR EITHER
instance Functor (Either a) where
    fmap f (Right x) = Right (f x)
    fmap f (Left x) = Left x

-- INSTANCE FOR LISTS
instance Functor [] where
    fmap = map

-- INSTANCE FOR IO
instance Functor IO where
    fmap f action = do  
        result <- action  
        return (f result)  

-- INSTANCE FOR ((->) e)
instance Functor ((->) r) where
    fmap f g = f . g

-- INSTANCE FOR ((,)a)
instance Functor ((,) a) where
    fmap f (x, y) = (x, f y)


toFloat :: Int -> Float -- int to float
toFloat = fromIntegral
-- example ghci> fmap toFloat (Just 10) // Just 10.00


-- MONOIDAL FUNCTORS
--class Functor f => Monoidal f where
--    unit :: f ()
--    (**) :: f a -> f b -> f (a, b)
--
--(<**>) :: Monoidal f => f a -> f (a -> b) -> f b
--(<**>) mf mx = fmap (\(f, x) -> f x) (mf ** mx)
--
--lift2 :: (a -> b -> c) -> f a -> f b -> f c
--lift2 f x = (<**>) (fmap f x)
---- example ghci> lift2 (+) (Just 1) (Just 2) // Just 3




