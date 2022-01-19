import Prelude hiding (Functor, Just, Maybe, Nothing, fmap)

-- fmap id = id  => (functor identity)
-- fmap g · fmap f = fmap (g · f) => (functor composition)

-- 1 FUNCTORS

class Functor f where
    fmap :: (a -> b) -> f a -> f b

instance Functor [] where
    fmap = map

-- define functorns for the following types

-- a)
data Maybe' a = Just' a | Nothing'
instance Functor Maybe' where
    fmap f (Just' x) = Just'(f x)
    fmap f Nothing' = Nothing'

-- b)
data Either' a b = Left' a | Right' b
instance Functor (Either' a) where
    fmap f (Left' x) = Left' x
    fmap f (Right' x) = Right' (f x)

-- c)
data Tree a = Leaf a | Branch (Tree a) (Tree a)
instance Functor Tree where
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Branch l r) = Branch (fmap f l) (fmap f r)

-- d) 
newtype Reader a b = MkReader (a -> b)
instance Functor (Reader a) where
    fmap f (MkReader g) = MkReader (f . g)

-- APLICATIVES

































