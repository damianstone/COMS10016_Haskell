class Applicative m => Monad m where
    (>>=) :: m a -> (a -> m b) -> m b


(>=>) :: Monad m 
=> (a -> m b) 
-> (b -> m c) 
-> (a -> m c)
famb >=> fbmc = \x -> famb >>= fbmc


-- Monad Laws

-- left identity
-- function that takes a and return a monad
pure a >>= f = f a

-- right identity
-- function that takes a monad and return a monad
m >>= pure == m

-- associativity
-- function that takes a monad and return a monad
m >=> (g >=> h) == (f >=> g) >=> h
