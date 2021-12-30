add :: (Int, Int) -> Int -- take two inputs and return one output 
add (x, y) = x + y

plus :: Int -> (Int -> Int)  -- take one inputs and return and function
plus x y = x + y
 -- to use this => plus 7 3 (it will be 10)

--the function curry is defined as follows: function
curry :: ((a, b)-> c) -> (a -> (a -> c))
curry f x y = f (x, y)

uncurry :: (a -> (b -> c)) -> ((a, b) -> c) 
uncurry g (x, y) = g x y

