-- File to read and write from worksheet 7
-- Hello world for Haskell is traditionally fibonacci
main :: IO ()
main = print (fib 10)
fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)



