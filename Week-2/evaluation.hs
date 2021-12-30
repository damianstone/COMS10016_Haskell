-- Combine Functions
two :: Int -> Int 
two n = n*2

square :: Int -> Int 
square n = n*n

twoSquare :: Int -> Int 
--twoSquare n = square (two n) 
twoSquare = square . two --25 = 2500

infinity :: Int 
infinity = infinity + 1
