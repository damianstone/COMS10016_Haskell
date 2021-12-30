-- Asing value 
aNumber :: Integer
aNumber = 5

aCharacter :: Char
aCharacter = 'a'

aBoolean :: Bool
aBoolean = True 

-- Function 1 SUM
add :: Num a => a -> a -> a
add a b = a + b

saveFunc :: Integer -> Integer
saveFunc = add 10 

-- Functions 3 datatypes
data Color = Red | Blue | Green
sayColor :: Color -> String
sayColor Red = "You are Red!"
sayColor Blue = "You are Blue!"
sayColor Green = "You are Green!"


-- Combine Functions
two :: Int -> Int 
two n = n*2

square :: Int -> Int 
square n = n*n

twoSquare :: Int -> Int 
--twoSquare n = square (two n) -- 25 = 2500
twoSquare = square . two --25 = 2500





