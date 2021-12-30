{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import GhcPrelude (String)

-- 1 GUARDS
square :: Int -> Int
square x = x * x

consta :: a -> b -> a
consta k x = k

twice :: (a -> a) -> (a -> a)
twice f x = f (f x)

infinity :: Integer
infinity = 1 + infinity

-- which cause an error and why
len1 :: Int
len1 = length [1 / 0] -- VALID

len2 :: Int
len2 = length [undefined, undefined] -- VALID BECAUSE UNDEFINED IN HASKELL IS AN EXCEPTIONAL VALUE THAT CAN BE AN ELEMENT OF ANY TYPE

-- len3 ::
-- len3 = [1, 'a'] ERROR


-- 2 GUARDS
-- Factorial functions
factorial :: Integer -> Integer
factorial n
  | n <= 0 = 1
  | n >= 1 = n * factorial(n - 1)

-- Polygon

polygon :: Int -> String
polygon n | n == 1 = "Dot"
          | n == 2 = "Line"
          | n == 3 = "Triangle"
          | n == 4 = "Square"
          | n == 5 = "Pentagon"

orientation :: Int -> Int -> String 
orientation x y | x < y = "Landscape"
                | x > y = "Portrait"
                | x == y = "Square"

era :: Int → String
era n
    | n < 0 = "Future"
    | n ≡ 0 = "Present"
    | n ⩽ 66 = "Cenozoic"
    | n ⩽ 252 = "Mesozoic"
    | n ⩽ 541 = "Paleozoic"
    | n ⩽ 1000 = "Neoproterozoic"
    | n ⩽ 1600 = "Mesoproterozoic"
    | n ⩽ 2500 = "Paleoproterozoic"


