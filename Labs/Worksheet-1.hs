

-- SECTION 2: GUESS THE TYPE --------------------------------------------------------------------------
--1
cloudCloud :: Float
cloudCloud = 0.69

-- 2
square :: Integer -> Integer
square x = x * x

-- 3
isPositive :: (Ord a, Num a) => a -> Bool
isPositive x = x > 0

-- 4 solution: (Int, Int)

-- 5 
data Cat = Persian | Siamese | Munchkin
data Dog = Labrador | Pug | Chihuahua

-- solution :: Either Cat Dog

-- 6 a)
takeAway :: Num a => (a, a) -> a
takeAway (x, y) = y - x

substract :: Num a => a -> a -> a
subtract x y = y - x

-- 6 b)
substract = carry takeAway

-- 6 c) 
apply :: (a -> b) -> a -> b
apply f x = f x

-- apply (λx → takeAway (3, x)) 1
-- apply (curry takeAway 3) 1
-- apply (takeAway · ((,) 3)) 1

-- 7 a)
deception :: Either (Either a Bool) b
deception = Left (Right True)

-- 7 b)
mysterious :: ()
mysterious = ()

-- 8 b) same? yes
f :: a -> b
g :: a -> b

-- 9 
choose :: Bool -> a -> b -> c
choose True x y = x
choose False x y = y

-- SECTION 3 PARENTHESES --------------------------------------------------------------------------

-- 1 Remove the redundant parentheses from its type signature.
f:: (((Int) -> (Bool)) -> ((Int) -> (Bool)))
f:: (Int -> Bool) -> (Int -> Bool)

-- 2 
g :: (Int -> Char) -> Int -> Char -> Char
h :: Int -> Char
--(g ((h) (5))) (h 5)))  
g h 5 (h 5)










