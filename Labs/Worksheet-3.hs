-- 1 give the types
deception :: [[[a]]]
deception = [[[ ], [ ]], [[ ]], [[ ], [ ], [ ]]]

-- 2 PATTERN MATCHING
-- a)
delta :: Int -> Int
delta 0 = 1
delta 1 = 0

-- b)
dirac :: Int -> (Int -> Int)
-- dirac n t = delta (t - n) give alternative definition using (.)
diract n = delta . subtract n

not :: Bool -> Bool
not True = False
not False = True

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- CARDS
data Suit = Hearts | Diamonds | Clubs | Spades
type Pip = Int
type Rank = Either Pip Court
data Card = Joker | Card Suit Rank

data Court = Ace | Jack | Queen | King

snap :: Card -> Card -> String 
snap Joker Joker = "SNAP"
snap (Card s1 r1) (Card s2 r2)
    | s1 == s2 && r1 == r2 = "SNAP"
    | otherwise = "NO SNAP"

-- MAYBE
data Maybe a = Just a | Nothing
isJust :: Maybe a -> Bool 
isJust (Just x) = True 
isJust Nothing = False

isNothing :: Maybe a -> Bool
isNothing = not . isJust

safeDiv :: Int -> Int -> Maybe Int
safeDiv x 0 = Nothing
safeDiv x y = Just (div x y)

fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing = x
fromMaybe _ (Just x) = x

perform :: (Int -> Int) -> Maybe Int -> Maybe Int
perform f Nothing = Nothing
perform f (Just x) = Just (f x)


-- EITHER
data Either a b = Left a | Right b

forget :: Either String a -> Maybe a 
forget (Left _) = Nothing
forget (Right x) = Just x

safeDiv' :: Int -> Int -> Either String Int
safeDiv' x 0 = Left "Division by zero"
safeDiv' x y = Right (div x y)




