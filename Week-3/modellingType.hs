import Text.Show.Functions

data Suit = Spades | Diamonds | Hearts | Clubs
  deriving (Show)

-- Pattern matching
data Colour = Black | Red
  deriving (Eq, Show)

colour :: Suit -> Colour
colour Spades = Black
colour Hearts = Red
colour Diamonds = Red
colour Clubs = Black

