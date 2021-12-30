
--Newtypes
newtype Metre = Metre Int 

-- CLASSS
--provides a way to give functions multiple meansdepend on the type of the func
class Show a where
    show :: a -> String

show :: Show a => a -> String
-- this above says 
-- "if a is a member of the Show family, then show has type a->String"

-- INSTANCE 
instance Show Bool where
    show True = "True"
    show False = "False"

--DERIVERING
-- derive definitions automatically for many datatypes
data Suit = Spades | Heart | Clubs
     deriving Show


--EQUALITY
instance Eq Bool where
    True == True = True
    False == False = False
    True == False = False
    deriving (Eq, Show)

