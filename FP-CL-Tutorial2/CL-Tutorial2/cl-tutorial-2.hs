import Data.List
-- Exercise 2
data Thing = A | B | C | D | E
    deriving (Eq,Show)

things :: [Thing]
things = [A, B, C, D, E]

data Colour = Amber | Blue
    deriving (Eq,Show)

data Shape = Disc | Square
    deriving (Eq,Show)

data Size = Big | Small
    deriving (Eq,Show)

data Border = Thin | Thick
    deriving (Eq,Show)

colour :: Thing -> Colour
colour A = Amber
colour B = Amber
colour C = Amber
colour D = Amber
colour E = Blue

shape :: Thing -> Shape
shape A = Square
shape B = Square
shape C = Disc
shape D = Square
shape E = Square

size :: Thing -> Size
size A = Big
size B = Big
size C = Big
size D = Big
size E = Small

border :: Thing -> Border
border A = Thick
border B = Thin
border C = Thick
border D = Thick
border E = Thick

-- Exercise 3
type Predicate u = u -> Bool

isBlue :: Predicate Thing
isBlue x = x == D

isAmber :: Predicate Thing
isAmber x = x /= D

isDisc :: Predicate Thing
isDisc x = x == C

isSquare :: Predicate Thing
isSquare x = x /= C

isSmall :: Predicate Thing
isSmall x = x == E

isBig :: Predicate Thing
isBig x = x /= E

isThin :: Predicate Thing
isThin x = x == B

isThick :: Predicate Thing
isThick x = x /= B

-- Exercise 4

-- Every blue square has a thin border.
a :: Bool
a = and [isThin x | x <- things, isBlue x && isSquare x ]

-- Some amber disc is not big.
b :: Bool
b = or [not (isBig x) | x <- things, isAmber x && isDisc x ]

-- Exercise 5

-- No square is blue
-- It is not the case that some X(square) is Y(blue)
c :: Bool
c = not (or [isBlue x | x <- things, isSquare x ])

-- Every X is not Y
d :: Bool
d = and [not (isBlue x) | x <- things, isSquare x]

-- Exercise 6

thingsOtherThan :: Thing -> [Thing]
thingsOtherThan x = delete x things

properties :: [Predicate Thing]
properties = [isBlue, isAmber, isDisc, isSquare, isSmall, isBig, isThin, isThick]

propertiesOf :: Thing -> [Predicate Thing]
propertiesOf x = [p | p <- properties, p x]

isPropertyOfAnotherThing :: Predicate Thing -> Thing -> Bool
isPropertyOfAnotherThing p x = or [p x' | x' <- thingsOtherThan x]

propertiesOnlyOf :: Thing -> [Predicate Thing]
propertiesOnlyOf x = [p | p <- properties, not (isPropertyOfAnotherThing p x)]

rank :: Thing -> Int
rank x = length (propertiesOnlyOf x)

