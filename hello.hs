import Test.QuickCheck
--1.
max3 :: Int -> Int ->  Int -> Int
max3 a b c
    | a>=b && a>=c = a
    | b>=a && b>=c = b
    | otherwise    = c

--2.

--3.Recursion
squaresRec :: [Int] -> [Int]
squaresRec xs = map (\ x -> x * x) xs

squares :: [Int] -> [Int]
squares xs = [x*x | x <- xs]

prop_squaresRec :: [Int] -> Bool
prop_squaresRec xs = squaresRec xs == squares xs

hanoi :: Int -> Char -> Char -> Char -> [(Char,Char)]
hanoi n a b c =
    if n == 1
        then [(a,c)]
        else hanoi (n-1) a c b ++ hanoi 1 a b c ++ hanoi (n-1) b a c


fact :: Int -> Int
fact n
    | n == 1 = 1
    | n > 1 = n * fact (n-1)
    | otherwise = 0

isPrime :: Int -> Bool
isPrime n = null [n | x <- [2..n-1], n `mod` x == 0]

dot :: [Int] -> [Int] -> Int
dot xs ys = if length xs == length ys then sum [x*y | (x,y) <- zip xs ys] else 0

dotRec :: [Int] -> [Int] -> Int
dotRec [] ys = 0
dotRec xs [] = 0
dotRec (x:xs) (y:ys) = x*y + dotRec xs ys

search :: Eq a => [a] -> a -> [Int]
search xs y = srch xs y 0
    where
        srch :: Eq a => [a] -> a -> Int -> [Int]
        srch [] y i = []
        srch (x:xs) y i
            | x == y = i : srch xs y (i+1)
            | otherwise = srch xs y (i+1)

--4.Higher-order functions
zipwith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipwith' _ [] _ = []
zipwith' _ _ [] = []
zipwith' f (x:xs) (y:ys) = f x y : zipwith' f xs ys

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) =
    let lowerBound = quickSort (filter (<=x) xs)
        higherBound = quickSort (filter (>x) xs)
    in lowerBound ++ [x] ++ higherBound


--5.lambda expression
sumSqr :: [Int] -> Int
sumSqr = foldr ((+) . (^2)) 0 . filter (>0)

--6.Algebraic data Types
data Point = Point Float Float
    deriving(Eq,Show)
    
data Shape = Circle Point Float | Triangle Point Point Point | Rectangle Point Point
    deriving(Eq,Show)

pointsDis :: Point -> Point -> Float
pointsDis (Point x1 y1) (Point x2 y2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^2
surface (Triangle p1 p2 p3) = sqrt (s * (s-a) * (s-b) * (s-c))
    where
        a = pointsDis p1 p2
        b = pointsDis p2 p3
        c = pointsDis p1 p3
        s = (a + b + c) / 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = abs (x2 - x1) * abs (y2 - y1)

--7.Expression Trees as Algebraic data Type
data Exp = Lit Int
    | Add Exp Exp
    | Mul Exp Exp
    deriving Eq

e0 = Add (Lit 1) (Mul (Lit 2) (Lit 3))
e1 = Mul (Add (Lit 1) (Lit 2)) (Lit 3)
e2 = Add e0 (Mul (Lit 4) e1)

par :: String -> String
par s = "(" ++ s ++ ")"

showExp :: Exp -> String
showExp (Lit n) = show n
showExp (Add e f) = par (showExp e ++ "+" ++ showExp f)
showExp (Mul e f) = par (showExp e ++ "*" ++ showExp f)

evalExp :: Exp -> Int
evalExp (Lit n) = n
evalExp (Add e f) = evalExp e + evalExp f
evalExp (Mul e f) = evalExp e * evalExp f

--7.1 Propositions
type Name = String
data Prop = Var Name
    | F
    | T
    | Not Prop
    | Prop :||: Prop
    | Prop :&&: Prop
    deriving Eq

p0 = Var "a" :&&: Not (Var "a")
p1 = (Var "a" :&&: Var "b") :||: (Not (Var "a") :&&: Not (Var "b"))
p2 = (Var "a" :&&: Not (Var "b") :&&: (Var "c" :||: (Var "d" :&&: Var "b")) :||: (Not (Var "b") :&&: Not (Var "a"))) :&&: Var "c"

instance Show Prop where
    show (Var x) = x
    show F = "F"
    show T = "T"
    show (Not p) = par ( "not " ++ show p)
    show (p :||: q) = par (show p ++ " || " ++ show q)
    show (p :&&: q) = par (show p ++ " && " ++ show q)

