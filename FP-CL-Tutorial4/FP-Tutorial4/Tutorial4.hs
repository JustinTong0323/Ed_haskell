module Tutorial4 where

import Data.Char
import Data.List
import Data.Ratio
import Test.QuickCheck


-- ** Optional material

-- 1. doubles
-- a.
doublesComp :: [Int] -> [Int]
doublesComp xs =  [x * 2 | x <- xs]

-- b.
doublesRec :: [Int] -> [Int]
doublesRec [] =  []
doublesRec (x:xs) = x * 2 : doublesRec xs

-- c.
doublesHO :: [Int] -> [Int]
doublesHO  = map (*2)

-- d.
prop_doubles :: [Int] -> Bool
prop_doubles xs =  doublesComp xs == doublesRec xs && doublesRec xs == doublesHO xs

-- 2. aboves
-- a.
abovesComp :: Int -> [Int] -> [Int]
abovesComp n xs =  [x | x <- xs, x > n]

-- b.
abovesRec :: Int -> [Int] -> [Int]
abovesRec n [] =  []
abovesRec n (x:xs)
    | x > n = x : abovesRec n xs
    | otherwise = abovesRec n xs

-- c.
abovesHO :: Int -> [Int] -> [Int]
abovesHO n =  filter (>n)

-- d.
prop_aboves :: Int -> [Int] -> Bool
prop_aboves n xs =  abovesComp n xs == abovesRec n xs && abovesRec n xs == abovesHO n xs

-- 3. parity
-- a.
xor :: Bool -> Bool -> Bool
xor a b
    | a && b = False
    | otherwise = a || b

-- b.
parityRec :: [Bool] -> Bool
parityRec [] =  True
parityRec (x:xs) = x `xor` parityRec xs

-- c.
parityHO :: [Bool] -> Bool
parityHO =  foldr xor True

-- d.
prop_parity :: [Bool] -> Bool
prop_parity xs =  parityRec xs == parityHO xs

-- 4. allcaps
-- a.
allcapsComp :: String -> Bool
allcapsComp xs =  and [isUpper x | x <- xs, isAlpha x]

-- b.
allcapsRec :: String -> Bool
allcapsRec [] = True
allcapsRec (x:xs)
    | isAlpha x = isUpper x && allcapsRec xs
    | otherwise = allcapsRec xs

-- c.
allcapsHO :: String -> Bool
allcapsHO xs =  foldr ((&&) . isUpper) True (filter isAlpha xs)

-- d.
prop_allcaps :: String -> Bool
prop_allcaps xs =  allcapsComp xs == allcapsRec xs && allcapsRec xs == allcapsHO xs


-- ** Optional material
-- Matrix manipulation

type Matrix = [[Rational]]

-- 5
-- a.
uniform :: [Int] -> Bool
uniform [] = True
uniform xs = all (== head xs) xs

-- b.
valid :: Matrix -> Bool
valid xs = not (all null xs) && uniform (map length xs)


-- 6.
width :: Matrix -> Int
width m = length (head m)

height :: Matrix -> Int
height = length

plusRow :: [Rational] -> [Rational] -> [Rational]
plusRow = zipWith (+)

plusM :: Matrix -> Matrix -> Matrix
plusM a b
    | valid a && valid b && width a == width b && height a == height b = zipWith plusRow a b
    | otherwise = error "Invalid Input!!"

-- 7.
dot :: [Rational] -> [Rational] -> Rational
dot a b = sum (zipWith (*) a b)

timesM :: Matrix -> Matrix -> Matrix
timesM a b
    | valid a && valid b && width a == height b = [[dot x y | y <- transpose b] | x <- a]
    | otherwise = error "Invalid Input!!"

ma :: Matrix
ma = [[1,2,3],[4,5,6]]

mb :: Matrix
mb = [[7,8],[9,10],[11,12]]

-- ** Challenge

-- 8.
-- Mapping functions
mapMatrix :: (a -> b) -> [[a]] -> [[b]]
mapMatrix f = map (map f)

zipMatrix :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
zipMatrix f m1 m2 = [zipWith f x y | (x,y) <- zip m1 m2]

-- All ways of deleting a single element from a list
removes :: [a] -> [[a]]
removes = undefined

-- Produce a matrix of minors from a given matrix
minors :: Matrix -> [[Matrix]]
minors m = undefined

-- A matrix where element a_ij = (-1)^(i + j)
signMatrix :: Int -> Int -> Matrix
signMatrix w h = [[(-1)^(i+j) | i <- [1..w]] | j <- [1..h]]

determinant :: Matrix -> Rational
determinant = undefined

cofactors :: Matrix -> Matrix
cofactors m = undefined

scaleMatrix :: Rational -> Matrix -> Matrix
scaleMatrix k = undefined

inverse :: Matrix -> Matrix
inverse m = undefined

-- Tests
identity :: Int -> Matrix
identity n = [[if i == j then 1 else 0 | i <- [1..n]]|j <- [1..n]]

prop_inverse2 :: Rational -> Rational -> Rational
                -> Rational -> Property
prop_inverse2 a b c d = undefined

type Triple a = (a,a,a)

prop_inverse3 :: Triple Rational ->
                 Triple Rational ->
                 Triple Rational ->
                 Property
prop_inverse3 r1 r2 r3 = undefined
