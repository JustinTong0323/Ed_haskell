-- Informatics 1 - Functional Programming 
-- Class Test 2022

module ClassExam where

import Data.Char
import Test.QuickCheck

-- Problem 1

-- a

f :: String -> Int
f xs = sum [ord x | x <- xs, isAlpha x]

-- b

g :: String -> Int
g [] = 0
g (x:xs) = if isAlpha x then ord x + g xs else g xs

-- c

h :: String -> Int
h xs = foldr ((+).ord) 0  (filter isAlpha xs)

-- d

prop_fgh :: String -> Bool
prop_fgh xs = f xs == g xs && g xs == h xs

-- Problem 2

-- a

c :: String -> String -> Bool
c xs ys = and [ x == y || not (isAlpha x) || not (isAlpha y) | (x,y) <- zip xs ys]

-- b

d :: String -> String -> Bool
d _ [] = True
d [] _ = True
d (x:xs) (y:ys) = ( x == y || not (isAlpha x) || not (isAlpha y) ) && d xs ys

-- c

prop_cd :: String -> String -> Bool
prop_cd xs ys = c xs ys == d xs ys

