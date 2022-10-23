module Tutorial2 where

import Data.Char
import Data.List
import Test.QuickCheck


-- 1. inRange

inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [x | x <- xs, x <= hi, x >= lo]


-- 2. multDigits

multDigits :: String -> Int
multDigits str = product [digitToInt x | x <- str, isDigit x]

countDigits :: String -> Int
countDigits str = length [x | x <- str, isDigit x]

prop_multDigits :: String -> Bool
prop_multDigits str = multDigits str <= 9 ^ countDigits str


-- 3. capitalise and title

capitalise :: String -> String
capitalise word = toUpper x : xs
    where (x:xs) = [toLower x | x <- word]

title :: [String] -> [String]
title words = capitalise x : [if length w >= 4 then capitalise w else map toLower w | w <- xs ]
    where (x:xs) = words

-- 4. score and totalScore

score :: Char -> Int
score x
    | x `elem` "AEIOU" = 3
    | isUpper x || x `elem` "aeiou" = 2
    | isLower x = 1
    | otherwise = 0

totalScore :: String -> Int
totalScore xs = product [score x | x <- xs, score x /= 0]

prop_totalScore_pos :: String -> Bool
prop_totalScore_pos xs = totalScore xs >= 1


-- ** Optional Material

-- 5. crosswordFind

crosswordFind :: Char -> Int -> Int -> [String] -> [String]
crosswordFind letter pos len words = [x | x <- words, x !! pos == letter, length x == len]


-- 6. search

search :: String -> Char -> [Int]
search str goal = [x | x <- [0..length str - 1], str !! x == goal]

-- Depending on the property you want to test, you might want to change the type signature
prop_search :: String -> Char -> Bool
prop_search str goal = length (search str goal) <= length str
