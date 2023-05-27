-- Remember: there are many possible solutions, and if your solution produces
-- the right results, then it is (most likely) correct. However, if your code
-- looks far more complicated than these sample solutions, then you're probably
-- making things too difficult for yourself---try to keep it simple!
{-# LANGUAGE TemplateHaskell #-}
module Tutorial2Sol where
  
import Data.Char
import Data.List
import Test.QuickCheck
  

-- 1. inRange

inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [x | x <- xs, lo <= x, x <= hi]


-- 2. multDigits

multDigits :: String -> Int
multDigits str = product [digitToInt ch | ch <- str, isDigit ch]

countDigits :: String -> Int
countDigits str = length [ ch | ch <- str, isDigit ch ]

prop_multDigits :: String -> Bool
prop_multDigits xs = multDigits xs <= 9 ^ countDigits xs


-- 3. capitalise and title

lowercase :: String -> String
lowercase word = [toLower c | c <- word]

capitalise :: String -> String
capitalise (first:word) = toUpper first : lowercase word

capitaliseLong :: String -> String
capitaliseLong word | length word >= 4 = capitalise word
                    | otherwise        = lowercase word

title :: [String] -> [String]
title [] = []
title (wd:wds) = capitalise wd : [capitaliseLong wd | wd <- wds]

test =
  title ["how","much","wood","would","A","WOODCHUCK","cHuck"]

-- 4. score

isVowel :: Char -> Bool
isVowel x  =  toLower x `elem` "aeiou"

score :: Char -> Int
score x = delta (isAlpha x) + delta (isUpper x) + delta (isVowel x)
  where
  delta t = if t then 1 else 0 

totalScore :: String -> Int
totalScore xs = product [ score x | x <- xs, score x > 0 ]

prop_totalScore_pos :: String -> Bool
prop_totalScore_pos xs = totalScore xs >= 1


-- ** Optional Material

-- 5. crosswordFind

crosswordFind :: Char -> Int -> Int -> [String] -> [String]
crosswordFind letter pos len words =
    [wd | wd <- words,  
          length wd == len,
          0 <= pos,
          pos < len, 
          wd !! pos == letter]


-- 6. search

-- This solution demonstrates the use of an infinite list [0..] of
-- indices. The "zip" function only zips pairs of elements together
-- as long as it has two elements to pair;  when it gets to the end
-- of one list, it stops.

search :: String -> Char -> [Int]
search str goal = [i | (c, i) <- zip str [0..], c == goal]

-- The length of the search result should be the same as the number of goal characters in the string.
prop_search :: String -> Char -> Bool
prop_search str goal = length [ c | c <- str, c == goal] == length (search str goal)
                      

-- ** Automatically run QuickCheck properties named 'prop_*' and any other tests.
return []

main :: IO ()
main = do
  qc <- $quickCheckAll
  putStrLn ("QuickCheck: " ++ if qc then "Pass" else "Fail")
