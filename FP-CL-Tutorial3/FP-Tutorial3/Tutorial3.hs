module Tutorial3 where

import Data.Char
import Data.List
import Test.QuickCheck


-- These are some helper functions for makeKey and makeKey itself.
-- Exercises continue below.

rotate :: Int -> [Char] -> [Char]
rotate k list | 0 <= k && k <= length list = drop k list ++ take k list
              | otherwise = error "Argument to rotate too large or too small"

--  prop_rotate rotates a list of lenght l first an arbitrary number m times,
--  and then rotates it l-m times; together (m + l - m = l) it rotates it all
--  the way round, back to the original list
--
--  to avoid errors with 'rotate', m should be between 0 and l; to get m
--  from a random number k we use k `mod` l (but then l can't be 0,
--  since you can't divide by 0)

prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                        where l = length str
                              m = if l == 0 then 0 else k `mod` l

alphabet = ['A'..'Z']

makeKey :: Int -> [(Char, Char)]
makeKey k = zip alphabet (rotate k alphabet)

-- ** Caesar Cipher Exercises

-- 1.
lookUp :: Char -> [(Char, Char)] -> Char
lookUp c xs = let li = [n | (m, n) <- xs , m == c] in if null li then c else head li

lookUpRec :: Char -> [(Char, Char)] -> Char
lookUpRec c [] = c
lookUpRec c (x:xs)
      | c == fst x = snd x
      | otherwise = lookUpRec c xs

{-Fail attempt
lookUpRec x xs = lookUpRecA x xs 0
      where
            lookUpRecA :: Char -> [(Char, Char)] -> Int -> Char
            lookUpRecA x xs i
                  | i == length xs + 1= x
                  | let (m, n) = xs !! i in x == m = n
                  | otherwise = lookUpRecA x xs (i+1)
-}

prop_lookUp :: Char -> [(Char, Char)] -> Bool
prop_lookUp c xs = lookUp c xs == lookUpRec c xs

-- 2.
encipher :: Int -> Char -> Char
encipher n c = lookUp c (makeKey n)

-- 3.
normalise :: String -> String
normalise xs = [toUpper x | x <- xs, isAlpha x]

normaliseRec :: String -> String
normaliseRec [] = []
normaliseRec (x:xs)
      | isAlpha x = toUpper x : normaliseRec xs
      | otherwise = normaliseRec xs

prop_normalise :: String -> Bool
prop_normalise xs = normalise xs == normaliseRec xs

-- 4.
enciphers :: Int -> String -> String
enciphers n xs = [encipher n x | x <- normalise xs]

-- 5.
reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey xs = [(n,m) | (m,n) <- xs]

reverseKeyRec :: [(Char, Char)] -> [(Char, Char)]
reverseKeyRec [] = []
reverseKeyRec (x:xs) = (n,m) : reverseKeyRec xs
      where (m,n) = x

prop_reverseKey :: [(Char, Char)] -> Bool
prop_reverseKey xs = reverseKey xs == reverseKeyRec xs

-- 6.
decipher :: Int -> Char -> Char
decipher n c = lookUp c (reverseKey (makeKey n))

decipherStr :: Int -> String -> String
decipherStr n xs = [decipher n x | x <- xs, isUpper x]


-- ** Optional Material

-- 7.

candidates :: String -> [(Int, String)]
candidates xs = [ (n,x) | (n,x) <- [(n,decipherStr n xs) | n <- [0..25]], isInfixOf "THE" x || isInfixOf "AND" x]


splitEachFive :: String -> [String]
splitEachFive xs | length xs > 5 = take 5 xs : splitEachFive (drop 5 xs)
                 | otherwise     = [ fillToFive xs ]

fillToFive :: String -> String
fillToFive xs = xs ++ replicate (5 - length xs) 'X'

-- An alternative solution demonstrating 'repeat'
fillToFive' :: String -> String
fillToFive' xs = take 5 (xs ++ repeat 'X')

-- The following example shows why 'transpose' is not
--  invertible in general. The transpose function
--  takes the 'columns' of a list of lists, and makes
--  them the 'rows' of a new list of lists. 
--
-- [[o n e],           [[o t t f f],       [[o n e e e],
--  [t w o],            [n w h o i],        [t w o r],  
--  [t h r e e],   -->  [e o r u v],   -->  [t h r e],  
--  [f o u r],          [e r e],            [f o u], 
--  [f i v e]   ]       [e],        ]       [f i v]     ]   

-- 8.
join :: [String] -> String
join [] = []
join (x:xs) = x ++ join xs

encrypt :: Int -> String -> String
encrypt n xs =  join (transpose (splitEachFive (enciphers n xs)))

-- 9.
splitEachN :: String -> [String]
splitEachN xs = split' xs n
      where n = length xs `div` 5
            split' :: String -> Int -> [String]
            split' xs n
                  | length xs >= n = take n xs : split' (drop n xs) n
                  | otherwise     =  []

decrypt :: Int -> String -> String
decrypt n xs = decipherStr n (join (transpose (splitEachN xs)))



