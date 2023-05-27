-- Remember: there are many possible solutions, and if your solution produces
-- the right results, then it is (most likely) correct. However, if your code
-- looks far more complicated than these sample solutions, then you're probably
-- making things too difficult for yourself---try to keep it simple!
{-# LANGUAGE TemplateHaskell #-}
module Tutorial3Sol where
  
import Data.Char
import Data.List
import Test.QuickCheck
  

-- These are some helper functions for makeKey and makeKey itself.
-- Exercises continue below.

rotate :: Int -> [Char] -> [Char]
rotate k list | 0 <= k && k <= length list = drop k list ++ take k list
              | otherwise = error "Argument to rotate too large or too small"

--  prop_rotate rotates a list of length l first by m and then by l-m;
--  together (m + l - m = l) it rotates it all the way round, back to
--  the original list
--
--  m should be between 0 and l; to get m from a random number k we
--  use k `mod` l (but then l can't be 0, since you can't divide by 0)
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
lookUp ch xs = head ([ b | (a,b) <- xs, a == ch] ++ [ch])

lookUpRec :: Char -> [(Char, Char)] -> Char
lookUpRec ch [] = ch
lookUpRec ch ((key,val):restKey)
    | key == ch = val
    | otherwise = lookUpRec ch restKey

prop_lookUp :: Char -> [(Char, Char)] -> Bool
prop_lookUp c key = lookUp c key == lookUpRec c key

-- 2.
encipher :: Int -> Char -> Char
encipher k ch = lookUp ch (makeKey k)

-- 3.
normalise :: String -> String
normalise str  =  [ toUpper ch | ch <- str, isAlpha ch ]

normaliseRec :: String -> String
normaliseRec [] = []
normaliseRec (ch:str)
    | isAlpha ch = toUpper ch : normaliseRec str
    | otherwise  = normaliseRec str

prop_normalise :: String -> Bool
prop_normalise str =  normalise str == normaliseRec str 

-- 4.
enciphers :: Int -> String -> String
enciphers k str = [encipher k ch | ch <- normalise str]

-- 5.
reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey key = [(b, a) | (a, b) <- key]

reverseKeyRec :: [(Char, Char)] -> [(Char, Char)]
reverseKeyRec [] = []
reverseKeyRec ((k,v):keys) = (v,k) : reverseKeyRec keys

prop_reverseKey :: [(Char, Char)] -> Bool
prop_reverseKey key = reverseKey key == reverseKeyRec key

-- 6.
decipher :: Int -> Char -> Char
decipher k ch = lookUp ch (reverseKey (makeKey k))

decipherStr :: Int -> String -> String
decipherStr k str = [decipher k ch | ch <- str, isUpper ch]


-- ** Optional Material

-- 7.
candidates :: String -> [(Int, String)]
candidates str = [ (n, s) | n <- [0..25], let s = decipherStr n str
                 , isInfixOf "AND" s || isInfixOf "THE" s ]


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
encrypt :: Int -> String -> String
encrypt n str = concat (transpose (splitEachFive (enciphers n str)))

splitFiveWays :: String -> [String]
splitFiveWays xs | n `mod` 5 == 0 = splitEach (n `div` 5) xs
                 | otherwise      = error "splitFiveWays: not a multiple of 5"
                 where n = length xs

splitEach :: Int -> String -> [String]
splitEach _ [] = []
splitEach n xs = take n xs : splitEach n (drop n xs)

-- 9.
decrypt :: Int -> String -> String
decrypt n str = concat (transpose (splitFiveWays (decipherStr n str)))

-- ** Automatically run QuickCheck properties named 'prop_*' and any other tests.
return []

main :: IO ()
main = do
  qc <- $quickCheckAll
  let showB b = if b then "Pass" else "Fail"
  putStrLn $ "QuickCheck: " ++ showB qc
