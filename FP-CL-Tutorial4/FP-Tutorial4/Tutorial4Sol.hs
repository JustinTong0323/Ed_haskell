-- Remember: there are many possible solutions, and if your solution produces
-- the right results, then it is (most likely) correct. However, if your code
-- looks far more complicated than these sample solutions, then you're probably
-- making things too difficult for yourself---try to keep it simple!
{-# LANGUAGE TemplateHaskell #-}
module Tutorial4Sol where
  
import Data.Char
import Data.List
import Test.QuickCheck
import Data.Ratio
  

-- 1. doubles
-- a.
doublesComp :: [Int] -> [Int]
doublesComp xs  =  [ 2*x | x <- xs ]

-- b.
doublesRec :: [Int] -> [Int]
doublesRec []      =  []
doublesRec (x:xs)  =  (2*x) : doublesRec xs

-- c.
doublesHO :: [Int] -> [Int]
doublesHO xs  =  map double xs
  where
  double x = 2 * x

-- or using sections and partial application
doublesHO' :: [Int] -> [Int]
doublesHO' =  map (2 *)

-- d.
prop_doubles :: [Int] -> Bool
prop_doubles xs  =
  doublesComp [3, 1, 4, 2, 3] == [6, 2, 8, 4, 6] &&
  doublesComp xs == doublesRec xs &&
  doublesComp xs == doublesHO xs &&
  doublesComp xs == doublesHO' xs

-- 2. aboves
-- a.
abovesComp :: Int -> [Int] -> [Int]
abovesComp w xs  =  [ x | x <- xs, x > w ]

-- b.
abovesRec :: Int -> [Int] -> [Int]
abovesRec w []                  =  []
abovesRec w (x:xs) | x > w      =  x : abovesRec w xs
                   | otherwise  =  abovesRec w xs

-- c.
abovesHO :: Int -> [Int] -> [Int]
abovesHO w xs  =  filter isAbove xs
  where
  isAbove x = x > w

-- or using sections and partial application
abovesHO' :: Int -> [Int] -> [Int]
abovesHO' w  =  filter (> w)

-- d.
prop_aboves :: Int -> [Int] -> Bool
prop_aboves w xs =
  abovesComp 2 [3, 1, 4, 2, 3] == [3, 4, 3] &&
  abovesComp w xs == abovesRec w xs &&
  abovesComp w xs == abovesHO w xs &&
  abovesComp w xs == abovesHO' w xs

-- 3. parity
-- a.
xor :: Bool -> Bool -> Bool
xor x y  =  (x && not y) || (not x && y)

xor' :: Bool -> Bool -> Bool
xor' x y  =  not (x == y)

prop_xor :: Bool -> Bool -> Bool
prop_xor x y = x `xor` y == x `xor'` y

-- b.
parityRec :: [Bool] -> Bool
parityRec []      =  True
parityRec (x:xs)  =  x `xor` parityRec xs

-- c.
parityHO :: [Bool] -> Bool
parityHO xs  =  foldr xor True xs

-- or using partial application
parityHO' :: [Bool] -> Bool
parityHO' =  foldr xor True

-- d.
prop_parity :: [Bool] -> Bool
prop_parity xs =
  parityRec [] == True &&
  parityRec [True] == False &&
  parityRec [True, True] == True &&
  parityRec [False, True, True] == True &&
  parityRec xs == parityHO xs &&
  parityRec xs == parityHO' xs

-- 4. allcaps
-- a.
allcapsComp :: String -> Bool
allcapsComp xs  =  and [ isUpper x | x <- xs, isAlpha x ]

-- b.
allcapsRec :: String -> Bool
allcapsRec []                  =  True
allcapsRec (x:xs) | isAlpha x  =  isUpper x && allcapsRec xs
                  | otherwise  =  allcapsRec xs

-- c.
allcapsHO :: String -> Bool
allcapsHO xs  =  foldr (&&) True (map isUpper (filter isAlpha xs))

-- or using partial application and composition
allcapsHO' :: String -> Bool
allcapsHO'  =  foldr (&&) True . map isUpper . filter isAlpha

-- d.
prop_allcaps :: String -> Bool
prop_allcaps xs =
  allcapsComp "" == True &&
  allcapsComp "Hello!" == False &&
  allcapsComp "HELLO!" == True &&
  allcapsComp xs == allcapsRec xs &&
  allcapsComp xs == allcapsHO xs &&
  allcapsComp xs == allcapsHO' xs


-- ** Optional material
-- Matrix manipulation

type Matrix = [[Rational]]

-- 5
-- a.
uniform :: [Int] -> Bool
uniform []     = True
uniform (x:xs) = all (== x) xs

-- The library function 'all' can be defined as:
--
-- all :: (a -> Bool) -> [a] -> Bool
-- all p xs = foldr (&&) True (map p xs)
--
-- Or using partial application and composition:
--
-- all p = foldr (&&) True . map p

-- b.
valid :: Matrix -> Bool
valid []     = False
valid (x:xs) = not (null x) && uniform (map length (x:xs))

-- using partial application and composition:
valid' :: Matrix -> Bool
valid' m = uniform (map length m)             -- (1)
        && not (null m) && all (not . null) m -- (2)


-- 6.
width :: Matrix -> Int
width m = length (head m)

height :: Matrix -> Int
height m = length m

-- plusRow :: [Rational] -> [Rational] -> [Rational]
-- plusRow = zipWith (+)

plusM :: Matrix -> Matrix -> Matrix
plusM m n | okay  = zipWith (zipWith (+)) m n
  where
    okay = valid m && valid n &&
           width m == width n &&
           height m == height n

-- 7.
timesM :: Matrix -> Matrix -> Matrix
timesM m1 m2 | okay  =  [ [ dot row col | col <- transpose m2 ]
                                        | row <- m1 ]
  where
    okay = valid m1 && valid m2 && width m1 == height m2
    dot xs ys = sum (zipWith (*) xs ys)

-- ** Challenge

-- 8.
-- Mapping functions
mapMatrix :: (a -> b) -> [[a]] -> [[b]]
mapMatrix f = map (map f)

zipMatrix :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
zipMatrix f = zipWith (zipWith f)

-- All ways of deleting a single element from a list
removes :: [a] -> [[a]]
removes []     = []
removes (x:xs) = xs : map (x :) (removes xs)

-- Produce a matrix of minors from a given matrix
minors :: Matrix -> [[Matrix]]
minors m = map (map transpose . removes . transpose) (removes m)

-- A matrix where element a_ij = (-1)^(i + j)
signMatrix :: Int -> Int -> Matrix
signMatrix w h = cycleN h [evenRow, oddRow]
  where evenRow     = cycleN w [1,-1]
        oddRow      = cycleN w [-1,1]
        cycleN n xs = take n (cycle xs)
        
determinant :: Matrix -> Rational
determinant [[x]] = x
determinant m = sum (zipWith (*) row (cycle [1,-1]))
  where f x m = x * determinant m
        row   = head (zipMatrix f m (minors m))

cofactors :: Matrix -> Matrix
cofactors m = zipMatrix (*) (mapMatrix determinant (minors m)) signs
  where signs = signMatrix (width m) (height m)
        
                
scaleMatrix :: Rational -> Matrix -> Matrix
scaleMatrix k = mapMatrix (k *)

inverse :: Matrix -> Matrix
inverse [[r]] = [[denominator r % numerator r]] -- spacial case for 1x1 matrix
inverse m = scaleMatrix (1 / determinant m) (transpose (cofactors m))

-- Tests
identity :: Int -> Matrix
identity n = map f [0..n - 1]
  where f m = take n (replicate m 0 ++ [1] ++ repeat 0)

prop_inverse1 :: Rational -> Property
prop_inverse1 a = determinant m /= 0 ==> 
                       m `timesM` inverse m    == identity 1
                       && inverse m `timesM` m == identity 1
  where m = [[a]]

prop_inverse2 :: Rational -> Rational -> Rational 
                -> Rational -> Property
prop_inverse2 a b c d = determinant m /= 0 ==> 
                       m `timesM` inverse m    == identity 2
                       && inverse m `timesM` m == identity 2
  where m = [[a,b],[c,d]]

type Triple a = (a,a,a)

prop_inverse3 :: Triple Rational -> 
                 Triple Rational -> 
                 Triple Rational ->
                 Property
prop_inverse3 r1 r2 r3 = determinant m /= 0 ==> 
                         m `timesM` inverse m    == identity 3
                         && inverse m `timesM` m == identity 3
  where m           = [row r1, row r2, row r3]
        row (a,b,c) = [a,b,c] 


-- ** Automatically run QuickCheck properties named 'prop_*' and any other tests.
return []

main :: IO ()
main = do  
  qc <- $quickCheckAll
  let showB b = if b then "Pass" else "Fail"
  putStrLn $ "QuickCheck: " ++ showB qc
