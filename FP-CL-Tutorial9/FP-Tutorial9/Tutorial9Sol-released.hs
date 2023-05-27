-- Remember: there are many possible solutions, and if your solution produces
-- the right results, then it is (most likely) correct. However, if your code
-- looks far more complicated than these sample solutions, then you're probably
-- making things too difficult for yourself---try to keep it simple!
{-# LANGUAGE TemplateHaskell #-}

module Tutorial9Sol where

-- Sudoku solver
-- Based on Bird, "Thinking Functionally with Haskell"
  
import Data.List (sort,nub,(\\),transpose,genericLength)
import Data.String (lines,unlines)
import Test.QuickCheck
  

-- Representing Sudoku puzzles

type Row a    = [a]
type Matrix a = [Row a]
type Digit    = Char

-- Examples from websudoku.com

easy :: Matrix Digit
easy = ["    345  ",
        "  89   3 ",
        "3    2789",
        "2 4  6815",
        "    4    ",
        "8765  4 2",
        "7523    6",
        " 1   79  ",
        "  942    "]

medium :: Matrix Digit
medium = ["   4 6 9 ",
          "     3  5",
          "45     86",
          "6 2 74  1",
          "    9    ",
          "9  56 7 8",
          "71     64",
          "3  6     ",
          " 6 9 2   "]

hard :: Matrix Digit
hard = ["9 3  42  ",
        "4 65     ",
        "  28     ",
        "     5  4",
        " 67 4 92 ",
        "1  9     ",
        "     87  ",
        "     94 3",
        "  83  6 1"]

evil :: Matrix Digit
evil = ["  9      ",
        "384   5  ",
        "    4 3  ",
        "   1  27 ",
        "2  3 4  5",
        " 48  6   ",
        "  6 1    ",
        "  7   629",
        "     5   "]

-- Another example, from Bird's book

book :: Matrix Digit
book = ["  4  57  ",
        "     94  ",
        "36      8",
        "72  6    ",
        "   4 2   ",
        "    8  93",
        "4      56",
        "  53     ",
        "  61  9  "]

-- Printing Sudoku puzzles

group :: [a] -> [[a]]
group = groupBy 3

groupBy :: Int -> [a] -> [[a]]
groupBy n [] = []
groupBy n xs = take n xs : groupBy n (drop n xs)

intersperse :: a -> [a] -> [a]
intersperse sep []     = [sep]
intersperse sep (y:ys) = sep : y : intersperse sep ys

showRow :: String -> String
showRow = concat . intersperse "|" . group

showGrid :: Matrix Digit -> [String]
showGrid = showCol . map showRow
  where
    showCol = concat . intersperse [bar] . group
    bar     = replicate 13 '-'

put :: Matrix Digit -> IO ()
put = putStrLn . unlines . showGrid

-- 1.
choice :: Digit -> [Digit]
choice d | d `elem` ['1'..'9'] = [d]
         | d == ' '            = ['1'..'9']

choices :: Matrix Digit -> Matrix [Digit]
choices = map (map choice)

-- 2.
splits :: [a] -> [(a, [a])]
splits xs  =
  [ (xs!!k, take k xs ++ drop (k+1) xs) | k <- [0..n-1] ]
  where
  n = length xs

single :: [Digit] -> Bool
single [d] = True
single _   = False

the :: [Digit] -> Digit
the [d] = d

fixed :: Row [Digit] -> [Digit]
fixed row = [ the ds | ds <- row, single ds ]

pruneRow :: Row [Digit] -> Row [Digit]
pruneRow row = [ ds \\ fixed rest | (ds,rest) <- splits row ]

-- this code builds on pruneRow to also prune columns and boxes

pruneBy :: (Matrix [Digit] -> Matrix [Digit]) -> Matrix [Digit] -> Matrix [Digit]
pruneBy f = f . map pruneRow . f

rows, cols, boxs :: Matrix a -> Matrix a
rows = id
cols = transpose
boxs = map ungroup . ungroup . map cols . group . map group
  where
    ungroup :: Matrix a -> [a]
    ungroup = concat

prune :: Matrix [Digit] -> Matrix [Digit]
prune = pruneBy boxs . pruneBy cols . pruneBy rows

-- 3.
close :: Eq a => (a -> a) -> a -> a
close f x | x == y    = x
          | otherwise = close f y
          where y = f x

-- 4.
extract :: Matrix [Digit] -> Matrix Digit
extract mat | all (all single) mat = map (map the) mat

-- 5.
solve :: Matrix Digit -> Matrix Digit
solve = extract . close prune . choices

-- solves easy and medium, not hard or evil

-- ** Optional Material

-- 6.
failed :: Matrix [Digit] -> Bool
failed = any (any null)

-- 7.
solved :: Matrix [Digit] -> Bool
solved = all (all single)

-- 8.
shortest :: Matrix [Digit] -> Int
shortest = minimum . filter (> 1) . map length . concat

-- 9.
expand :: Matrix [Digit] -> [Matrix [Digit]]
expand mat =
  [ preMat ++ [preRow ++ [[d]] ++ postRow] ++ postMat | d <- ds ]
  where
    short ds              = length ds == shortest mat
    (preMat, row:postMat) = break (any short) mat
    (preRow, ds:postRow)  = break short row

-- 10.
search :: Matrix Digit -> [Matrix Digit]
search = map extract . loop . choices
  where
    loop :: Matrix [Digit] -> [Matrix [Digit]]
    loop mat | solved pruned = [pruned]
             | failed pruned = []
             | otherwise     = [ soln | exp <- expand pruned, soln <- loop exp ]
             where
               pruned = close prune mat

-- solves easy, medium, hard and evil (for which there are 17 solutions)


-- display puzzle and then solution(s) found by search

puzzle :: Matrix Digit -> IO ()
puzzle g = put g >> puts (search g) >> putStrLn "***"
     where puts = sequence_ . map put

main :: IO ()
main = puzzle easy >>
       puzzle medium >>
       puzzle hard >>
       puzzle evil

                 
-- ** Automatically run QuickCheck properties named 'prop_*' and any other tests.
return []

mainQC :: IO ()
mainQC = do  
  qc <- $forAllProperties $ quickCheckWithResult $ stdArgs {maxSize = 5}
  let showB b = if b then "Pass" else "Fail"
  putStrLn $ "QuickCheck: " ++ showB qc
