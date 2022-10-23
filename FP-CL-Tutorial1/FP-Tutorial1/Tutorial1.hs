module Tutorial1 where

import PicturesSVG -- needed for the optional chess part
import Test.QuickCheck
--import Data.Time.Format.ISO8601 (yearFormat)
--import GHC.Stats (RTSStats(cumulative_par_balanced_copied_bytes))


-- 2.
double :: Int -> Int
double x = x + x

square :: Int -> Int
square x = x * x

-- 3.
isTriple :: Int -> Int -> Int -> Bool
isTriple a b c = square a + square b == square c

-- 4.
leg1 :: Int -> Int -> Int
leg1 x y = square x - square y

leg2 :: Int -> Int -> Int
leg2 x y = 2 * x * y

hyp :: Int -> Int -> Int
hyp x y = square x + square y

-- 5.
prop_triple :: Int -> Int -> Bool
prop_triple x y = isTriple (leg1 x y) (leg2 x y) (hyp x y)

-- 7.
pic1 :: Picture
pic1 = above (twoBeside knight) (invert (twoBeside knight))

pic2 :: Picture
pic2 = above (twoBeside knight) (invert (twoBeside (flipV knight)))

-- ** Functions

twoBeside :: Picture -> Picture
twoBeside x = beside x (invert x)


-- 8.
twoAbove :: Picture -> Picture
twoAbove x = above x (invert x)

fourPictures :: Picture -> Picture
fourPictures x = twoAbove (twoBeside x)

-- 9.
-- a)
emptyRow :: Picture
emptyRow = repeatH 4 (beside whiteSquare blackSquare)

-- b)
otherEmptyRow :: Picture
otherEmptyRow = repeatH 4 (beside blackSquare whiteSquare)

-- c)
middleBoard :: Picture
middleBoard = repeatV 2 (above emptyRow otherEmptyRow)

-- d)
whiteRow1 :: Picture
whiteRow1 = beside rook (beside knight (beside bishop (beside queen (beside king (beside bishop (beside knight rook))))))

whiteRow :: Picture
whiteRow = over whiteRow1 otherEmptyRow

blackRow :: Picture
blackRow = over (invert whiteRow1) emptyRow

whitePawns :: Picture
whitePawns = repeatH 8 pawn

-- e)
populatedBoard :: Picture
populatedBoard = above blackRow (above (over (invert whitePawns) otherEmptyRow)(above middleBoard (above (over whitePawns emptyRow) whiteRow)))
