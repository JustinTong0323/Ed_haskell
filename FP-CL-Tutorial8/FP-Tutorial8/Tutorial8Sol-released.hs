-- Remember: there are many possible solutions, and if your solution produces
-- the right results, then it is (most likely) correct. However, if your code
-- looks far more complicated than these sample solutions, then you're probably
-- making things too difficult for yourself---try to keep it simple!

{-# LANGUAGE TemplateHaskell #-}
module Tutorial8Sol where  

import System.Random
import Test.QuickCheck

-- Importing the keymap module

-- import KeymapList
import KeymapTreeSol


-- Type declarations

type Barcode = String
type Product = String
type Unit    = String

type Item    = (Product,Unit)

type Catalogue = Keymap Barcode Item


-- A little test catalog

testDB :: Catalogue
testDB = fromList [
 ("0265090316581", ("The Macannihav'nmor Highland Single Malt", "75ml bottle")),
 ("0903900739533", ("Bagpipes of Glory", "6-CD Box")),
 ("9780201342758", ("Thompson - \"Haskell: The Craft of Functional Programming\"", "Book")),
 ("0042400212509", ("Universal deep-frying pan", "pc"))
 ]

-- Exercise 1

catMaybes :: [Maybe a] -> [a]
catMaybes = concat . map maybeToList
  where
  maybeToList :: Maybe a -> [a]
  maybeToList Nothing  = []
  maybeToList (Just x) = [x]

catMaybes' :: [Maybe a] -> [a]
catMaybes' xs  =  [ x | Just x <- xs ]

prop_catMaybes :: [Maybe Int] -> Bool
prop_catMaybes xs  =  catMaybes xs == catMaybes' xs

getItems :: [Barcode] -> Catalogue -> [Item]
getItems keys db = catMaybes [ get k db | k <- keys ]

prop_items :: Bool
prop_items =  getItems ["0001","9780201342758","0003"] testDB ==
                [("Thompson - \"Haskell: The Craft of Functional Programming\"","Book")]

-- Exercise 2

{-
*Tutorial8Sol> db <- readDB
Done
(2.85 secs, 2,481,415,928 bytes)
*Tutorial8Sol> size db
104651
(0.03 secs, 90,384 bytes)
*Tutorial8Sol> ks <- samples 1000 db
(0.39 secs, 9,680,064 bytes)
*Tutorial8Sol> force (getItems ks db)
()
(6.33 secs, 581,104 bytes)

Doubling the database size would double the time.
-}

-- for Exercises 3--6 check KeymapTree.hs 

-- Exercise 7

{-
*Tutorial8Sol> db <- readDB
Done
(6.32 secs, 3,577,770,608 bytes)
*Tutorial8Sol> ks <- loadKeys
(0.00 secs, 103,440 bytes)
*Tutorial8Sol> force (getItems ks db)
()
(0.06 secs, 28,080,776 bytes)

Doubling the database size would increase the time by 7%.
(Computed as log 20000 / log 10000.)
-}

-- for Exercises 8--10 check KeymapTree.hs 


-- ** Input-output

readDB :: IO Catalogue
readDB = do dbl <- readFile "database.csv"
            let db = fromList (map readLine (lines dbl))
            putStrLn (force (show db) `seq` "Done")
            return db

readLine :: String -> (Barcode,Item)
readLine str = (a,(c,b))
    where
      (a,str2) = splitUpon ',' str
      (b,c)    = splitUpon ',' str2

splitUpon :: Char -> String -> (String,String)
splitUpon _ "" = ("","")
splitUpon c (x:xs) | x == c    = ("",xs)
                   | otherwise = (x:ys,zs)
                   where
                     (ys,zs) = splitUpon c xs

samples :: Int -> Catalogue -> IO [Barcode]
samples n db =
  do g <- newStdGen
     let allKeys = [ key | (key,item) <- toList db ]
     let indices = randomRs (0, length allKeys - 1) g
     let keys = take n [ allKeys !! i | i <- indices ]
     saveKeys keys
     return (force keys `seq` keys)

saveKeys :: [Barcode] -> IO ()
saveKeys = writeFile "keys.cache" . show

loadKeys :: IO [Barcode]
loadKeys = do
  keys <- read <$> readFile "keys.cache"
  return (force keys `seq` keys)

force :: [a] -> ()
force = foldr seq ()

-- ** Automatically run QuickCheck properties named 'prop_*'.
return []

main :: IO ()
main = do
  qc <- $quickCheckAll
  let showB b = if b then "Pass" else "Fail"
  putStrLn ("QuickCheck: " ++ showB qc)
