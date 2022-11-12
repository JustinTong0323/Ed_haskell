module Tutorial8 where  

import System.Random
-- :set -package random
import Test.QuickCheck

-- Importing the keymap module

-- import KeymapList
import KeymapTree

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

getItems :: [Barcode] -> Catalogue -> [Item]
getItems [] _ = []
getItems (k:ks) c = let mi = get k c in 
  if mi == Nothing then getItems ks c else 
    let Just i = mi in i : getItems ks c

-- Exercise 2

{-
*Tutorial8> db <- readDB
Done
(2.41 secs, 2,254,810,600 bytes)
*Tutorial8> size db
104651
(0.01 secs, 60,312 bytes)
*Tutorial8> ks <- samples 1000 db
(0.20 secs, 9,155,040 bytes)
*Tutorial8> force (getItems ks db)
()
(4.78 secs, 686,056 bytes)

If the database was two times bigger,
how would you expect the time to change?
The time would also double as the time increase linearly.
-}

-- for Exercises 3--6 check KeymapTree.hs 

-- Exercise 7

{-
*Tutorial8> db <- readDB
Done
(6.12 secs, 3,339,891,784 bytes)
*Tutorial8> size db
104651
(0.07 secs, 27,688,168 bytes)
*Tutorial8> depth db
40
(0.09 secs, 26,844,528 bytes)
*Tutorial8> ks <- loadKeys
(0.00 secs, 69,592 bytes)
*Tutorial8> force (getItems ks db)
()
(0.04 secs, 25,716,152 bytes)

If the database was two times bigger,
how would you expect the time to change?
The time could be less than double but it depends on the order of the keys.
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
