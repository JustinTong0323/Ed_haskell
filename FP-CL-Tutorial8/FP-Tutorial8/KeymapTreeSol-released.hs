-- Remember: there are many possible solutions, and if your solution produces
-- the right results, then it is (most likely) correct. However, if your code
-- looks far more complicated than these sample solutions, then you're probably
-- making things too difficult for yourself---try to keep it simple!

{-# LANGUAGE TemplateHaskell #-}
module KeymapTreeSol ( Keymap,
                       invariant, keys,
                       size, depth, get, set, select,
                       toList, fromList,
                       filterLT, filterGT, merge,
                       -- for autotest
                       prop_get, prop_toList, prop_fromList,
                       prop_set_get, prop_toList_fromList,
                       testTree
                     )

where

-- Modules for testing
  
import Test.QuickCheck
import Control.Monad
import Data.List
  
-- The data type

data Keymap k a = Leaf
                | Node k a (Keymap k a) (Keymap k a)
                deriving (Eq, Show)

-- A test tree

testTree :: Keymap Int Int
testTree = Node 2 20 (Node 1 10 Leaf Leaf)
                     (Node 3 30 Leaf 
                               (Node 4 40 Leaf Leaf ))

-- Invariant

invariant :: Ord k => Keymap k a -> Bool
invariant Leaf  =  True
invariant (Node k _ left right)  =  all (< k) (keys left) &&
                                    all (> k) (keys right) &&
                                    invariant left &&
                                    invariant right

keys :: Keymap k a -> [k]
keys Leaf  =  []
keys (Node k _ left right)  =  keys left ++ [k] ++ keys right

size :: Keymap k a -> Int
size Leaf = 0
size (Node _ _ left right) = 1 + size left + size right

depth :: Keymap k a -> Int
depth Leaf = 0
depth (Node _ _ left right) = 1 + (depth left `max` depth right)

-- Exercise 3

toList :: Keymap k a -> [(k,a)]
toList Leaf = []
toList (Node k x left right) = toList left ++ [(k,x)] ++ toList right

prop_toList :: Bool
prop_toList =  toList testTree == [(1,10),(2,20),(3,30),(4,40)]

-- Exercise 4

set :: Ord k => k -> a -> Keymap k a -> Keymap k a
set key value = go
    where go Leaf = Node key value Leaf Leaf
          go (Node k v left right) | key == k = Node k value left right
                                   | key < k  = Node k v (go left) right
                                   | key > k  = Node k v left (go right)

-- Exercise 5

get :: Ord k => k -> Keymap k a -> Maybe a
get key = go
    where go Leaf = Nothing
          go (Node k v l r) | key == k = Just v
                            | key < k  = go l
                            | key > k  = go r

prop_get :: Bool
prop_get =
  get 3 testTree == Just 30 &&
  get 6 testTree == Nothing

prop_set_get :: Int -> String -> Keymap Int String -> Bool
prop_set_get k v db = get k (set k v db) == Just v

-- Exercise 6

fromList :: Ord k => [(k,a)] -> Keymap k a
fromList [] = Leaf
fromList ((k,v):xs) = set k v (fromList xs)

-- Alternative higer-order solution:
fromList' :: Ord k => [(k,a)] -> Keymap k a
fromList' = foldr (uncurry set) Leaf

prop_fromList :: Bool
prop_fromList =
  fromList [(4,40),(3,30),(1,10),(2,20)] == testTree  

prop_fromList' :: [Int] -> [String] -> Bool
prop_fromList' xs ys = fromList zs == fromList' zs
  where zs = zip (nub xs) ys  

prop_toList_fromList :: [Int] -> [String] -> Bool
prop_toList_fromList xs ys  =  toList (fromList zs) == sort zs
  where zs = zip (nub xs) ys


-- ** Optional Material

-- Exercise 8

filterLT :: Ord k => k -> Keymap k a -> Keymap k a
filterLT _ Leaf = Leaf
filterLT x (Node k v l r) | k == x = l
                          | k < x  = Node k v l (filterLT x r)
                          | k > x  = filterLT x l
                                     
filterGT :: Ord k => k -> Keymap k a -> Keymap k a
filterGT _ Leaf = Leaf
filterGT x (Node k v l r) | k == x = r
                          | k < x  = filterGT x r
                          | k > x  = Node k v (filterGT x l) r

prop_filter :: Bool
prop_filter =
    keys (filterLT "0900000000000" testDB) == ["0042400212509","0265090316581"] &&
    keys (filterLT "0" testDB) == [] &&
    keys (filterGT "0900000000000" testDB) == ["0903900739533","9780201342758"] &&
    keys (filterGT "0" testDB) ==
              ["0042400212509","0265090316581","0903900739533","9780201342758"]
  where
  testDB = fromList [
   ("0265090316581", ("The Macannihav'nmor Highland Single Malt", "75ml bottle")),
   ("0903900739533", ("Bagpipes of Glory", "6-CD Box")),
   ("9780201342758", ("Thompson - \"Haskell: The Craft of Functional Programming\"", "Book")),
   ("0042400212509", ("Universal deep-frying pan", "pc"))
   ]

-- Exercise 9
                                     
merge :: Ord k => Keymap k a -> Keymap k a -> Keymap k a
merge Leaf t = t
merge t Leaf = t
merge (Node k v l r) x'@(Node k' _ l' r')
  | k == k'  = Node k v (l `merge` l') (r `merge` r')
  | otherwise = Node k v (l `merge` filterLT k x') (r `merge` filterGT k x')

merge' :: Ord k => Keymap k a -> Keymap k a -> Keymap k a
merge' Leaf t = t
merge' t Leaf = t
merge' (Node k v l r) t = Node k v (l `merge` filterLT k t)
                                   (r `merge` filterGT k t)


                
prop_merge :: Keymap Int Int -> Keymap Int Int -> Bool                
prop_merge t1 t2 = sort (nubBy p (toList t1 ++ toList t2)) == toList (t1 `merge` t2)
  where p (k,_) (k',_) = k == k'

prop_merge' :: Keymap Int Int -> Keymap Int Int -> Bool                
prop_merge' t1 t2 = merge t1 t2 == merge' t1 t2  

-- Exercise 10

select :: Ord k => (a -> Bool) -> Keymap k a -> Keymap k a
select f = go
  where go Leaf = Leaf
        go (Node k v l r) | f v       = Node k v (go l) (go r)
                          | otherwise = go l `merge` go r

-- Instances for QuickCheck -----------------------------

instance (Ord k, Arbitrary k, Arbitrary a) => Arbitrary (Keymap k a) where
    arbitrary = liftM fromList (liftM2 zip (liftM nub arbitrary) arbitrary)


-- ** Automatically run QuickCheck properties named 'prop_*' and any other tests.
return []

main :: IO ()
main = do
  qc <- $quickCheckAll
  let showB b = if b then "Pass" else "Fail"
  putStrLn ("QuickCheck: " ++ showB qc)
