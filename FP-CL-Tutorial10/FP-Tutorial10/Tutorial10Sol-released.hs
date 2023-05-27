{-# LANGUAGE TemplateHaskell #-}
module Tutorial10Sol where

import Test.QuickCheck
import Control.Monad
import Data.Char

-- Question 1

-- 1a

ok :: String -> Bool
ok c = all isLower c && length c < 6

prop_1a =
  ok "a" == True &&
  ok "zzzzz" == True &&
  ok "Short" == False &&
  ok "longer" == False &&
  ok "???" == False

-- 1b

f :: [String] -> String
f cs = minimum ("zzzzz" : [ c | c <- cs, ok c ])

prop_1b =
  f ["a","bb","ccc","dddd","eeeee","ffffff"] == "a" &&
  f ["uuuuuu","vvvvv","wwww","xxx","yy","z"] == "vvvvv" &&
  f ["Short","longer","???"] == "zzzzz"

-- 1c

g :: [String] -> String
g []                 = "zzzzz"
g (c:cs) | ok c      = c `min` g cs
         | otherwise = g cs

prop_1c =
  g ["a","bb","ccc","dddd","eeeee","ffffff"] == "a" &&
  g ["uuuuuu","vvvvv","wwww","xxx","yy","z"] == "vvvvv" &&
  g ["Short","longer","???"] == "zzzzz"

-- 1d

h :: [String] -> String
h = foldr min "zzzzz" . filter ok

prop_1d =
  h ["a","bb","ccc","dddd","eeeee","ffffff"] == "a" &&
  h ["uuuuuu","vvvvv","wwww","xxx","yy","z"] == "vvvvv" &&
  h ["Short","longer","???"] == "zzzzz"

prop_fgh :: [String] -> Bool
prop_fgh cs = f cs == g cs && g cs == h cs

-- Question 2

-- 2a

i :: [a] -> [a] -> [a]
i xs ys = tail xs ++ [head ys]

prop_2a =
  i "abc" "def" == "bcd" &&
  i "def" "ghi" == "efg" &&
  i "ghi" "abc" == "hia"

-- 2b

j :: [[a]] -> [[a]]
j xss = [ i xs ys | (xs,ys) <- zip xss (i xss xss) ]

prop_2b =
  j ["abc","def","ghi"] == ["bcd","efg","hia"] &&
  j ["once","upon","a","time"] == ["nceu","pona","t","imeo"] &&
  j ["a","b","c"] == ["b","c","a"] &&
  j ["abc"] == ["bca"] &&
  j ["a"] == ["a"]

-- 2c

k :: [[a]] -> [[a]]
k (xs:xss) = l xs xss xs
  where
  l xs [] zs       = [i xs zs]
  l xs (ys:yss) zs = i xs ys : l ys yss zs

prop_2c =
  k ["abc","def","ghi"] == ["bcd","efg","hia"] &&
  k ["once","upon","a","time"] == ["nceu","pona","t","imeo"] &&
  k ["a","b","c"] == ["b","c","a"] &&
  k ["abc"] == ["bca"] &&
  k ["a"] == ["a"]

nn :: [a] -> Bool
nn = not . null

prop_j :: [String] -> Property
prop_j xss  =
  nn xss && all nn xss ==>
    i (concat xss) (concat xss) == concat (j xss)

prop_jk :: [String] -> Property
prop_jk xss  =
  nn xss && all nn xss ==>
    j xss == k xss
    
-- Question 3

data Prop = X
          | Y
          | T
          | F
          | Not Prop
          | Prop :&&: Prop
          | Prop :||: Prop
          | Prop :->: Prop
  deriving (Eq, Show)

instance Arbitrary Prop where
  arbitrary = sized gen
    where
    gen 0 =
      oneof [ return X,
              return Y,
              return T,
              return F ]
    gen n | n>0 =
      oneof [ return X,
              return Y,
              return T,
              return F,
              liftM Not prop,
              liftM2 (:&&:) prop prop,
              liftM2 (:||:) prop prop,
              liftM2 (:->:) prop prop]
      where
      prop = gen (n `div` 2)

-- 3a

eval :: Bool -> Bool -> Prop -> Bool
eval x y X          = x
eval x y Y          = y
eval x y T          = True
eval x y F          = False
eval x y (Not p)    = not (eval x y p)
eval x y (p :&&: q) = eval x y p && eval x y q
eval x y (p :||: q) = eval x y p || eval x y q
eval x y (p :->: q) = eval x y p <= eval x y q

prop_3a
  =  eval False False ((X :->: Y) :&&: (Not Y :||: X)) == True
  && eval False True  ((X :->: Y) :&&: (Not Y :||: X)) == False
  && eval True  False ((X :->: Y) :&&: (Not Y :||: X)) == False
  && eval True  True  ((X :->: Y) :&&: (Not Y :||: X)) == True

-- 3b

simple :: Prop -> Bool
simple T  = True
simple F  = True
simple p  = check p
  where
  check :: Prop -> Bool
  check X          = True
  check Y          = True
  check T          = False
  check F          = False
  check (Not p)    = check p
  check (p :&&: q) = check p && check q
  check (p :||: q) = check p && check q
  check (p :->: q) = check p && check q

prop_3b
  =  simple T                            == True
  && simple F                            == True
  && simple ((T :||: X) :->: (T :&&: Y)) == False
  && simple ((X :||: F) :->: (Y :&&: F)) == False
  && simple ((X :&&: Y) :->: (X :||: Y)) == True

-- 3c

simplify :: Prop -> Prop
simplify X       = X
simplify Y       = Y
simplify T       = T
simplify F       = F
simplify (Not p) = snot (simplify p)
  where
  snot T = F
  snot F = T
  snot p = Not p
simplify (p :&&: q) = simplify p `sand` simplify q
  where
  T `sand` p = p
  F `sand` p = F
  p `sand` T = p
  p `sand` F = F
  p `sand` q = p :&&: q
simplify (p :||: q) = simplify p `sor` simplify q
  where
  T `sor` p = T
  F `sor` p = p
  p `sor` T = T
  p `sor` F = p
  p `sor` q = p :||: q
simplify (p :->: q) = simplify p `simp` simplify q
  where
  T `simp` p = p
  F `simp` p = T
  p `simp` T = T
  p `simp` F = Not p
  p `simp` q = p :->: q

prop_3c :: Bool
prop_3c
  =  simplify T                            == T
  && simplify F                            == F
  && simplify ((T :||: X) :->: (T :&&: Y)) == Y
  && simplify ((X :||: F) :->: (X :&&: F)) == Not X
  && simplify ((X :||: Y) :->: (X :&&: Y)) == ((X :||: Y) :->: (X :&&: Y))

prop_simplify :: Bool -> Bool -> Prop -> Bool
prop_simplify x y p  =
  simple (simplify p) &&
  simplify (simplify p) == simplify p &&
  eval x y p == eval x y (simplify p)

-- ** Automatically run QuickCheck properties named 'prop_*' and any other tests.
return []

main :: IO ()
main = do
  qc <- $quickCheckAll
  let showB b = if b then "Pass" else "Fail"
  putStrLn $ "QuickCheck: " ++ showB qc
