module Tutorial10 where

import Test.QuickCheck
import Control.Monad
import Data.Char

-- Question 1

-- 1a

ok :: String -> Bool
ok [] = False
ok s = all isLower s && length s < 6

-- 1b

f :: [String] -> String
f l = if null oklist then "zzzzz" else minimum oklist
  where oklist = [ x | x <- l, ok x]

-- 1c

g :: [String] -> String
g [] = "zzzzz"
g (x:xs)
  | ok x = min x (g xs)
  | otherwise = g xs

-- 1d

h :: [String] -> String
h = foldr min "zzzzz" . filter ok

-- Question 2

-- 2a

i :: [a] -> [a] -> [a]
i a b = tail a ++ [head b]

-- 2b

j :: [[a]] -> [[a]]
j l = [i a b | (a,b) <- zip l (i l l)]

-- 2c

k :: [[a]] -> [[a]]
--k [] = []
k l = k' (l ++ [head l])
  where
    k' :: [[a]] -> [[a]]
    k' [x] = []
    k' (x:xs) = i x (head xs) : k' xs

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
eval x y p = eval' p
  where
    eval' :: Prop -> Bool
    eval' X = x
    eval' Y = y
    eval' T = True
    eval' F = False
    eval' (Not p)     = not (eval' p)
    eval' (p :||: q)  = eval' p || eval' q
    eval' (p :&&: q)  = eval' p && eval' q
    eval' (p :->: q)  = not (eval' p) || eval' q

-- 3b

simple :: Prop -> Bool
simple p = not (((X `elem` vars) || (Y `elem` vars)) && ((T `elem` vars) || (F `elem` vars)))
  where
    vars = l p
    l :: Prop -> [Prop]
    l T = [T]
    l F = [F]
    l X = [X]
    l Y = [Y]
    l (Not p) = l p
    l (p :||: q) = l p ++ l q
    l (p :&&: q) = l p ++ l q
    l (p :->: q) = l p ++ l q

-- 3c

simplify :: Prop -> Prop
simplify (Not T) = F
simplify (Not F) = T
simplify (F :&&: p) = F
simplify (p :&&: F) = F
simplify (T :&&: p) = simplify p
simplify (p :&&: T) = simplify p
simplify (p :&&: q) = if simple (p :&&: q) then p :&&: q else simplify (simplify p :&&: simplify q)
simplify (F :||: p) = simplify p
simplify (p :||: F) = simplify p
simplify (T :||: p) = T
simplify (p :||: T) = T
simplify (p :||: q) = if simple (p :||: q) then p :||: q else simplify (simplify p :||: simplify q)
simplify (F :->: p) = T
simplify (p :->: T) = T
simplify (T :->: p) = simplify p
simplify (p :->: F) = Not (simplify p)
simplify (p :->: q) = if simple (p :->: q) then p :->: q else simplify (simplify p :->: simplify q)
simplify p = p
