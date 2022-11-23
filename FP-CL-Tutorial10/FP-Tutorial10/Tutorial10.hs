module Tutorial10 where

import Test.QuickCheck
import Control.Monad
import Data.Char

-- Question 1

-- 1a

ok :: String -> Bool
ok = undefined

-- 1b

f :: [String] -> String
f = undefined

-- 1c

g :: [String] -> String
g = undefined

-- 1d

h :: [String] -> String
h = undefined

-- Question 2

-- 2a

i :: [a] -> [a] -> [a]
i = undefined

-- 2b

j :: [[a]] -> [[a]]
j = undefined

-- 2c

k :: [[a]] -> [[a]]
k = undefined

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
eval = undefined

-- 3b

simple :: Prop -> Bool
simple = undefined

-- 3c

simplify :: Prop -> Prop
simplify = undefined
