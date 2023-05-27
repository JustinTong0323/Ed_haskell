module PropSol where

import Control.Monad
import MyParser
import Test.QuickCheck

data Prop   = X
            | Y
            | T
            | F
            | Not Prop
            | Prop :&&: Prop
            | Prop :||: Prop
            | Prop :->: Prop
  deriving (Eq)

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

par :: String -> String
par s  =  "(" ++ s ++ ")"

instance Show Prop where
  show X            =  "X"
  show Y            =  "Y"
  show F            =  "0"
  show T            =  "1"
  show (Not p)      =  par ("not " ++ show p)
  show (p :||: q)   =  par (show p ++ " || " ++ show q)
  show (p :&&: q)   =  par (show p ++ " && " ++ show q)
  show (p :->: q)   =  par (show p ++ " -> " ++ show q)

-- ** Optional Exercises

-- Exercise 1

parseProp :: Parser Prop
parseProp  =  parseX `mplus` parseY `mplus` parseF `mplus` parseT `mplus`
             parseNot  `mplus` parseOr `mplus` parseAnd `mplus` parseImp
  where
  parseX    =  do { token 'X'; return X }
  parseY    =  do { token 'Y'; return Y }
  parseF    =  do { token '0'; return F }
  parseT    =  do { token '1'; return T }
  parseNot  =  do { token '(';
                    match "not ";
                    p <- parseProp;
                    token ')';
                    return (Not p) }
  parseOr   = do { token '(';
                   p <- parseProp;
                   match " || ";
                   q <- parseProp;
                   token ')';
                   return (p :||: q) }
  parseAnd  = do { token '(';
                   p <- parseProp;
                   match " && ";
                   q <- parseProp;
                   token ')';
                   return (p :&&: q) }
  parseImp  = do { token '(';
                   p <- parseProp;
                   match " -> ";
                   q <- parseProp;
                   token ')';
                   return (p :->: q) }

-- Exercise 2

prop_roundtrip :: Prop -> Bool
prop_roundtrip prop  =  (parse parseProp (show prop) == prop)

main :: IO ()
main = quickCheck prop_roundtrip
