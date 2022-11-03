module Tutorial7 where

import LSystem
import Test.QuickCheck

import Data.List

pathExample = Go 30 :#: Turn 120 :#: Go 30 :#: Turn 120 :#: Go 30

-- 1a. copy
copy :: Int -> Command -> Command
copy 0 _ = Sit
copy n c = c :#: copy (n-1) c

-- 1b. polygon
polygon :: Distance -> Int -> Command
polygon d s =  copy s (Go d :#: Turn angle)
    where angle = fromIntegral (360 `div` s)

ex = let inDirection angle = Branch (Turn angle :#: Go 100) in join (map inDirection [20,40..360])

-- 2. snowflake
snowflake :: Int -> Command
snowflake x = f x :#: n :#: n :#: f x :#: n :#: n :#: f x :#: n :#: n
    where
        f 0 = GrabPen (Colour 1.0 0.5 0.5) :#: Go 10
        f x = f(x-1) :#: p :#: f(x-1) :#: n :#: n :#: f(x-1) :#: p :#: f(x-1)
        p = Turn (-60)
        n = Turn 60

-- 3. sierpinski
sierpinski :: Int -> Command
sierpinski = f
    where
        f 0 = GrabPen (Colour 1.0 0.5 0.5) :#: Go 10
        f x = g(x-1) :#: p :#: f(x-1) :#: p :#: g(x-1)
        g 0 = GrabPen (Colour 1.0 0.5 0.5) :#: Go 10
        g x = f(x-1) :#: n :#: g(x-1) :#: n :#: f(x-1)
        p = Turn (-60)
        n = Turn 60

-- 4. hilbert
hilbert :: Int -> Command
hilbert =  l
    where
        l 0 = GrabPen (Colour 1.0 0.5 0.5) :#: Go 10
        l x = p :#: r(x-1) :#: n :#: l(x-1) :#: l(x-1) :#: n :#: r(x-1) :#: p
        r 0 = GrabPen (Colour 1.0 0.5 0.5) :#: Go 10
        r x = n :#: l(x-1) :#: p :#: r(x-1) :#: r(x-1) :#: p :#: l(x-1) :#: n
        p = Turn (-90)
        n = Turn 90

-- 5. dragon
dragon :: Int -> Command
dragon =  l
    where
        l 0 = GrabPen (Colour 1.0 0.5 0.5) :#: Go 10
        l x = l(x-1) :#: p :#: r(x-1) :#: p
        r 0 = GrabPen (Colour 1.0 0.5 0.5) :#: Go 10
        r x = n :#: l(x-1) :#: n :#: r(x-1)
        p = Turn (-90)
        n = Turn 90

-- ** Optional Material

-- 6a. split
split :: Command -> [Command]
split (a :#: b) =  split a ++ split b
split c = [c | c /= Sit]

-- 6b. join
join :: [Command] -> Command
join = foldr (:#:) Sit

-- Auxiliary function: simplify the command
simplifyCommand :: Command -> Command
simplifyCommand (p :#: (q :#: r)) = simplifyCommand p :#: simplifyCommand q :#: simplifyCommand r
simplifyCommand (p :#: Sit) = simplifyCommand p
simplifyCommand (Sit :#: p) = simplifyCommand p
simplifyCommand (p :#: q) = simplifyCommand p :#: simplifyCommand q
simplifyCommand p = p


-- 6c. equivalent
equivalent :: Command -> Command -> Bool
equivalent a b =  split a == split b

-- 6d. testing join and split
prop_split_join :: Command -> Bool
prop_split_join c = equivalent (join (split c)) c


prop_split :: Command -> Bool
prop_split c =  not ((Sit `elem` cl) && any isCmds cl)
    where
        cl = split c

---- Auxiliary function
isCmds :: Command -> Bool
isCmds (a :#: b) = True
isCmds a = False


-- 7. optimise
optimise :: Command -> Command
optimise c =  join' (split c \\ [Sit, Go 0, Turn 0])

-- Auxiliary functions

join' :: [Command] -> Command
join' = foldr1 (:#:)