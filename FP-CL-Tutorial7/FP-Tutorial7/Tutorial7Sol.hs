-- Remember: there are many possible solutions, and if your solution produces
-- the right results, then it is (most likely) correct. However, if your code
-- looks far more complicated than these sample solutions, then you're probably
-- making things too difficult for yourself---try to keep it simple!
{-# LANGUAGE TemplateHaskell #-}
module Tutorial7Sol where
  
import LSystem
import Test.QuickCheck

pathExample = Go 30 :#: Turn 120 :#: Go 30 :#: Turn 120 :#: Go 30

-- 1a. copy
copy :: Int -> Command -> Command
copy n cmd  |  n == 0    = Sit
            |  otherwise = cmd :#: copy (n-1) cmd

-- alternative copy (using join, replicate)
copy' :: Int -> Command -> Command
copy' n cmd = foldr (:#:) Sit (replicate n cmd)

prop_copy :: Bool
prop_copy =
   copy 3 (Go 10 :#: Turn 120) ==
   ((Go 10 :#: Turn 120) :#:
    (Go 10 :#: Turn 120) :#:
    (Go 10 :#: Turn 120) :#:
    Sit)
   
prop_copy' :: Int -> Command -> Property
prop_copy' n cmd  =  (n >= 0) ==> copy n cmd == copy' n cmd
   
-- 1b. polygon
polygon :: Distance -> Int -> Command
polygon side nsides = 
    copy nsides (Go side :#: Turn angle)
        where angle = 360 / (fromIntegral nsides)

prop_polygon :: Bool
prop_polygon =
    polygon 50 5 ==
    ((Go 50.0 :#: Turn 72.0) :#:
     (Go 50.0 :#: Turn 72.0) :#:
     (Go 50.0 :#: Turn 72.0) :#:
     (Go 50.0 :#: Turn 72.0) :#:
     (Go 50.0 :#: Turn 72.0) :#:
     Sit)

-- L-Systems
-- 2. snowflake
snowflake :: Int -> Command
snowflake x = f x :#: n :#: n :#: f x :#: n :#: n :#: f x :#: n :#: n 
    where
      f 0 = Go 10
      f x = f (x-1) :#: p :#: f (x-1) :#: n :#: n :#: f (x-1):#: p :#: f (x-1)
      n = Turn 60
      p = Turn(-60)

-- 3. sierpinski
sierpinski :: Int -> Command
sierpinski x = f x
    where
      f 0 = GrabPen red :#: Go 10
      f x = g (x-1) :#: p :#: f (x-1) :#: p :#: g (x-1)
      g 0 = GrabPen blue :#: Go 10
      g x = f (x-1) :#: n :#: g (x-1) :#: n :#: f (x-1)
      n = Turn 60
      p = Turn(-60)
     
-- 4. hilbert
hilbert :: Int -> Command
hilbert x = l x
    where
      l 0 = Sit 
      l x = p :#: r (x-1) :#: f :#: n :#: l (x-1) :#: f :#: l (x-1) :#: n :#: f :#: r (x-1) :#: p
      r 0 = Sit 
      r x = n :#: l (x-1) :#: f :#: p :#: r (x-1) :#: f :#: r (x-1) :#: p :#: f :#: l (x-1) :#: n
      f = GrabPen black :#: Go 10
      n = Turn 90
      p = Turn(-90)

-- 5. dragon
dragon :: Int -> Command
dragon x  =  Go 10 :#: l x
    where
      l 0  =  Sit
      l x  =  l (x-1) :#: p :#: r (x-1) :#: f :#: p
      r 0  =  Sit
      r x  =  n :#: f :#: l (x-1) :#: n :#: r (x-1)
      f  =  Go 10
      n  =  Turn 90
      p  =  Turn (-90)

-- ** Optional Material

-- 6a. split
split :: Command -> [Command]
split Sit              =  []
split (cmd1 :#: cmd2)  =  split cmd1 ++ split cmd2
split cmd              =  [cmd]

-- 6b. join
join :: [Command] -> Command
join = foldr (:#:) Sit

-- alternative joins (optimised to avoid extra Sit)
join' :: [Command] -> Command
join' []     = Sit
join' [x]    = x
join' (x:xs) = x :#: join' xs

join'' :: [Command] -> Command
join'' [] = Sit 
join'' xs = foldr1 (:#:) xs

prop_join xs = join' xs == join'' xs

-- 6c. equivalent
equivalent :: Command -> Command -> Bool
equivalent cmd1 cmd2 = split cmd1 == split cmd2

-- 6d. testing join and split
prop_split_join :: Command -> Bool
prop_split_join cmd = equivalent (join (split cmd)) cmd 

prop_split :: Command -> Bool
prop_split cmd = all p (split cmd)
    where
      p Sit       = False
      p (_ :#: _) = False
      p _         = True

-- 7. optimise

-- old solution (five passes)

optimise :: Command -> Command
optimise = join' . compress . filter (/= Turn 0) . compress . filter (/= Go 0) . split
    where
      compress :: [Command] -> [Command]
      compress [] = []
      compress (Turn x : Turn y : cs) = compress (Turn (x+y) : cs)
      compress (Go x : Go y : cs) = compress (Go (x+y) : cs)
      compress (x:cs) = x : compress cs

-- fast split (linear time, original is quadratic in worst case)

split' :: Command -> [Command]
split' x  =  helper x []
  where
  helper :: Command -> [Command] -> [Command]
  helper Sit cs        =  cs
  helper (x :#: y) cs  =  helper x (helper y cs)
  helper x cs          =  x : cs

{-
This uses a standard trick to speed up code with append:
build append into the computation. Define
  helper x cs  =  split x ++ cs
Then compute its recursive definition as follows.

     helper Sit cs
  =  split Sit ++ cs
  =  [] ++ cs
  =  cs

     helper (x :#: y) cs
  =  split (x :#: y) ++ cs
  =  (split x ++ split y) ++ cs    (*)
  =  split x ++ (split y ++ cs)
  =  helper x (helper y cs)

     helper x cs  (x == Turn a  or  x == Go a)
  =  split x ++ cs
  =  [x] ++ cs
  =  x : cs

The use of associativity at line (*) is responsible
for the speed-up.
-}

-- Liam's solution (single pass)

optimiseList :: Command -> Command
optimiseList =  join' . foldr add [] . split'

add :: Command -> [Command] -> [Command]
add (Turn a) (Turn b : cs)             =  turn (a + b) cs
add (Turn a) cs                        =  turn a cs
add (Go a) (Go b : cs)                 =  go (a + b) cs
add (Go a) cs                          =  go a cs

go :: Distance -> [Command] -> [Command]
go 0 cs  =  cs
go a cs  =  Go a : cs

turn :: Angle -> [Command] -> [Command]
turn 0 cs  =  cs
turn a cs  =  Turn a : cs

-- Phil's solution
-- inspired by a question from Oishani Dutta

optimiseDirect :: Command -> Command
optimiseDirect x =  next (step x)
  where
  next Nothing   =  x
  next (Just y)  =  optimiseDirect y

step :: Command -> Maybe Command
step (Go 0) = Just Sit
step (Go a :#: Go b) = Just (Go (a + b))
step (Turn 0) = Just Sit
step (Turn a :#: Turn b) = Just (Turn (a + b))
step (x :#: Sit) = Just x
step (Sit :#: x) = Just x
step ((x :#: y) :#: z) = Just (x :#: (y :#: z))
step (x :#: y) | Just x' <- step x =  Just (x' :#: y)
step (x :#: y) | Just y' <- step y =  Just (x :#: y')
step (x :#: (y :#: z)) | Just xy' <- step (x :#: y) = Just (xy' :#: z)
step _ = Nothing

-- Approximate equality (useful for floats)

class ApEq a where
  (~~) :: a -> a -> Bool

instance ApEq Float where
  a ~~ b  =  abs (a - b) < epsilon  
    where epsilon = 0.0001

instance ApEq Command where
  Sit ~~ Sit              =  True
  Go a ~~ Go b            =  a ~~ b
  Turn a ~~ Turn b        =  a ~~ b
  (u :#: v) ~~ (x :#: y)  =  u ~~ x && v ~~ y

-- Compare solutions

prop_optimise :: Command -> Bool
prop_optimise x  =  optimise x ~~ optimiseList x &&
                    optimiseList x ~~ optimiseDirect x

-- ** Automatically run QuickCheck properties named 'prop_*' and any other tests.
return []

main :: IO ()
main = do
  qc <- $quickCheckAll
  let showB b = if b then "Pass" else "Fail"
  putStrLn $ "QuickCheck: " ++ showB qc
