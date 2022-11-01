module Tree
(Set(Nil,Node),empty,insert,set,element,equal) where

import Test.QuickCheck

data Set a = Nil | Node (Set a) a (Set a)
    deriving (Eq, Show)


t1 :: Set Int
t1 = Node (Node Nil 1 (Node Nil 0 Nil)) 2 (Node Nil 4 (Node Nil 3 Nil))

list :: Set a -> [a]
list Nil = []
list (Node l x r) = list l ++ [x] ++ list r

invariant :: Ord a => Set a -> Bool
invariant Nil = True
invariant (Node l x r) =
    invariant l && invariant r &&
    and [ y < x | y <- list l ] &&
    and [ y > x | y <- list r ]

empty :: Set a
empty = Nil

insert :: Ord a => a -> Set a -> Set a
insert x Nil = Node Nil x Nil
insert x (Node l y r)
    | x == y = Node l y r
    | x < y = Node (insert x l) y r
    | x > y = Node l y (insert x r)

set :: Ord a => [a] -> Set a
set = foldr insert empty

element :: Ord a => a -> Set a -> Bool
x `element` Nil = False
x `element` (Node l y r)
    | x == y = True
    | x < y = x `element` l
    | x > y = x `element` r
    
equal :: Ord a => Set a -> Set a -> Bool
s `equal` t = list s == list t

prop_invariant :: [Int] -> Bool
prop_invariant xs = invariant s
    where s = set xs

prop_element :: [Int] -> Bool
prop_element ys = and [ x `element` s == odd x | x <- ys ]
    where s = set [ x | x <- ys, odd x ]
check =
    quickCheck prop_invariant >>
    quickCheck prop_element
-- Prelude Tree> check
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
