module Sort where
import Test.QuickCheck

isort :: Ord a => [a] -> [a]
isort = foldr insert []
    where
        insert :: Ord a => a -> [a] -> [a]
        insert x [] = [x]
        insert x (y:ys)
            | x <= y = x : y : ys
            | otherwise = y : insert x ys

qsort :: Ord a => Int -> [a] -> [a]
qsort k xs | length xs <= k = isort xs
qsort k (y:xs) = qsort k (filter (<y) xs) ++ [y] ++ qsort k (filter (>= y) xs)

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in  smallerSorted ++ [x] ++ biggerSorted

prop_quicksort_sorts :: (Ord a) => [a] -> Bool
prop_quicksort_sorts xs = quicksort xs == qsort 5 xs




