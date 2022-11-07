module Sort where

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

