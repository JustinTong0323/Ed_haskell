import Data.Char (toUpper)
echo :: IO ()
echo = do
    line <- getLine;
    if line == "" then
        return ()
    else do
        putStrLn (map toUpper line)
        echo

i :: [a] -> [a] -> [a]
i a b = tail a ++ [head b]

k :: [[a]] -> [[a]]
k l = k' (l ++ [head l])
  where
    k' :: [[a]] -> [[a]]
    k' [x] = []
    k' (x:xs) = i x (head xs) : k' xs
