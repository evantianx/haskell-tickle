merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x > y = y:merge (x:xs) ys
                    | otherwise = x:merge xs (y:ys)
                    
mergesort :: Ord a => [a] -> [a]
mergesort xs = merge (mergesort x1) (mergesort x2)
  where
    (x1, x2) = halve xs
    halve xs = (take l xs, drop l xs)
    l = (length xs) `div` 2
