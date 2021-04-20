filtersplit :: (a -> Bool) -> [a] -> ([a], [a])
filtersplit _ [] = ([], [])
filtersplit f (x:xs) | f x = ((x:l), r)
                     | otherwise = (l, (x:r))
  where (l, r) = filtersplit f xs
  
quicksort :: Ord a => a -> [a] -> [a]
quicksort [] = []
quicksort [x] = [x]
quicksort (x:xs) = quicksort l ++ [x] ++ quicksort r
  where (l, r) = filtersort (<x) xs
