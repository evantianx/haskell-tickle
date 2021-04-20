delete :: Eq a => a -> [a] -> [a]
delete _ [] = []
delete x (y:ys) | x == y = ys
                | otherwise = y:delete x ys
                
selectionsort :: Ord a => [a] -> [a]
selectionsort [] = []
selectionsort xs = mini : selectionsort xs'
  where mini = minimum xs
        xs' = delete mini xs
