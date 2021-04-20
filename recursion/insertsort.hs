insert :: (Ord a) => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x <= y = x:y:ys
                | otherwise = y:insert x ys

insertSort :: Ord a => [a] -> [a] -> [a]
insertSort xs [] = xs
insertSort xs (y:ys) = insertSort (insert y xs) ys
