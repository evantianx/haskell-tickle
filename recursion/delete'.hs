delete' :: Eq a => a -> [a] -> [a]
delete' _ [] = []
delete' a (x:xs) = if a == x then delete' a xs else x:(delete' a xs)
