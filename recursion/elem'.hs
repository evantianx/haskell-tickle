elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs) = if a == x then True else elem' a xs
