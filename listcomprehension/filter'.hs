filter' :: (a -> b) -> [a] -> [b]
filter' f xs = [x | x <- xs, f x]
