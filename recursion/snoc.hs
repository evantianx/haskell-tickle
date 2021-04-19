-- snoc: put element at the last position in a list
snoc :: a -> [a] -> [a]
snoc x [] = [x]
snoc x (y:ys) = y : snoc x ys
