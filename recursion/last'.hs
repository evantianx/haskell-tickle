last' :: [a] -> a
last' [] = error "This is an empty list"
last' [x] = x
last' (_:xs) = last' xs
