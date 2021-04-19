gcd' :: Int -> Int -> Int
gcd' x y = if y == 0 then x else gcd' y (mod x y)
