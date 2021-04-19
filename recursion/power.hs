-- simple but slow solution
power :: Int -> Int -> Int
power 0 0 = 1
power _ 0 = 1
power x n = x * power x (n-1)


-- optimized version:
-- n is even: x ^ 2n = x ^ n * x ^ n
-- n is odd:  x ^ (2n+1) = x ^ n * x ^ n * x
power' :: Int -> Int -> Int
power' 0 0 = 1
power' _ 0 = 1
power' x n | odd n = let p = power' x ((n-1) `div` 2) in x * p * p
           | otherwise = let p = power' x (n `div` 2) in p * p
