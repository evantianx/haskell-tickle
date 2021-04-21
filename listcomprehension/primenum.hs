-- too slow..
factors :: Integral a => a -> [a]
factors n = [x | x <- [1..n], mod n x == 0]

isPrime :: Integral a => a -> Bool
isPrime n = factors n == [1, n]

primes :: Integral a => a -> [a]
primes n = [x | x <- [1..n], isPrime x]

-- 2nd version
isPrime' :: Integral a => a -> Bool
isPrime' 2 = True
isPrime' p =
  p > 1 && (all (\n -> p `mod` n /= 0) $ takeWhile (\n -> n * n <= p) [3, 5..])

-- Eratosthenes sieve
sieve :: Integral a => [a] -> [a]
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]
