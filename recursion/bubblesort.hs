swaps :: Ord a => [a] -> [a]
swaps [] = []
swaps [x] = [x]
swaps (x1:x2:xs)
  | x1 > x2 = x2 : swaps(x1:xs)
  | otherwise = x1 : swaps(x2:xs)
  
bubbleSort :: Ord a => [a] -> [a]
bubbleSort xs 
  | swaps xs == xs = xs
  | otherwise = bubbleSort $ swaps xs

bubbleSort' :: Ord a => [a] -> [a]
bubbleSort' [] = []
bubbleSort' xs = bubbleSort' initialElements ++ [lastElement]
  where swappedxs = swaps xs
        initialElements = init swappedxs
        lastElement = last swappedxs
