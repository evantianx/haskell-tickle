length' :: Foldable t => t a -> Int
length' xs = sum [1 | _ <- xs]
