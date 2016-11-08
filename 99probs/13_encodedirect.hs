rle :: (Eq a) => [a] -> [(Int,a)]
rle = foldr helper []
  where
    helper x [] = [(1,x)]
    helper x (y@(a,b):ys)
      | x == b      = (1+a,x):ys
      | otherwise   = (1,x):y:ys


data RLEItem a = Single a | Multiple Int a deriving (Show)


encodeRLE xs = map helper $ rle xs
  where
    helper (1,x) = Single x
    helper (n,x) = Multiple n x
