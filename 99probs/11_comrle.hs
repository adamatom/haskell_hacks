group' :: (Eq a) => [a] -> [[a]]
group' [] = []
group' (x:xs) = (x:ys) : group' zs
                where (ys,zs) = span ((==) x) xs

rle :: (Eq a) => [a] -> [(Int,a)]
rle xs = map (\ys -> (length ys, (head ys))) $ group' xs

data RLEItem a = Single a | Multiple Int a deriving (Show)


encodeRLE xs = map helper $ rle xs
  where
    helper (1,x) = Single x
    helper (n,x) = Multiple n x
