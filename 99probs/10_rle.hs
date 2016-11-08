group' :: (Eq a) => [a] -> [[a]]
group' [] = []
group' (x:xs) = (x:ys) : group' zs
                where (ys,zs) = span ((==) x) xs

rle :: (Eq a) => [a] -> [(Int,a)]
rle xs = map (\ys -> (length ys, (head ys))) $ group' xs
