group' :: (Eq a) => [a] -> [[a]]
group' [] = []
group' (x:xs) = (x:ys) : group' zs
                where (ys,zs) = span ((==) x) xs

compress :: (Eq a) => [a] -> [a]
compress xs = map head $ group' xs
-- compress xs = foldr (\a b -> if a == (head b) then b else a:b) [last xs] xs
