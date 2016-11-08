split :: Int -> [a] -> ([a],[a])
split n xs = (take n xs, drop n xs)

dropEvery :: Int -> [a] -> [a]
dropEvery _ [] = []
dropEvery n xs = (take (n-1) xs) ++ (dropEvery n . snd $ splitAt n xs)

