duplistn :: Int -> [a] -> [a]
duplistn n xs = foldr (++) [] $ map (replicate n) xs
