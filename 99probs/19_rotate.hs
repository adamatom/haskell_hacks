rotate :: Int -> [a] -> [a]
rotate n xs
  | n >= 0      = (drop n xs) ++ (take n xs)
  | otherwise   = (drop r xs) ++ (take r xs)
    where
      r = (length xs) + n
