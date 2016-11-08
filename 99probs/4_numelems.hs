myLength :: [a] -> Int
myLength list = sum $ map (const 1) list
