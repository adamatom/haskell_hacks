duplist :: [a] -> [a]
duplist xs = foldr (++) [] $ map helper xs where helper x = [x,x]
