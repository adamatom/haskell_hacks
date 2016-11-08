isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome xs
    | a /= z = False
    | a == z = isPalindrome mid
  where
    a = head xs
    z = head $ reverse xs
    mid = tail $ init xs
