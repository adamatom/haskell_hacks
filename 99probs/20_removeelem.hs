removeAt n xs = (elem, theRest)
  where
    elem = xs !! (n-1)
    theRest = (take (n-1) xs) ++ (drop n xs)
