doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100 then x else doubleMe x
fizzBuzz xs = [ case (rem x 3, rem x 5) of
                    (0,0) -> "FizzBuzz"
                    (0,_) -> "Fizz"
                    (_,0) -> "Buzz"
                    (_,_) -> show x | x <- xs]

length' xs = sum [1 | _ <- xs]

rightTri :: [(Integer, Integer, Integer)]
rightTri = [ (a,b,c) | c <- [1..10], a <- [1..c], b <- [1..a], a^2 + b^2 == c^2, a+b+c == 24]


removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <- st, elem c ['A' .. 'Z'] ]

fibonacci :: Int -> Int
fibonacci 1 = 1
fibonacci 2 = 1
fibonacci x = fibonacci (x-1) + fibonacci (x -2) -- constructs a deep stack/tree shape

-- a <- a+b
-- b <- a
iter a b i count= if i > count then b else iter (a+b) a (i+1) count -- iterative version of the fibonacci sequence
fib x = iter 0 1 0 x

cube x = x * x * x

-- TODO: implement simpsons numerical method
--

-- pattern matching
tell :: (Show a) => [a] -> String
tell [] = "Empty list"
tell (x:[]) = "One item: " ++ show x
tell (x:y:[]) = "Two items: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "Many elements"
-- doesnt work tell all@(x:y:_) = "Many elements in " ++ disp all
-- try map (\xs lamda to eat single var, pass to show) [1,2,3]  -- how do we convert all elements of a list to
-- a string?


calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [ bmi x y | (x,y) <- xs]
                where bmi w h = w / h^2

cylinderArea :: Double -> Double -> Double
cylinderArea r h = let baseArea = pi * r ^2; sideArea = 2 * pi * r * h in sideArea + 2 * baseArea

cylinderArea' :: Double -> Double -> Double
cylinderArea' r h = sideArea + 2 * baseArea
                    where sideArea = 2 * pi * r * h
                          baseArea = pi * r^2

-- recursion
-- implement replicate n i, return a list of n i's
replicate' n i
    | (n <= 0) = []
    | otherwise = i : replicate' (n-1) i

quicksort [] = []
quicksort (x:xs) = quicksort lesser ++ [x] ++ quicksort greater
    where lesser = filter (<= x) xs
          greater = filter (> x) xs

-- had this problem at work, C++ <algorithm> went poorly
-- given a list of one type of tuple and a list of another type of tuple, transform the tuples into lists and then zip
-- them together into a final list
complexZip :: [(t, t1, t2)] -> [(t3, t4, t5)] -> [(t, t1, t2, t3, t4, t5)]
complexZip xs ys = let mergeTuple (a,b,c) (d,e,f) = (a,b,c,d,e,f) in zipWith mergeTuple xs ys

-- higher order functions
zipWith' :: ( a -> b -> c ) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- fun with recursion
-- implement Collatz sequence: start with natural n, if n is 1, stop. if 1 is even, divide by 2. if odd, (+ (* n 3) 1)
-- repeat

collatz :: Int -> [Int]
collatz 1 = [1]
collatz n
    | (rem n 2 == 0 ) = n : collatz (div n 2)
    | otherwise = n : collatz ((n * 3) + 1)

numLongCollatz :: Int -> Int -> Int
numLongCollatz n u = length . filter (\xs -> length xs > n) $ map collatz [1..u] -- hey look, a lambda

phoneBook =
    [("adam", "555-9096")
    ,("ellen", "555-9097")
    ,("home", "555-5024")
    ]

findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key xs = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing xs
