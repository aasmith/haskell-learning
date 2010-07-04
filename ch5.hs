-- return all triples (a,b,n) where a^2 + b^2 == n^2
-- using [1..n] as the search range
pyths :: Int -> [(Int,Int,Int)]
pyths n = [(a,b,c) | a <- r, b <- r, c <- r, a^2 + b^2 == c^2]
  where r = [1..n]

-- pyths 5 == [(3,4,5),(4,3,5)]

-- importing factors from lecture
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

-- a perfect number > 0 is: (sum of factors n) - n == n
perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (factors x) - x == x]

-- perfects 500 == [6,28,496]

-- a scalar product is the sum of each pair, where a pair is an 
-- element from both lists sharing the same index.
scalarp :: [Int] -> [Int] -> Int
scalarp xs ys = sum [x * y | (x,y) <- zip xs ys]

-- scalarp [1,2,3] [4,5,6] == 32
