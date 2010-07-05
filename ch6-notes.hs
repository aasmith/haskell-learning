-- factorial 4 is 
-- product [1..4] is
-- product [1,2,3,4] is
-- 1*2*3*4 is
-- 24
--

productt :: (Num a) => [a] -> a
productt [] = 1 -- could also use product [x] = x
productt (x:xs) = x * productt xs

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)
-- n+k deprecated;
--factorial (n+1) = (n + 1) * factorial n

-- note similarity to productt.
length :: [a] -> Int
length []     = 0
length (_:xs) = 1 + length xs

-- same
reverse :: [a] -> [a]
reverse []     = []
reverse (x:xs) = reverse xs ++ [x]

zip :: [a] -> [b] -> [(a,b)]
zip []     _      = []
zip _      []     = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys

drop :: Int -> [a] -> [a]
-- drop 0 xs = xs -- naive; doesnt work for < 0
drop n xs    | n <= 0 = xs -- better; guard on pattern works for <= 0
drop _ []             = []
drop n (_:xs)         = drop (n - 1) xs

-- list concatenation
(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

qsort :: [Int] -> [Int]
qsort []     = []
qsort (x:xs) = 
  qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a <= x]
    larger  = [b | b <- xs, b >  x]


