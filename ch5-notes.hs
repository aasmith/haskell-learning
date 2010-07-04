-- comprehension

[x+1 | x <- [1..5]] == [2,3,4,5,6]
--     ^^^^^^^^^^^----- generator; can be multiple

-- ruby equiv; 
-- z = []; [1,2,3].each { |e| [4,5].each { |ee| z << [e,ee]  }  }; z
[(x,y) | x <- [1,2,3], y <- [4,5]] == [(1,4),(1,5),(2,4),(2,5),(3,4),(3,5)]

-- correlated subquery - use outer loop variable in inner loop
[(x,y) | x <- [1..3], y <- [x..3]] == [(1,1),(1,2),(1,3),(2,2),(2,3),(3,3)]

-- generators always pull from a list;  t <- [t]

concat :: [[a]] -> [a]
concat xss = [x |  xs <- xss,   x <- xs  ]
--  types:    a   [a] <- [[a]], a <- [a]  

-- using a guard:
[x | x <- [1..10], even x] == [2,4,6,8,10]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

factors 15 == [1,3,5,15]

prime :: Int -> Bool
prime n = [1,n] == factors n

primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]

primes 15 == [2,3,5,7,11,13]

-- zip :: [a] -> [b] -> [(a,b)]

-- pair a list using zip and tail:
--      [1,2,3,4]  -> [1,2,3,4]  -> [(1,2), (2,3), (3,4)]
-- tail [1,2,3,4]  -> [2,3,4]

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

pairs [1..4] == [(1,2),(2,3),(3,4)]

-- check if list is sorted by taking every pair and checking that
-- the first tuple element (fst) is <= the last tuple element (snd)
-- this will produce a list of Bool, which is then passed to and.
-- 'sorted [1,2,3,4]' produces 'and [True, True, True]', produces 'True'.
sorted :: (Ord a) => [a] -> Bool
sorted xs = and [x <= y | (x,y) <- pairs xs]

sorted [1,2,3,4] == True
sorted [4,1,2,3] == False

-- find positions of a value in a list
-- as there is no inherent index, one can be created by zipping a list
-- with the infinite list [0..]; hence zip [0..] [a,b] [(0,a), (1,b)]

positions :: (Eq a) => a -> [a] -> [Int]
positions x xs = 
  [i | (x',i) <- zip xs [0..], x == x']

positions 1 [9,3,1,1,5,1] == [2,3,5]
--           0 1 2 3 4 5
--               ^ ^   ^

-- number of lower case chars in a string
lowers :: [Char] -> Int
lowers xs = length [x | x <- xs, isLower x]

lowers "Haskell" == 6


