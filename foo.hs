prod (x:xs) = x * prod xs
prod x = 1

fac n = prod [1..n]

-- workslike sum
summ (x:xs) = x + summ xs
summ x = 0

-- workslike length
len (x:xs) = 1 + len xs
len [] = 0

flatten (xs:xss) = xs ++ flatten xss
flatten xs = []

-- workslike last, old thinking
oldend (xs) = xs !! (length xs - 1)

-- workslike last
end (x:[]) = x       -- when there is one element, and an 
                     -- empty list remaining (i.e. end of list), return x
end (_:xs) = end xs  -- when there is one element, and something
                     -- else that is not an empty list, recurse passing in
                     -- the not empty list
end [] = undefined   -- fallthrough case when end is called on empty list.

-- workslike init [1,2,3] # => [1,2]
noend (x:[]) = []
noend (x:xs) = [x] ++ noend(xs)
noend [] = []

-- workslike fst (a,b) # => a
first (a,_) = a

-- workslike reverse
backwards :: [a] -> [a]
backwards (x:xs) = backwards xs ++ [x]
backwards x = []

avg :: (Fractional a) => [a] -> a
avg xs = summ xs / fromIntegral (len xs)
-- :
-- prod 5
-- fac 4

--
divide a b = a / b
-- :
-- 3 `divide` 2


