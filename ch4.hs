-- exercises

-- implement 'safetail', works like tail, but returns empty list when given an empty list.
-- use conditionals, guards and pattern matching.

-- using conditionals
safetail_c :: [a] -> [a]
safetail_c xs = if null xs then [] else drop 1 xs

-- using guards
safetail_g :: [a] -> [a]
safetail_g xs | null xs   = []
              | otherwise = drop 1 xs

-- using pattern matching
safetail_p :: [a] -> [a]
safetail_p (x:xs) = xs
safetail_p [] = []

-- define || three different ways with pattern matching
or_a :: Bool -> Bool -> Bool
or_a True _ = True 
or_a _ True = True
or_a _ _ = False

or_b :: Bool -> Bool -> Bool
or_b True  True  = True
or_b True  False = True
or_b False True  = True
or_b False False = False

-- or_c ?

-- redefine && using conditionals from
-- True && True = True
-- -    && _    = False

(&&) :: Bool -> Bool -> Bool
(&&) x y = if x == True then 
             if y == True then True else False
           else False

-- define && using conditionals from
-- True  && b = b
-- False && _ = False

(&&&) :: Bool -> Bool -> Bool
(&&&) x y = if x == True then y else False
