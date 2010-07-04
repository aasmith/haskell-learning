oldabs :: Int -> Int
oldabs x = if x < 0 then -x else x

oldsignum :: Int -> Int
oldsignum n = if n < 0 then -1 else
              if n == 0 then 0 else 1

-- guarded equations
abs :: Int -> Int
abs x | x >= 0    = x
      | otherwise = -x

signum :: Int -> Int
signum x | x >  0  =  1
         | x == 0  =  0
         | x <  0  = -1

-- simple pattern matching
not :: Bool -> Bool
not False = True
not True  = False

-- argument that pattern matching is not unique to functional languages;
-- here it is reproduced in OO (ruby).
-- class TrueClass;  def not; false; end; end
-- class FalseClass; def not; true;  end; end

-- defining && using pattern matching and wildcards
-- wildcards are simply a way of requiring a parameter, but explcitly
-- declaring that it wont be used by the function; reduces logic errors
(&&) :: Bool -> Bool -> Bool
True && x = x
_    && _ = False

-- lists
-- [a,b,..] is extra syntax for the structure a:(b:(c:[]))
-- [1,2,3,4] == 1:(2:(3:(4:[])))

head :: [a] -> a
head (x:_) = x      -- function that operates on a non-empty list
                    -- must have an element x, and a list (which could be empty list [])
head [] = undefined -- function for the empty list []

-- laziness using a simple example
f :: a -> Int
f x = 1

-- executing: f undefined
-- stil returns 1 instead of raising an exception, as undefined 
-- has not been evaluated, because x is not used in the function body

-- lambda; anonymous function
-- (\x -> x + x) 1 #  returns  2

odds_wo_lambda n = map f [0..n-1]
                   where
                     f x = x * 2 + 1

odds n = map (\x -> x * 2 + 1) [0..n-1]

