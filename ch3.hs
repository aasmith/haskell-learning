second :: [a] -> a
second xs = head (tail xs)

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

pair :: x -> y -> (x,y)
pair x y = (x,y)

double :: (Num x) => x -> x
double x = x * 2

palindrome :: (Eq a) => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> (a -> a)
twice f x = f (f x)

-- twice takes:
--  (a -> a)  a function that takes something and returns something
--            such as double(x)
--  ->        and twice itself returns
--  (a -> a)  a function as above
