
andd :: [Bool] -> Bool
andd [] = True
andd (x:xs) = x && andd xs


concatt :: [[a]] -> [a]
concatt []     = []
concatt (xs:xss) = xs ++ concatt xss


replicatee :: Int -> a -> [a]
replicatee 0 _   = []
replicatee n e   = e : replicatee (n - 1) e


(!!!) :: [a] -> Int -> a
[]     !!! _ = undefined
(x:xs) !!! n = if n == 0 then x else xs !!! (n - 1)


elemm :: (Eq a) => a -> [a] -> Bool
elemm _ []     = False
elemm e (x:xs) = if e == x then True else elemm e xs
