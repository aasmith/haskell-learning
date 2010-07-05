
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
(x:_)  !!! 0 = x
(_:xs) !!! n = xs !!! (n - 1)


elemm :: (Eq a) => a -> [a] -> Bool
elemm _ []                 = False
elemm e (x:xs) | e == x    = True
               | otherwise = elemm e xs

