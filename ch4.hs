abs :: Int -> Int
abs x = if x < 0 then -x else x

signum :: Int -> Int
signum n = if n < 0 then -1 else
              if n == 0 then 0 else 1
