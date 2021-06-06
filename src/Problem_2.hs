module Problem_2
    ( sumEvenFib
    , fibonacci
    ) where

-- https://projecteuler.net/problem=2
  
sumEvenFib :: Int -> Int
sumEvenFib 0 = 0
sumEvenFib 1 = 1
sumEvenFib l = fib 0 (1, 2)
  where fib n (a, b) 
          | b > l = n
          | even b = fib (n + b) (b, a + b)
          | otherwise = fib n (b, a + b)

-- Just for checking actual fibonacci values  
fibonacci :: Int -> [Int]
fibonacci l = fib [0] (0, 1)
  where fib xs (a, b)
          | b > l = xs
          | otherwise = fib (xs ++ [b]) (b, a + b)     

