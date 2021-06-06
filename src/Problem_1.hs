module Problem_1
    ( sumMultiple
    ) where

-- https://projecteuler.net/problem=1
  
sumMultiple :: Int -> Int
sumMultiple 0 = 0
sumMultiple n = sum [a | a <- [1..n-1], mod a 3 == 0 || mod a 5 == 0]

