module Problem_3
  ( largestPrimeFactor
  , isPrime
  , primes
  )
where

import Data.Foldable (find)
import Data.Maybe (fromMaybe)

-- https://projecteuler.net/problem=3
-- Largest prime factor of 600851475143
largestPrimeFactor :: Int -> Int
largestPrimeFactor n = fromMaybe 1 $ find (\p -> mod n p == 0) (reverse factors)
  where factors = takeWhile (\p -> p * p <= n) primes

primes :: [Int]
primes = 2 : [x | x <- [3..], isPrime x]

isPrime :: Int -> Bool
isPrime x = all (\p -> x `mod` p > 0) (factorsToTry x)
  where factorsToTry a = takeWhile (\p -> p * p <= a) primes