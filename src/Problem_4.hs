module Problem_4
  ( largestProductPalindrome
  )
where

-- https://projecteuler.net/problem=4
-- Find the largest palindrome made from the product of two 3-digit numbers.
-- Ex: 9009 = 91 × 99 for 2-digit numbers

largestProductPalindrome :: Int -> Int -> Int
largestProductPalindrome m n = maximum . filter isPalindrome $ map tupleProduct factors
  where tupleProduct = uncurry (*)
        factors = cartesianProduct [m..n] [m..n]

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct xs ys = (,) <$> xs <*> ys

isPalindrome :: Int -> Bool
isPalindrome n = and $ zipWith (==) xs (reverse xs)
  where xs = show n