module Prime (
  isPrime,
  findFactors
) where

import           Data.List

factor :: (Integral a) => a -> [a]
factor n = [x | x <- [2..n - 1], n `mod` x == 0]

fastFactors :: (Integral a) => a -> [a]
fastFactors n = [x | x <- [2..intSqrt n], n `mod` x == 0]

intSqrt :: (Integral a) => a -> a
intSqrt = ceiling . sqrt . fromIntegral

isPrime :: (Integral a) => a -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime 2 = True
isPrime n = null $ fastFactors n

add1AndSelf :: (Integral a) => a -> [a] -> [a]
add1AndSelf n (x:xs) = 1:n:(x:xs)
add1AndSelf n []     = [1, n]

findFactors :: (Integral a) => a -> [a]
findFactors n = sort(add1AndSelf n (factor n))
