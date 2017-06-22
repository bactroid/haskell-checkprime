module Prime (
  isPrime,
  findFactors
) where

import Data.List

factor :: (Integral a) => a -> [a]
factor n = [x | x <- [2..n - 1], n `mod` x == 0]

isPrime :: (Integral a) => a -> Bool
isPrime n = null $ factor n

add1AndSelf :: (Integral a) => a -> [a] -> [a]
add1AndSelf n (x:xs) = 1:n:(x:xs)
add1AndSelf n [] = 1:n:[]

findFactors :: (Integral a) => a -> [a]
findFactors n = sort(add1AndSelf n (factor n))

