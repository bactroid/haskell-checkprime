import System.Environment
import Text.Read
import Data.List
import Data.Maybe

factor :: (Integral a) => a -> [a]
factor n = [x | x <- [2..n - 1], n `mod` x == 0]

isPrime :: (Integral a) => a -> Bool
isPrime n = null $ factor n

add1AndSelf :: (Integral a) => a -> [a] -> [a]
add1AndSelf n (x:xs) = 1:n:(x:xs)
add1AndSelf n [] = 1:n:[]

findFactors :: (Integral a) => a -> [a]
findFactors n = sort(add1AndSelf n (factor n))

stringToInt :: String -> Maybe Integer
stringToInt x = readMaybe x :: Maybe Integer

parseArgs = stringToInt . head

main = do
  args <- getArgs
  let n = parseArgs args
  let resultIsPrime = fmap isPrime n
  if resultIsPrime == Nothing
    then putStrLn "Please provide a valid postive integer."
    else do
      if fromJust resultIsPrime
        then putStrLn $ (show $ fromJust n) ++ " is prime"
        else do 
          putStrLn $ (show $ fromJust n) ++ " is composite"
          putStrLn . show . fromJust $ fmap findFactors n
