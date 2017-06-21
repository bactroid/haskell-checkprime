import Prime
import Args
import System.Environment
import Data.Maybe

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
