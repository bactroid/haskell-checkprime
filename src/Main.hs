module Main where

import           Args
import           Prime
import           System.Environment

makeResultText :: Integer -> String
makeResultText n | isPrime n = show n ++ " is prime."
                 | otherwise = show n ++ " is composite.\n" ++ factorList
  where factorList = show $ findFactors n

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    (Left  _) -> putStrLn "Please provide a valid postive integer."
    (Right x) -> putStrLn . makeResultText $ x
