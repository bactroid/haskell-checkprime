module Main where

import           Args
import           Data.Either
import           Prime
import           System.Environment

makeResultText :: Integer -> String
makeResultText n | isPrime n = show n ++ " is prime."
                 | otherwise = show n ++ " is composite.\n" ++ factorList
  where factorList = show $ findFactors n

main :: IO ()
main = do
  args <- getArgs
  let n = parseArgs args
  if isRight n
    then putStrLn . fromRight mempty $ makeResultText <$> n
    else putStrLn . fromLeft mempty $ n
