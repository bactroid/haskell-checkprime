module Args (
  parseArgs
) where

import           Text.Read

stringToInt :: String -> Maybe Integer
stringToInt x = readMaybe x :: Maybe Integer

parseArgs :: [String] -> Maybe Integer
parseArgs = stringToInt . head

