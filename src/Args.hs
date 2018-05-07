module Args (
  parseArgs
) where

import           Text.Read
import           Control.Monad

type Error = String

stringToInt :: String -> Maybe Integer
stringToInt x = readMaybe x :: Maybe Integer

maybeHead :: [String] -> Maybe String
maybeHead [] = Nothing
maybeHead xs = Just $ head xs

parseArgs :: [String] -> Either Error Integer
parseArgs = maybeToEither . maybeArgs
  where maybeArgs = stringToInt <=< maybeHead

maybeToEither :: Maybe a -> Either Error a
maybeToEither Nothing  = Left "Please provide a valid postive integer."
maybeToEither (Just x) = Right x
