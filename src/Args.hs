module Args (
  parseArgs
) where

import           Text.Read

type Error = String

stringToInt :: String -> Maybe Integer
stringToInt x = readMaybe x :: Maybe Integer

parseArgs :: [String] -> Either Error Integer
parseArgs = maybeToEither . stringToInt . head

maybeToEither :: Maybe a -> Either Error a
maybeToEither Nothing  = Left "Please provide a valid postive integer."
maybeToEither (Just x) = Right x
