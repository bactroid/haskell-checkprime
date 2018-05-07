module Args (
  parseArgs
) where

import           Text.Read
import           Control.Monad

type Error = String

getHead :: [a] -> Either Error a
getHead []    = Left "getHead: Empty List"
getHead (x:_) = Right x

parseArgs :: [String] -> Either Error Integer
parseArgs = readEither <=< getHead
