module Day12 where

import Data.Aeson
import Data.Scientific
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy.Char8 as BS

parse :: String -> Maybe Value
parse input = decode (BS.pack input)

sumJsonNumbers :: Value -> Int
sumJsonNumbers value =
  case value of
    Number val -> fromIntegral $ coefficient val
    Array v -> sum $ V.map sumJsonNumbers v
    Object hm -> sum $ map sumJsonNumbers (H.elems hm)
    _ -> 0

sumOfNumbers :: String -> Int
sumOfNumbers input =
  case parse input of
    Just value -> sumJsonNumbers value
    Nothing -> 0