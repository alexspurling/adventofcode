{-# LANGUAGE OverloadedStrings #-}

module Day12 where

import Data.Aeson
import Data.List
import Data.Scientific
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V

parse :: String -> Maybe Value
parse input = decode (BS.pack input)

sumJsonNumbers :: Value -> Int
sumJsonNumbers value =
  case value of
    Number val -> fromIntegral $ coefficient val
    Array v -> sum $ V.map sumJsonNumbers v
    Object hm -> sum $ map sumJsonNumbers (H.elems hm)
    _ -> 0

sumNonRedJsonNumbers :: Value -> Int
sumNonRedJsonNumbers value =
  case value of
    Number val -> fromIntegral $ coefficient val
    Array v -> sum $ V.map sumNonRedJsonNumbers v
    Object hm ->
      if elem "red" (H.elems hm) then 0
      else sum $ map sumNonRedJsonNumbers (H.elems hm)
    _ -> 0

sumOfNumbers :: (Value -> Int) -> String -> Int
sumOfNumbers f input =
  case parse input of
    Just value -> f value
    Nothing -> 0

sumOfAllNumbers :: String -> Int
sumOfAllNumbers input = sumOfNumbers sumJsonNumbers input

sumOfNonRedNumbers :: String -> Int
sumOfNonRedNumbers input = sumOfNumbers sumNonRedJsonNumbers input