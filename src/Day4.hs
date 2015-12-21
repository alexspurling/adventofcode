module Day4 where

import qualified Data.ByteString.Char8 as BS
import Crypto.Hash
import Data.List
import Data.Maybe

md5 :: BS.ByteString -> Digest MD5
md5 = hash

getHashNumber :: Maybe Int
getHashNumber =
  findIndex (hashMatches "00001000000000000000000000000000") (map addKey [0..])

getHashNumber2 :: Maybe Int
getHashNumber2 =
  findIndex (hashMatches "00000100000000000000000000000000") (map addKey [0..])

addKey :: Int -> String
addKey num =
  "bgvyzdsv" ++ (show num)

hashMatches :: String -> String -> Bool
hashMatches maximum input =
  let
    md5BS = digestToHexByteString (md5 (BS.pack input))
  in
    md5BS < BS.pack maximum

