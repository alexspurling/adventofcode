import qualified Data.ByteString.Char8 as BS
import Crypto.Hash
import Data.List
import Data.Maybe

md5 :: BS.ByteString -> Digest MD5
md5 = hash

main = do
  let
    message =
      case getHashNumber of
        Nothing -> "No hash value found."
        Just x -> "Answer is: " ++ (show x)
  putStrLn message

getHashNumber :: Maybe Int
getHashNumber =
  findIndex hashMatches (map addKey [0..])

addKey :: Int -> String
addKey num =
  "bgvyzdsv" ++ (show num)

hashMatches :: String -> Bool
hashMatches input =
  let
    md5BS = digestToHexByteString (md5 (BS.pack input))
  in
    md5BS < BS.pack "00000100000000000000000000000000"

