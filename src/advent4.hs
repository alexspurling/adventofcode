import qualified Data.ByteString.Lazy as LB
import Crypto.Hash

md5 :: LB.ByteString -> Digest MD5
md5 = hashlazy

main = do
    fileContent <- LB.readFile "advent3.txt"
    let md5Digest = md5 fileContent
    print $ digestToHexByteString md5Digest