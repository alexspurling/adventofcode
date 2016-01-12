
encodeString :: String -> String
encodeString input =
  concat $ map encode input where
    encode '\\' = "\\\\"
    encode '\"' = "\\\""
    encode a = [a]