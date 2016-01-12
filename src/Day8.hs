{-# LANGUAGE OverloadedStrings #-}

module Day8 where

import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import Control.Applicative
import Data.Char

codeLength :: String -> Int
codeLength input =
  sum (map length (lines input))

escapedQuote :: Parser Char
escapedQuote = string "\\\"" >> return '"'

escapedBackslash :: Parser Char
escapedBackslash = string "\\\\" >> return '\\'

fromHex :: Char -> Char -> Char
fromHex char1 char2 =
  toEnum (16 * (digitToInt char1) + (digitToInt char2))

escapedHex :: Parser Char
escapedHex = do
  string "\\x"
  hexchar1 <- anyChar
  hexchar2 <- anyChar
  return (fromHex hexchar1 hexchar2) --Converts character from ascii code to character

stringChar :: Parser Char
stringChar = escapedQuote <|> escapedBackslash <|> escapedHex <|> anyChar

stringParser :: Parser String
stringParser = char '"' *> manyTill stringChar (char '"')

inputParser :: Parser [String]
inputParser = many (stringParser <* endOfLine)

parseInput :: String -> [String]
parseInput input =
  case parseOnly inputParser (BS.pack input) of
      Left unparsed -> error ("Failed to parse input. Error: " ++ unparsed)
      Right parsed -> parsed

memoryLength :: [String] -> Int
memoryLength strings =
  sum (map length strings)

characterDifference :: String -> Int
characterDifference input =
  codeLength input - memoryLength (parseInput input)

encodeString :: String -> String
encodeString input =
  "\"" ++ (concat (map encode input)) ++ "\"" 
  where
    encode '\\' = "\\\\"
    encode '\"' = "\\\""
    encode a = [a]

encodedStrings :: [String] -> [String]
encodedStrings strings =
  map encodeString strings

encodedLength :: [String] -> Int
encodedLength strings =
  sum (map length (encodedStrings strings))

encodedDifference :: String -> Int
encodedDifference input =
  encodedLength (lines input) - codeLength input
