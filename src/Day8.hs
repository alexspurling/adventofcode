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

memoryLengthLine :: String -> Int
memoryLengthLine line =
  case parseOnly stringParser (BS.pack line) of
    Left unparsed -> error ("Failed to parse: " ++ line ++ ". Error: " ++ unparsed)
    Right parsed -> length parsed

memoryLength :: String -> Int
memoryLength input = 
  sum (map memoryLengthLine (lines input))

characterDifference :: String -> Int
characterDifference input =
  codeLength input - memoryLength input