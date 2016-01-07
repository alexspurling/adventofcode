{-# LANGUAGE OverloadedStrings #-}

module Day7 where

import qualified Data.Map

-- This attoparsec module is intended for parsing text that is
-- represented using an 8-bit character set, e.g. ASCII or ISO-8859-15.
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import Data.Word
import Control.Applicative


{--
-- | Type for IP's.
data IP = IP Word8 Word8 Word8 Word8 deriving Show

parseIP :: Parser IP
parseIP = do
  d1 <- decimal
  char '.'
  d2 <- decimal
  char '.'
  d3 <- decimal
  char '.'
  d4 <- decimal
  return $ IP d1 d2 d3 d4

main :: IO ()
main = print $ parseOnly parseIP "131.45.68.123"
--}

type Value = Word16
type Variable = String
data Operand = IntOperand Value | StringOperand Variable deriving Show
data Instruction = Identity Operand
  | And Operand Operand
  | Or Operand Operand 
  | Lshift Operand Operand 
  | Rshift Operand Operand 
  | Not Operand
  deriving Show
data Formula = Formula Instruction Operand deriving Show

intOperand :: Parser Operand
intOperand = do
  value <- decimal
  return $ IntOperand value

word = many1 letter_ascii

stringOperand :: Parser Operand
stringOperand = do
  value <- word
  return $ StringOperand value

operandParser :: Parser Operand
operandParser = stringOperand <|> intOperand

notParser = do
  string "NOT "
  param <- operandParser
  return $ Not param

identityParser = do
  param <- operandParser
  return $ Identity param

andParser = do
  param1 <- operandParser
  string " AND "
  param2 <- operandParser
  return $ And param1 param2

orParser = do
  param1 <- operandParser
  string " OR "
  param2 <- operandParser
  return $ Or param1 param2

lshiftParser = do
  param1 <- operandParser
  string " LSHIFT "
  param2 <- operandParser
  return $ Lshift param1 param2

rshiftParser = do
  param1 <- operandParser
  string " RSHIFT "
  param2 <- operandParser
  return $ Rshift param1 param2

instructionParser :: Parser Instruction
instructionParser = 
  notParser <|> andParser <|> orParser <|> lshiftParser <|> rshiftParser <|> identityParser

formulaParser :: Parser Formula
formulaParser = do
  instruction <- instructionParser
  string " -> "
  target <- stringOperand
  return $ Formula instruction target

parseInstruction :: String -> Instruction
parseInstruction input =
  case (parseOnly instructionParser (BS.pack input)) of
    (Left unparsed) -> error ("Failed to parse: " ++ input)
    (Right formula) -> formula

parseInstructions :: String -> [Instruction]
parseInstructions input =
  map parseInstruction (lines input)

parseFormula :: String -> Formula
parseFormula input =
  case (parseOnly formulaParser (BS.pack input)) of
    (Left unparsed) -> error ("Failed to parse: " ++ input)
    (Right formula) -> formula

parseFormulas :: String -> [Formula]
parseFormulas input =
  map parseFormula (lines input)

valueOfA :: String -> String
valueOfA input = show $ parseFormulas input