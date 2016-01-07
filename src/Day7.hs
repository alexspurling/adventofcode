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
type Target = Variable
data Operand = IntOperand Value | StringOperand Variable deriving Show
data Operator = Identity Operand
  | And Operand Operand
  | Or Operand Operand 
  | Lshift Operand Operand 
  | Rshift Operand Operand 
  | Not Operand
  deriving Show
data Instruction = Instruction Operator Target deriving Show

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

binaryParser :: BS.ByteString -> (Operand -> Operand -> Operator) -> Parser Operator
binaryParser operatorString contructor = do
  param1 <- operandParser
  string operatorString
  param2 <- operandParser
  return $ contructor param1 param2

andParser = binaryParser " AND " And
orParser = binaryParser " OR " Or
lshiftParser = binaryParser " LSHIFT " Lshift
rshiftParser = binaryParser " RSHIFT " Rshift

operatorParser :: Parser Operator
operatorParser = 
  notParser <|> andParser <|> orParser <|> lshiftParser <|> rshiftParser <|> identityParser

instructionParser :: Parser Instruction
instructionParser = do
  operator <- operatorParser
  string " -> "
  target <- word
  return $ Instruction operator target

parseOperator :: String -> Operator
parseOperator input =
  case (parseOnly operatorParser (BS.pack input)) of
    (Left unparsed) -> error ("Failed to parse: " ++ input)
    (Right instruction) -> instruction

parseOperators :: String -> [Operator]
parseOperators input =
  map parseOperator (lines input)

parseInstruction :: String -> Instruction
parseInstruction input =
  case (parseOnly instructionParser (BS.pack input)) of
    (Left unparsed) -> error ("Failed to parse: " ++ input)
    (Right instruction) -> instruction

parseInstructions :: String -> [Instruction]
parseInstructions input =
  map parseInstruction (lines input)

valueOfA :: String -> String
valueOfA input = show $ parseInstructions input