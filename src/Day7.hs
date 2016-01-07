{-# LANGUAGE OverloadedStrings #-}

module Day7 where

import Data.Bits
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import Data.Word
import Control.Applicative
import qualified Data.Map as Map

type Value = Word16
type Variable = String
type Target = Variable
data Operand = ValueOperand Value | VariableOperand Variable deriving Show
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
  return $ ValueOperand value

word = many1 letter_ascii

stringOperand :: Parser Operand
stringOperand = do
  value <- word
  return $ VariableOperand value

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

type SignalMap = Map.Map String Word16

{--
evaluateInstructions :: [Instruction] -> SignalMap
evaluateInstructions instructions =
  evaluateInstructions' instructions Map.empty

evaluateInstructions' :: [Instruction] -> SignalMap -> SignalMap
evaluateInstructions' instructions signalMap =
  let
    fullyEvaluatedInstructions = filter evaluated instructions
  in
    updateSignals fullyEvaluatedInstructions signalMap

updateSignals :: [Instruction] -> SignalMap -> SignalMap
updateSignals instructions signalMap =
  foldr signalMap
--}

evaluateOperator :: Operator -> Value
evaluateOperator operator =
  case operator of
    Identity (ValueOperand value) -> value
    Not (ValueOperand value) -> complement value
    And (ValueOperand value1) (ValueOperand value2) -> value1 .&. value2
    Or (ValueOperand value1) (ValueOperand value2) -> value1 .|. value2
    Lshift (ValueOperand value1) (ValueOperand value2) -> value1 `shift` (fromInteger (toInteger value2))
    Rshift (ValueOperand value1) (ValueOperand value2) -> value1 `shift` (-(fromInteger (toInteger value2)))
    _ -> error ("Could not evaluate operator: " ++ (show operator))

valueOfA :: String -> String
valueOfA input = "foo"