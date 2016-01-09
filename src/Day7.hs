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

type SignalMap = Map.Map Variable Word16

isResolved :: Instruction -> Bool
isResolved (Instruction operator target) =
  case operator of
    Identity (ValueOperand _) -> True
    Not (ValueOperand _) -> True
    And (ValueOperand _) (ValueOperand _) -> True
    Or (ValueOperand _) (ValueOperand _) -> True
    Lshift (ValueOperand _) (ValueOperand _) -> True
    Rshift (ValueOperand _) (ValueOperand _) -> True
    _ -> False

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

updateSignal :: Instruction -> SignalMap -> SignalMap
updateSignal (Instruction operator target) signalMap =
  Map.insert target (evaluateOperator operator) signalMap

updateSignals :: [Instruction] -> SignalMap -> SignalMap
updateSignals resolvedInstructions signalMap =
  foldr updateSignal signalMap resolvedInstructions

resolveOperand :: SignalMap -> Operand -> Operand
resolveOperand signalMap operand =
  case operand of
    VariableOperand var ->  
      case (Map.lookup var signalMap) of
        Just value -> ValueOperand value
        Nothing -> operand
    _ -> operand

updateInstruction :: SignalMap -> Instruction -> Instruction
updateInstruction signalMap (Instruction operator target) =
  let
    newOperator = case operator of
      Identity operand -> Identity (resolveOperand signalMap operand)
      Not operand -> Not (resolveOperand signalMap operand)
      And operand1 operand2 -> And (resolveOperand signalMap operand1) (resolveOperand signalMap operand2)
      Or operand1 operand2 -> Or (resolveOperand signalMap operand1) (resolveOperand signalMap operand2)
      Lshift operand1 operand2 -> Lshift (resolveOperand signalMap operand1) (resolveOperand signalMap operand2)
      Rshift operand1 operand2 -> Rshift (resolveOperand signalMap operand1) (resolveOperand signalMap operand2)
  in
    Instruction newOperator target

updateInstructions :: [Instruction] -> SignalMap -> [Instruction]
updateInstructions unresolvedInstructions signalMap =
  map (updateInstruction signalMap) unresolvedInstructions

evaluateInstructions :: [Instruction] -> SignalMap -> SignalMap
evaluateInstructions [] signalMap = signalMap
evaluateInstructions instructions signalMap =
  let 
    resolved = filter isResolved instructions
    unresolved = filter (not . isResolved) instructions
    newSignalMap = updateSignals resolved signalMap
    newInstructionSet = updateInstructions unresolved newSignalMap
  in
    evaluateInstructions newInstructionSet newSignalMap

evaluateA :: SignalMap -> Word16
evaluateA signalMap =
  case Map.lookup "a" signalMap of
    Just value -> value
    Nothing -> error "Could not find value for signal a"

valueOfA :: String -> Word16
valueOfA input = 
  let
    instructions = parseInstructions input
    evaluatedSignals = evaluateInstructions instructions Map.empty
  in
    evaluateA evaluatedSignals

valueOfA2 :: String -> Word16
valueOfA2 input = 
  let
    a = valueOfA input
    instructions = parseInstructions input
    instructionsWithoutB = filter (\(Instruction operator target) -> target /= "b") instructions
    newBInstruction = Instruction (Identity (ValueOperand a)) "b"
    newInstructions = instructionsWithoutB ++ [newBInstruction]
    evaluatedSignals = evaluateInstructions newInstructions Map.empty
  in
    evaluateA evaluatedSignals