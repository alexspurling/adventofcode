module Day6 where

import Linear.V2
import Data.Array.ST
import Data.Array.Unboxed

import Control.Monad
import Control.Monad.ST

import Text.Regex.Posix

type Index    = V2 Int
type Grid     = UArray Index Bool
type STGrid s = STUArray s Index Bool
type BrightGrid     = UArray Index Int
type BrightSTGrid s = STUArray s Index Int

data Rectangle = Rectangle (V2 Int) (V2 Int)
  deriving (Show, Eq)

data Operation = TurnOn | TurnOff | Toggle
  deriving (Show, Eq)

data Instruction = Instruction Operation Rectangle
  deriving (Show, Eq)

listIndices :: Rectangle -> [Index]
listIndices (Rectangle c1 c2) = range (c1,c2)

--On/Off grid
turnOn :: STGrid s -> Index -> ST s ()
turnOn grid index = writeArray grid index True

turnOff :: STGrid s -> Index -> ST s ()
turnOff grid index = writeArray grid index False

toggle :: STGrid s -> Index -> ST s ()
toggle grid index = do
    curState <- readArray grid index
    writeArray grid index (not curState)

runLightInstruction :: STGrid s -> Instruction -> ST s ()
runLightInstruction grid = go
  where
    go (Instruction TurnOn  rect) = forM_ (listIndices rect) (turnOn  grid)
    go (Instruction TurnOff rect) = forM_ (listIndices rect) (turnOff grid)
    go (Instruction Toggle  rect) = forM_ (listIndices rect) (toggle  grid)

runInstructions :: [Instruction] -> Grid
runInstructions instructions = runST $ do
    grid <- newArray (V2 0 0, V2 999 999) False
    forM_ instructions (runLightInstruction grid)
    freeze grid

-- Brightness Grid
turnOnB :: BrightSTGrid s -> Index -> ST s ()
turnOnB grid index = do
    curBrightness <- readArray grid index
    writeArray grid index (curBrightness + 1)

turnOffB :: BrightSTGrid s -> Index -> ST s ()
turnOffB grid index = do
    curBrightness <- readArray grid index
    writeArray grid index (max (curBrightness - 1) 0)

toggleB :: BrightSTGrid s -> Index -> ST s ()
toggleB grid index = do
    curBrightness <- readArray grid index
    writeArray grid index (curBrightness + 2)

runBrightInstruction :: BrightSTGrid s -> Instruction -> ST s ()
runBrightInstruction grid = go
  where
    go (Instruction TurnOn  rect) = forM_ (listIndices rect) (turnOnB  grid)
    go (Instruction TurnOff rect) = forM_ (listIndices rect) (turnOffB grid)
    go (Instruction Toggle  rect) = forM_ (listIndices rect) (toggleB  grid)

runBrightInstructions :: [Instruction] -> BrightGrid
runBrightInstructions instructions = runST $ do
    grid <- newArray (V2 0 0, V2 999 999) 0
    forM_ instructions (runBrightInstruction grid)
    freeze grid

parseInstruction :: String -> Instruction
parseInstruction input =
  let 
    match = head (input =~ "(.*) ([0-9]+),([0-9]+) through ([0-9]+),([0-9]+)")
    instruction = match !! 1
    fromX = read (match !! 2) :: Int
    fromY = read (match !! 3) :: Int
    toX = read (match !! 4) :: Int
    toY = read (match !! 5) :: Int
    rectangle = Rectangle (V2 fromX fromY) (V2 toX toY)
  in
    case instruction of
      "toggle" -> Instruction Toggle rectangle
      "turn on" -> Instruction TurnOn rectangle
      "turn off" -> Instruction TurnOff rectangle
      _ -> error ("Instruction not recognised: " ++ instruction)

parseInstructions :: String -> [Instruction]
parseInstructions input =
  map parseInstruction (lines input)

(|>) x y = y x

lightsLit input = parseInstructions input
  |> runInstructions
  |> elems
  |> filter id
  |> length

totalBrightness input = parseInstructions input
  |> runBrightInstructions
  |> elems
  |> sum