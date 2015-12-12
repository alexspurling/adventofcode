main = do
  input <- getLine
  let steps = stepsToFloor input 0 (-1) 0
  putStrLn ("Requires " ++ (show steps) ++ " steps to get to the basement")

stepsToFloor :: String -> Int -> Int -> Int -> Int
stepsToFloor input currentFloor targetFloor stepsTaken =
  case input of
    [] -> stepsTaken
    (x:xs) ->
      let
        newFloor = currentFloor + (advanceFloor x)
      in
        if newFloor == targetFloor then
          stepsTaken + 1
        else
          stepsToFloor xs newFloor targetFloor (stepsTaken + 1)


advanceFloor :: Char -> Int
advanceFloor char =
  case char of
    '(' -> 1
    ')' -> -1
    _ -> 0
