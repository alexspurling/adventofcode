module Day13 where

import Data.List

happinessMatrix :: [[Int]]
happinessMatrix =
  [[  0, 93,-54, 43,  8,-97, 45, 31],
   [ -2,  0,-70,-96,-89,  6, 76,-32],
   [-62, 19,  0,-53,-69, -9, 63, 95],
   [ 65,  5,-37,  0,-34, 56, 54, 91],
   [ 21, 49,-46,-30,  0,-17, 54,-66],
   [-81, 68, 33,-12, 95,  0, 30,-75],
   [ -4, 23,-35, 75, 34, 18,  0,-99],
   [-80, 29, 10,-20,-99,-56,  7,  0]]

happinessMatrix2 :: [[Int]]
happinessMatrix2 =
  [[  0, 93,-54, 43,  8,-97, 45, 31, 0],
   [ -2,  0,-70,-96,-89,  6, 76,-32, 0],
   [-62, 19,  0,-53,-69, -9, 63, 95, 0],
   [ 65,  5,-37,  0,-34, 56, 54, 91, 0],
   [ 21, 49,-46,-30,  0,-17, 54,-66, 0],
   [-81, 68, 33,-12, 95,  0, 30,-75, 0],
   [ -4, 23,-35, 75, 34, 18,  0,-99, 0],
   [-80, 29, 10,-20,-99,-56,  7,  0, 0],
   [  0,  0,  0,  0,  0,  0,  0,  0, 0]]

-- We use cycle here because we want to pair up the
-- final guest with the first.
paring guests = zip guests (tail (cycle guests))

happiness matrix pairs =
  map guestHappiness pairs
  where
    guestHappiness (guest1, guest2) =
      let
        happiness1 = (matrix!!guest1)!!guest2
        happiness2 = (matrix!!guest2)!!guest1
      in
        happiness1 + happiness2

totalHappiness matrix =
  let
    numGuests = length matrix - 1
    guestOrderings = permutations [0..numGuests]
    allParings = map paring guestOrderings
    allHappiness = map (happiness matrix) allParings
  in
    map sum allHappiness

maximumGuestHappiness :: Int
maximumGuestHappiness =
  maximum $ totalHappiness happinessMatrix

maximumGuestHappinessIncludingMe :: Int
maximumGuestHappinessIncludingMe =
  maximum $ totalHappiness happinessMatrix2