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

-- We use cycle here because we want to pair up the
-- final guest with the first.
paring guests = zip guests (tail (cycle guests))

happiness pairs =
  map guestHappiness pairs
  where
    guestHappiness (guest1, guest2) =
      let
        happiness1 = (happinessMatrix!!guest1)!!guest2
        happiness2 = (happinessMatrix!!guest2)!!guest1
      in
        happiness1 + happiness2

totalHappiness =
  let
    numGuests = length happinessMatrix - 1
    guestOrderings = permutations [0..numGuests]
    allParings = map paring guestOrderings
    allHappiness = map happiness allParings
  in
    map sum allHappiness

maximumGuestHappiness :: Int
maximumGuestHappiness =
  maximum totalHappiness