module Day15 where

data Ingredient = Ingredient 
  { name :: String
  , capacity :: Int
  , durability :: Int
  , flavor :: Int
  , texture :: Int
  , calories :: Int }

ingredients :: [Ingredient]
ingredients = 
  [ Ingredient "Sprinkles" 5 (-1) 0 0 5
  , Ingredient "PeanutButter" (-1) 3 0 0 1
  , Ingredient "Frosting" 0 (-1) 4 0 6
  , Ingredient "Sugar" (-1) 0 0 2 8 ]

ingredientAmounts :: [[Int]]
ingredientAmounts =
  let
    max = 100
  in
    [[s,p,f,(max-s-p-f)] | s <- [0..max], p <- [0..max], f <- [0..max], s+p+f <= max]

type Score = [Int]

ingredientScore :: [Int] -> Int -> Score
ingredientScore amounts index =
  let
    ingredient = ingredients !! index
    amount = amounts !! index
  in
    [capacity ingredient * amount,
     durability ingredient * amount,
     flavor ingredient * amount,
     texture ingredient * amount]

cookieScore :: [Int] -> Int
cookieScore amounts =
  let
    sprinkles = ingredientScore amounts 0
    peanutbutter = ingredientScore amounts 1
    frosting = ingredientScore amounts 2
    sugar = ingredientScore amounts 3
    combined = foldr (zipWith (+)) (repeat 0) [sprinkles, peanutbutter, frosting, sugar]
    normalised = map (max 0) combined
    total = foldr (*) 1 normalised
  in
    total

calculateCalories :: [Int] -> Int
calculateCalories amounts =
  sum $ zipWith (*) amounts $ map calories ingredients

amountsWith500Calories :: ([[Int]] -> [[Int]])
amountsWith500Calories =
  filter (\x -> (calculateCalories x) == 500)

bestCookieScore :: Int
bestCookieScore = maximum $ map cookieScore ingredientAmounts

bestHealthyCookieScore :: Int
bestHealthyCookieScore = maximum $ map cookieScore (amountsWith500Calories ingredientAmounts)