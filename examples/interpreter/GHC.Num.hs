module FirstOrderFunctions (
  -- computed with Num operations
    numberTen
  , numberEleven
  , numberTwenty
  , numberTwentyTwo
  , numberFourtyTwo
  -- predicates on Nums
  , isTenEven
  , isHundredLargerThanZero
  ) where

-- Operatinons on Num
plusOneInt :: Int -> Int --TODO, remove type signature
plusOneInt n = n + 1
twice :: Int -> Int --TODO, remove type signature
twice n = n * 2

-- Computed with Num operations
numberTen = plusOneInt 9
numberEleven = plusOneInt numberTen
numberTwenty = twice $ numberTen
numberTwentyTwo = twice numberEleven
numberFourtyTwo = twice $ (twice 5) * 4 + 1

-- Predicates
isTenEven = isEven numberTen
isHundredLargerThanZero = 100 >= 2

isEven n = n `mod` 2 == 0


