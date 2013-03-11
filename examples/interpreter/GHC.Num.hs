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
  , tenIsEven
  , tautology1
  ) where

-- Operatinons on Num
plusOneInt :: Int -> Int --TODO, remove type signature
plusOneInt n = n + 1
twice :: Int -> Int --TODO, remove type signature
twice n = n * 2

-- Computed with Num operations
numberTen = plusOneInt 9 -- DONE
numberEleven = plusOneInt numberTen
numberTwenty = twice $ numberTen -- DONE
numberTwentyTwo = twice numberEleven -- DONE
numberFourtyTwo = twice $ (twice 5) * 2 + 1 -- DONE

-- Predicates

isTenEven = isEven numberTen
isHundredLargerThanZero = 100 >= 2

isEven n = n `mod` 2 == 0


tenIsEven = 10 `mod` 2 == 0

tautology1 = 10 == 10
