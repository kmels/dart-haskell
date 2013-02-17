module FirstOrderFunctions (
  plusOneInt
  , sum
  , isEven
  , intListLength
  --numbers
  , numberTen
  , numberEleven
  , numberTwenty
  , numberTwentyTwo
  , numberFourtyTwo
  , numberFourtyFour
  --integer operations
  , twice
  , four
  , hundred
  , hundredOne
  , isOneEven
  ) where
  
plusOneInt :: Int -> Int
plusOneInt n = n + 1

numberEleven = plusOneInt numberTen
numberTen = plusOneInt 9
numberTwenty = twice $ numberTen

numberTwentyTwo = twice numberEleven

numberFourtyTwo = twice $ (twice 5) + 11
numberFourtyFour = twice numberTwentyTwo

twice :: Int -> Int
twice n = n * 2

--plusOneGeneric n = n + 1

mySum :: Int -> Int -> Int
a `mySum` b = a + b

isEven :: Int -> Bool
isEven n = n `mod` 2 == 0

intListLength :: [Int] -> Int
intListLength [] = 0
intListLength (x:xs) = intListLength xs + 1

four = intListLength [0,1,2,3]
hundred = intListLength $ take 100 [1..]
hundredOne = intListLength [1..101]

isOneEven = isEven 1

main :: IO ()
main = putStrLn "main"
