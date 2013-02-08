module FirstOrderFunctions (
  plusOneInt
  , sum
  , isEven
  , intListLength
  , numberTen
  , fifty
  , isOneEven
  ) where
  
plusOneInt :: Int -> Int
plusOneInt n = n + 1

numberTen = plusOneInt 9

--plusOneGeneric n = n + 1

mySum :: Int -> Int -> Int
a `mySum` b = a + b

isEven :: Int -> Bool
isEven n = n `mod` 2 == 0

intListLength :: [Int] -> Int
intListLength [] = 0
intListLength (x:xs) = intListLength xs + 1

fifty = intListLength [2,4..100]
isOneEven = isEven 1

main :: IO ()
main = putStrLn "main"
