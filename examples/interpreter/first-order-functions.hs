
plusOne n = n + 1

sum :: Int -> Int -> Int
a `sum` b = a + b

isOdd :: Int -> Bool
isOdd n = n `mod` 2 == 0

intListLength :: [Int] -> Int
intListLength [] = 0
intListLength (x:xs) = intListLength xs + 1

--eval
fifty = intListLength [2,4..100]
isOneOdd = isOdd 1

main :: IO ()
main = do
  putStrLn . show $ fifty
  putStrLn . show $ isOneOdd
