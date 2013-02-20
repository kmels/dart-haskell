module Lists (
  naturals
  , first5nats
  , first10primes
) where

naturals = [0..]

first5nats = take 5 naturals

first10primes = take 10 $ filter isPrime naturals

isPrime n = let
  lpd = n `div` 2 -- largest possible divisor
  possible_divisors = drop 1 $ take lpd naturals
  in
   not $ any (\pd -> n `divides` pd) possible_divisors

n `divides` m = n `mod` m == 0


intListLength :: [Int] -> Int
intListLength [] = 0
intListLength (x:xs) = intListLength xs + 1

four = intListLength [0,1,2,3]
hundred = intListLength $ take 100 [1..]
hundredOne = intListLength [1..101]
