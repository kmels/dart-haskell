module Examples.GHC.Num (
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
 , fib0, fib1, fib2, fib3, fib4, fib5, fib6 , fib23 --, fib25, fibs, map
-- , five
 ) where

import Prelude hiding (map)

-- Operatinons on Num
numberTwenty = twice $ numberTen -- DONE

plusOneInt :: Int -> Int --TODO, remove type signature
plusOneInt n = n + 1
twice :: Int -> Int --TODO, remove type signature
twice n = n * 2

-- Computed with Num operations
numberTen = plusOneInt 9 -- DONE
numberEleven = plusOneInt numberTen

numberTwentyTwo = twice numberEleven -- DONE
numberFourtyTwo = twice $ (twice 5) * 2 + 1 -- DONE

-- Predicates

isTenEven = isEven numberTen
isHundredLargerThanZero = 100 >= 2

isEven n = n `mod` 2 == 0

tenIsEven = 10 `mod` 2 == 0

tautology1 = 10 == 10

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fib0 = fib 0
fib1 = fib 1
fib2 = fib 2
fib3 = fib 3
fib4 = fib 4
fib5 = fib 5
fib6 = fib 6

fib23 = fib 17
fib25 = fib 20
fibs = map fib [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]

map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs

five :: (Num a) => a
five = 5
