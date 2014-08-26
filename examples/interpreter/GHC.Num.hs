module DART.Examples.GHC.Num (
 -- computed with Num operations
   numberTen
 , numberEleven
 , numberTwenty
 , numberTwentyTwo
 , numberFourtyTwo
 , fib0, fib1, fib2, fib3, fib4, fib5, fib6 , fib23, fib25, fibs
 -- predicates on Nums
 , isTenEven
 , isHundredLargerThanZero
 , tenIsEven
 , tautology1
 -- operations
 , sumPlusOne
 , example1
-- , five
 ) where

import qualified Control.Exception as E
--import Prelude hiding (map)

-- Operatinons on Num
numberTwenty = twice $ numberTen -- DONE

plusOneInt :: Int -> Int --TODO, remove type signature
plusOneInt n = n + 1

-- | A function of arity 3, x `sumPlusOne` y = x + y + 1
sumPlusOne :: Int -> Int -> Int
sumPlusOne x y = plusOneInt $ x + y 

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

--fibs = map fib [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]

fibs = map fib [17,20]

five :: (Num a) => a
five = 5

myDiv1 :: Float -> Float -> Float
myDiv1 x 0 = error "Division by zero"
myDiv1 x y = x / y

-- from http://www.randomhacks.net/2007/03/10/haskell-8-ways-to-report-errors/
example1 :: Float -> Float -> IO ()
example1 x y =
  E.catch (putStrLn (show (myDiv1 x y)))
          (\err -> putStrLn (show err))
