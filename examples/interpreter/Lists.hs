module Lists (
  xs, listHead, one
  , naturals
  , first50
  , first3nats
  , first2, first0, first1
  , first5nats, first5nats_length
  , first10primes
  , five
  , str
  , chr
  , sumfifty
  , takeTest1, takeTest2, takeTest3, takeTest4, takeTest5, takeTest6
  , false1
  , intListLength, hundred, four
  , odd, even, evenOf10
) where

import Prelude hiding (even,odd)

data List a = Nil | Cons a (List a)

listHead :: List a -> a 
listHead Nil = error "Empty list"
listHead (Cons x _) = x

-- works
xs = Cons 1000 (Cons 2000 Nil) 
one = listHead xs

-- first 5 nats

five :: (Num a) => a
five = 5

--str :: (Eq a) => [a]
chr = 'a'
str = "a string"

first5nats = take 5 naturals
first3nats = take 3 first5nats

first5nats_length = length first5nats

naturals = [0..]
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

four = intListLength $ take 4 [0,1,2,3,10,20]
hundred = intListLength $ take 100 [1..]
hundredOne = intListLength [1..101]

first50 = take 50 naturals
takeTest1 = take 0 [] -- works
takeTest2 = take 1 [] -- works
takeTest3 = take 500 [] -- works
takeTest4 = take (5-5) [1,2,3]
takeTest5 = take 1 [1]
takeTest6 = take 2 [1,2,3]

sumfifty = sum [1..50]
  
false1 :: Bool
false1 = (takeTest2 :: [Int]) == (takeTest1 :: [Int])

even 0 = True
even x = odd (x-1)

odd 0 = False
odd x = even (x-1)

evenOf10 = even 10
------------------------- mlist -------------------------
-- data Mlist a = Mlist [a]

-- data Mordering = MEQ | MIN deriving (Eq, Show)

-- s = Mlist [1, 2, 3]
-- t = Mlist [1, 4, 2, 3]

-- class Mord a where 
--     mcompare :: a -> a -> Mordering

-- instance Mord (Mlist a) where
--     mcompare (Mlist xs) (Mlist ys)
--            | length xs == length ys && null (xs \\ ys) = MEQ
--            | otherwise = MIN

first2 = take 2 [1,2,3,4]
first0 = take 0 [1,2]
first1 = mytake 1 [5..10]

mytake n _      | n <= 0 =  []
mytake _ []              =  []
mytake n (x:xs)          =  x : mytake (n-1) xs

first2' = show $ f [1,2,3]
  where
    f (x:xs) = [x]
    f _ = []
