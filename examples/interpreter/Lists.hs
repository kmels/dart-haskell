module Lists (
  xs, head', one
  , naturals
  , first50
  , first3nats
  , first5nats
  , first10primes
  , five
  , str
  , chr
  , take'
  , takeTest1, takeTest2, takeTest3, takeTest4, takeTest5, takeTest6
) where

data List a = Nil | Cons a (List a)

-- works
xs = Cons 1000 (Cons 2000 Nil) 

head' :: List a -> a 
head' Nil = error "Empty list"
head' (Cons x _) = x

-- works
one = head' xs

-- first 5 nats


four :: Int
four = 4
five :: (Num a) => a
five = 5

--str :: (Eq a) => [a]
chr = 'a'
str = "a string"

first5nats = [1,2,3,four,five]
first3nats = take' 3 first5nats

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

four' = intListLength [0,1,2,3]
hundred = intListLength $ take 100 [1..]
hundredOne = intListLength [1..101]

take' :: Int -> [Int] -> [Int]
take' n _      | n <= 0 =  []
take' _ []              =  []
take' n (x:xs)          =  x : take' (n-1) xs

first50 = take' 50 naturals
takeTest1 = take' 0 [] -- works
takeTest2 = take' 1 [] -- works
takeTest3 = take' 500 [] -- works
takeTest4 = take' (5-5) [1,2,3]
takeTest5 = take' 1 [1]
takeTest6 = take' 2 [1,2,3]


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
