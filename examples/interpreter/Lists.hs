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
