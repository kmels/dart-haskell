module RecursiveVdefg(
  even, odd
  , testOdd, testOdd', testOdd''
  , testEven, testEven', testEven''
  , oddOf0, oddOf11
  , evenOf1
  , evenOf10
  -- fibs
  , fib0, fib1, fib2, fib10
  ) where

-- | There is no function to test here, yet.
-- The purpose of these definitions is to test the interpretation
-- of recursive definitions

import Prelude hiding (even,odd)

even :: Int -> Bool
even 0 = True
even n = odd (n-1)

odd :: Int -> Bool
odd 0 = False
--odd 1 = True
odd n = even (n-1)

oddOf0 = odd 0 
oddOf11 = odd 11
evenOf1 = even 1
evenOf10 = even 10

testOdd = odd 5
testOdd' = odd 6
testOdd'' = odd 7

testEven = even 5
testEven' = even 6
testEven'' = even 7

-- fib :: Int -> Int
-- fib 0 = 0
-- fib 1 = 1
-- fib n = 2 -- fib (n-2) + fib (n-1)

-- fib0 = fib 0
-- fib1 = fib 1
-- fib2 = fib 2
-- fib10 = fib 10

fib0 = 0
fib1 = 0
fib2 = 0
fib10 = 0
