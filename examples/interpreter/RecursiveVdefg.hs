module RecursiveVdefg(
  even, odd
  , testOdd, testOdd', testOdd''
  , testEven, testEven', testEven''
  , oddOf0, oddOf11
  , evenOf1
  , evenOf10
  ) where

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
