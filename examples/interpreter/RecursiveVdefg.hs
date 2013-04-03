module RecursiveVdefg(
  even, odd
  
  ) where

import Prelude hiding (even,odd)

even :: Int -> Bool
even 0 = True
even n = odd (n-1)

odd :: Int -> Bool
odd 0 = False
--odd 1 = True
odd n = even (n-1)
