----------------------------------------------------------------------------
-- |
-- Module      :  DART.Examples.Testing.OnTrees
-- Copyright   :  (c) Carlos López-Camey, University of Freiburg
-- License     :  BSD-3
--
-- Maintainer  :  c.lopez@kmels.net
-- Stability   :  stable
--
--
-- >>DESC<<
--
-- >>DETAILED DESC<<
-----------------------------------------------------------------------------

module DART.Examples.Testing.OnTrees(
  sumTree
  ,failOnEvenSum
  ,failOnOddSum) where

data IntTree Int = Leaf Int | Branch IntTree IntTree

-- | Transverses an IntTree and sums every node value
sumTree :: IntTree -> Int
sumTree (Leaf n) = n
sumTree (Branch t r) = sumTree t + sumTree r

-- | Function that fails if a tree sum is even, otherwise returns the sum
failOnEvenSum :: IntTree -> Int
failOnEvenSum tree = let sum = sumTree in if (sum `mod` 2 /= 0) then sum else error $ "Sum is even: " ++ show sum

-- | Function that fails if a tree sum is odd, otherwise returns the sum
failOnOddSum :: IntTree -> Int
failOnOddSum tree = let sum = sumTree in if (sum `mod` 2 == 0) then sum else error $ "Sum is odd: " ++ show sum
