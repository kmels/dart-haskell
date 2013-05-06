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
  sumTreeI
  --, sumTree
  ,failOnEvenSumI
  ,failOnOddSumI
  ,sumOfTreeSums
  ,myTree) where

import Data.Monoid
data IntTree = Leaf Int | Branch IntTree IntTree

-- | A polymorphic version of a tree that is sumable
data SumableTree a = PLeaf a | PBranch (SumableTree a) (SumableTree a)

myLeaf1 = Leaf 1
myLeaf2 = Leaf 2
myLeaf3 = Leaf 3
myTree = Branch myLeaf1 (Branch myLeaf2 myLeaf3)

-- | Transverses an IntTree and sums every node value
sumTreeI :: IntTree -> Int
sumTreeI (Leaf n) = n
sumTreeI (Branch t r) = sumTreeI t + sumTreeI r

-- | Function that fails if a tree sum is even, otherwise returns the sum
failOnEvenSumI :: IntTree -> Int
failOnEvenSumI tree = let sum = sumTreeI tree in if (sum `mod` 2 /= 0) then sum else error $ "Sum is even: " ++ show sum

-- | Function that fails if a tree sum is odd, otherwise returns the sum
failOnOddSumI :: IntTree -> Int
failOnOddSumI tree = let sum = sumTreeI tree in if (sum `mod` 2 == 0) then sum else error $ "Sum is odd: " ++ show sum

sumOfTreeSums :: [IntTree] -> Int
sumOfTreeSums = sum . map sumTreeI

-- | Transverses an IntTree and sums every node value
-- sumTree :: Monoid a => SumableTree a -> a
-- sumTree (PLeaf a) = a
-- sumTree (PBranch t r) = sumTree t <> sumTree r

