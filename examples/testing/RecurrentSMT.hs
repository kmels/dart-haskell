----------------------------------------------------------------------------
-- |
-- Module      :  DART.Examples.Testing.RecurrentSMT
-- Copyright   :  (c) Carlos López-Camey, University of Freiburg
-- License     :  BSD-3
--
-- Maintainer  :  c.lopez@kmels.net
-- Stability   :  stable
--
--
-- Useful definitions to test the SMT solver
-----------------------------------------------------------------------------

module DART.Examples.Testing.RecurrentSMT (arithmeticBranches,foldOrdered3) where

arithmeticBranches :: Int -> Int -> Int
arithmeticBranches x y = 
  let z = 2 * y 
  in if (x < y) then x * 3
  else if (x < z) then y
  else if (4 * x > 5 * y) then 0
  else error $ " If we reach this code, we've won."
  

-- | Define a function predicate that has a arity greater than 2
-- used to test pretty printing to Z3 
isOrdered3 :: Int -> Int -> Int -> Bool
isOrdered3 a b c = a > b && b > c

data Ordered = Ordered | NotOrdered deriving Show

-- | Use isOrdered3 as a predicate to build branches on the ordering (this fold)
foldOrdered3 :: Int -> Int -> Int -> Ordered
foldOrdered3 a b c = if (isOrdered3 a b c) 
                   then
                     Ordered
                   else
                     error . show $ NotOrdered
