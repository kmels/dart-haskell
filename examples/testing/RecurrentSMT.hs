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

module DART.Examples.Testing.RecurrentSMT (arithmeticBranches) where

arithmeticBranches :: Int -> Int -> Int
arithmeticBranches x y = 
  let z = 2 * y 
  in if (x < y) then x * 3
  else if (x < z) then y
  else if (4 * x > 5 * y) then 0
  else error $ " If we reach this code, we've won."
  
