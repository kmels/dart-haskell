----------------------------------------------------------------------------
-- |
-- Module      :  DART.MkRandom
-- Copyright   :  (c) Carlos López-Camey, University of Freiburg
-- License     :  BSD-3
--
-- Maintainer  :  c.lopez@kmels.net
-- Stability   :  stable
--
--
-- This module contains functions that to generate random values for different types.
-----------------------------------------------------------------------------

module DART.MkRandom where

import Language.Core.Interpreter.Structures
import System.Random
import DART.ExtCore.TypeExtractor

-- | A type class for types that gives us a function to randomize on types
class RandomizableType t where 
  mkRandomVal :: t -> IM Value

-- | Given a type, creates a random value, stores it in the heap and returns a heap reference
mkRandomHR :: (RandomizableType t) => t -> IM HeapReference
mkRandomHR t = mkRandomVal t >>= memorizeVal

instance RandomizableType PrimitiveType where
  mkRandomVal (PrimitiveIntType _) = io rndInt >>= return . Num . toInteger
  
-- | From the documentation of Haskell's Int:
-- "A fixed-precision integer type with at least the range [-2^29 .. 2^29-1]. The exact range for a given implementation can be determined by using Prelude.minBound and Prelude.maxBound from the Prelude.Bounded class. "
rndInt :: IO Int
rndInt = getStdRandom (randomR (minBound, maxBound))

-- | And use the instances provided by the random package
rndBool :: IO Bool
rndBool = getStdRandom (randomR (minBound, maxBound))
