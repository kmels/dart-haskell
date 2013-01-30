----------------------------------------------------------------------------
-- |
-- Module      :  DART.MkRandomValue
-- Copyright   :  (c) Carlos López-Camey, University of Freiburg
-- License     :  BSD-3
--
-- Maintainer  :  c.lopez@kmels.net
-- Stability   :  stable
--
--
-- This module contains functions that generate random values of different types.
-----------------------------------------------------------------------------

> module DART.MkRandomValue where

We use the random package

> import System.Random

From the documentation of Haskell's Int:

"A fixed-precision integer type with at least the range [-2^29 .. 2^29-1]. The exact range for a given implementation can be determined by using Prelude.minBound and Prelude.maxBound from the Prelude.Bounded class. "

> rndInt :: IO Int
> rndInt = getStdRandom (randomR (minBound, maxBound))

And use the instances provided by the random package

> rndBool :: IO Bool
> rndBool = getStdRandom (randomR (minBound, maxBound))

> rndChar :: IO Char
> rndChar = getStdRandom (randomR (minBound, maxBound))

> rndDouble :: IO Double
> rndDouble = getStdRandom (randomR (minBound, maxBound))

> rndFloat :: IO Double
> rndFloat = getStdRandom (randomR (minBound, maxBound))

> rndInteger :: IO Integer
> rndInteger = getStdRandom (randomR (minBound, maxBound))

Random Bool	 
Random Char	 
Random Double	 
Random Float	 
Random Int	 
Random Integer
Random 
