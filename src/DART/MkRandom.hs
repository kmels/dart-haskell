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
import Language.Core.Interpreter
import System.Random
import DART.ExtCore.TypeExtractor
import Data.List((!!))

-- | Randomize on external core types. An environment might be needed in case there is a reference to the heap as an identifier in e.g. a data type
tyMkRandom :: Ty -> Env -> Maybe (IM Value)
tyMkRandom ty env = extractType ty >>= \et -> case et of
  (CType ctype) -> Just $ mkRandomVal ctype env 
  --gtyp -> error $ "The impossible happened @tyMkRandom: It should not be called upon types that are not concrete (We are unpredicative), received " ++ show gtyp
  _ -> Nothing

-- | A function that generates a random value given a type.
-- We could have type classes on RandomizableTypes but it would imply using template haskell
-- as there are Ty's in DataCons and they're not pattern match friendly (we have indeed an extractor)
mkRandomVal :: ConcreteType -> Env -> IM Value
mkRandomVal (DType (DataType "ghc-prim:GHC.Types.Int")) _ = io rndInt >>= return . Num . toInteger
mkRandomVal (DType (DataType id)) env = do
  type_constructors <- fetchDataCons id env
  sumTypeMkRandom type_constructors env
    where
      fetchDataCons :: Id -> Env -> IM [DataCon]
      fetchDataCons id env = do
        -- look for the data type
        msumtype <- lookupId id env
        return $ case msumtype of
          (Right (SumType datacons)) -> datacons
          _ -> []
        
-- | Given a list of data constructors (that form a sum type), make a random
-- value of type of the sum type
sumTypeMkRandom :: [DataCon] -> Env -> IM Value
sumTypeMkRandom [] _ = return . Wrong $ "@dconsMkRandom: No data constructor"
sumTypeMkRandom tcs@(dc:ds) env = do -- TODO, consider other data cons (pick one randomly)
  -- pick a random data constructor
  typecons_idx <- io . getStdRandom $ randomR (0,length ds)
  let typecons = tcs !! typecons_idx
  tyConMkRandom typecons env

-- | Creates a value using a type constructor, exhausting every type argument
-- an environment might be needed in case the types in the type constructors
-- contain references to some data type in the heap as an identifier
-- TODO: MkDataCon should contain a list of ConcreteType and no Ty's
tyConMkRandom :: DataCon -> Env -> IM Value
tyConMkRandom dc@(MkDataCon id []) env = return $ TyConApp dc []
tyConMkRandom dc@(MkDataCon id tys) env = do
  ptrs <- mapM (flip tyRndValPtr env) tys -- :: [Pointer] where the generated random vals are
  return $ TyConApp dc ptrs

-- | Makes a random value from a type and returns a pointer to it
tyRndValPtr :: Ty -> Env -> IM Pointer
tyRndValPtr ty env = do
  val <- tyGetRandom ty env
  heap_ref@(_,addr) <- memorizeVal val
  return . MkPointer $ addr

-- | Version of tyMkRandom that returns an error value in case the given type is not understood
tyGetRandom :: Ty -> Env -> IM Value
tyGetRandom ty env = case tyMkRandom ty env of
  Nothing -> return . Wrong $ "tyGetRandom: Could not generate random value from " ++ show ty
  Just rndval -> rndval 

-- | Given a type, creates a random value, stores it in the heap and returns a heap reference. An environment might be needed in case the type is a reference to the heap
mkRandomHR :: ConcreteType -> Env -> IM HeapReference
mkRandomHR ct env = mkHeapRef $ mkRandomVal ct env

-- | Given a value, stores it in the heap and returns a heap reference
mkHeapRef :: IM Value -> IM HeapReference
mkHeapRef = (=<<) memorizeVal

-- | From the documentation of Haskell's Int:
-- "A fixed-precision integer type with at least the range [-2^29 .. 2^29-1]. The exact range for a given implementation can be determined by using Prelude.minBound and Prelude.maxBound from the Prelude.Bounded class. "
rndInt :: IO Int
rndInt = getStdRandom (randomR (minBound, maxBound))

-- | And use the instances provided by the random package
rndBool :: IO Bool
rndBool = getStdRandom (randomR (minBound, maxBound))
