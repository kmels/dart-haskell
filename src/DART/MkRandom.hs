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
--class RandomizableType t where 
--mkRandomVal :: someTyp -> IM Value
--mkRandomVal (PrimitiveIntType _) = io rndInt >>= return . Num . toInteger

tyMkRandom :: Ty -> Maybe (IM Value)
tyMkRandom ty = extractType ty >>= \et -> case et of
  (CType ctype) -> Just . mkRandomVal $ ctype
  --gtyp -> error $ "The impossible happened @tyMkRandom: It should not be called upon types that are not concrete (We are unpredicative), received " ++ show gtyp
  _ -> Nothing

-- | A function that generates a random value given a type.
-- We could have type classes on RandomizableTypes but it would imply using template haskell
-- as there are Ty's in DataCons and they're not pattern match friendly (we have indeed an extractor)
mkRandomVal :: ConcreteType -> IM Value
mkRandomVal (DType (DataType "ghc-prim:GHC.Types.Int")) = io rndInt >>= return . Num . toInteger
mkRandomVal (DType (DataType id)) = error $ "TODO: dtMkRandomVal " ++ id

-- | Given a list of data constructors (that form a sum type), make a random
-- value of type of the sum type
sumTypeMkRandom :: [DataCon] -> IM Value
sumTypeMkRandom [] = return . Wrong $ "@dconsMkRandom: No data constructor"
sumTypeMkRandom (dc:ds) = do -- TODO, consider other data cons (pick one randomly)
  tyConMkRandom dc

-- | Creates a value using a type constructor, exhausting every type argument
tyConMkRandom :: DataCon -> IM Value
tyConMkRandom dc@(MkDataCon id []) = return $ TyConApp dc []
tyConMkRandom dc@(MkDataCon id tys) = do
  ptrs <- mapM tyRndValPtr tys -- :: [Pointer] where the generated random vals are
  return $ TyConApp dc ptrs

tyRndValPtr :: Ty -> IM Pointer
tyRndValPtr ty = do
  val <- tyGetRandom ty
  heap_ref@(_,addr) <- memorizeVal val
  return . MkPointer $ addr

tyGetRandom :: Ty -> IM Value
tyGetRandom ty = case tyMkRandom ty of
  Nothing -> return . Wrong $ "tyGetRandom: Could not generate random value from " ++ show ty
  Just rndval -> rndval

-- | Given a type, creates a random value, stores it in the heap and returns a heap reference
mkRandomHR :: ConcreteType -> IM HeapReference
mkRandomHR = mkHeapRef . mkRandomVal

-- | Given a value, stores it in the heap and returns a heap reference
mkHeapRef :: IM Value -> IM HeapReference
mkHeapRef = (=<<) memorizeVal

--mkRandomHRTy :: Ty -> IM HeapReference
--mkRandomHRTy ty = do
--  let extractedType = extractType ty -- :: Maybe GeneralType
  -- do we have a randomizable instance for extractedType?


-- instance RandomizableType PrimitiveType where
--   mkRandomVal (PrimitiveIntType _) = io rndInt >>= return . Num . toInteger
  
-- -- | Given a type constructor, generate a value
-- instance RandomizableType DataCon where
--   mkRandomVal dc@(MkDataCon id ty) = do  
--     -- TODO
--     fst <- mkRandomHR ty
--     return $ TyConApp dc []

-- | From the documentation of Haskell's Int:
-- "A fixed-precision integer type with at least the range [-2^29 .. 2^29-1]. The exact range for a given implementation can be determined by using Prelude.minBound and Prelude.maxBound from the Prelude.Bounded class. "
rndInt :: IO Int
rndInt = getStdRandom (randomR (minBound, maxBound))

-- | And use the instances provided by the random package
rndBool :: IO Bool
rndBool = getStdRandom (randomR (minBound, maxBound))
