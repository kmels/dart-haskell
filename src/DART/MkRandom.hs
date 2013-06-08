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
import Data.List((!!))

-- | Randomize on external core types. An environment might be needed in case there is a reference to the heap as an identifier in e.g. a data type
-- tyMkRandom :: Env -> Ty -> Maybe (IM Value)
-- tyMkRandom env ty = do
--   (_,val) <- runStateT mkRandomVal ty env
--   case val of
--     (Wrong _) -> Nothing
--     _ -> Just val

-- extractType ty >>= \et -> case et of
--   (CType ctype) -> Just $ mkRandomVal ctype env 
--   --gtyp -> error $ "The impossible happened @tyMkRandom: It should not be called upon types that are not concrete (We are unpredicative), received " ++ show gtyp
--   _ -> Nothing

-- | A function that generates a random value given a type.
-- We could have type classes on RandomizableTypes but it would imply using template haskell
-- as there are Ty's in DataCons and they're not pattern match friendly (we have indeed an extractor)
mkRandomVal :: Ty -> Env -> IM Value
mkRandomVal (Tcon qual_tcon) env = case showQualified qual_tcon of
  "ghc-prim:GHC.Types.Int" -> rndInt >>= return . Num . toInteger
  id -> do
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
mkRandomVal ty _ = return . Wrong $ " mkRandomVal: I don't know how to make a random val for the type " ++ showExtCoreTypeVerbose ty
        
-- | Given a list of data constructors (that form a sum type), make a random
-- value of type of the sum type
sumTypeMkRandom :: [DataCon] -> Env -> IM Value
sumTypeMkRandom [] _ = return . Wrong $ "@dconsMkRandom: No data constructor"
sumTypeMkRandom tcs@(dc:dcs) env = do 
  -- randomly pick one data constructor
  typecons_idx <- io . getStdRandom $ randomR (0,length dcs)
  let typecons@(MkDataCon typecons_id _) = tcs !! typecons_idx
  --io . putStrLn $ "Picked type cons: " ++ typecons_id
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
  val <- mkRandomVal ty env
  heap_ref@(_,addr) <- memorizeVal val
  return . MkPointer $ addr
  
-- | Version of tyMkRandom that returns an error value in case the given type is not understood
-- tyGetRandom :: Ty -> Env -> IM Value
-- tyGetRandom ty env = case tyMkRandom env ty of
--   Nothing -> return . Wrong $ "tyGetRandom: Could not generate random value from " ++ show ty
--   Just rndval -> rndval 

-- | Given a type, creates a random value, stores it in the heap and returns a heap reference. An environment might be needed in case the type is a reference to the heap
--mkRandomHR :: ConcreteType -> Env -> IM HeapReference
--mkRandomHR ct env = mkHeapRef $ mkRandomVal ct env

-- | Given a value, stores it in the heap and returns a heap reference
mkHeapRef :: IM Value -> IM HeapReference
mkHeapRef = (=<<) memorizeVal

-- | From the documentation of Haskell's Int:
-- "A fixed-precision integer type with at least the range [-2^29 .. 2^29-1]. The exact range for a given implementation can be determined by using Prelude.minBound and Prelude.maxBound from the Prelude.Bounded class. "
rndInt :: IM Int
rndInt = do
  min_bound <- getSetting min_int_bound
  max_bound <- getSetting max_int_bound
  io $ getStdRandom (randomR (min_bound, max_bound))

-- | And use the instances provided by the random package
rndBool :: IO Bool
rndBool = getStdRandom (randomR (minBound, maxBound))
