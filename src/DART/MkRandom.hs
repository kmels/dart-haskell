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

-- newtype GenM a = GenM 
--     { unGenM :: ReaderT (Int,Int) (StateT Int (MaybeT (Rand StdGen))) a }
--   deriving (Functor, Applicative, Monad, MonadPlus, MonadRandom,
--             MonadState Int, MonadReader (Int,Int))
           
-- -- | Run a generator to generate a data type of (approximately) size targetSize
-- runGenM :: Int -> Double -> GenM a -> IM (Maybe a)
-- runGenM targetSize eps m = do
--   let wiggle  = floor $ fromIntegral targetSize * eps
--       minSize = targetSize - wiggle
--       maxSize = targetSize + wiggle
--   g <- newStdGen
--   return . (evalRand ?? g) . runMaybeT . (evalStateT ?? 0)
--          . (runReaderT ?? (minSize, maxSize)) . unGenM
--          $ m
   
-- -- | Checks whether the size has exceeded and fails in case the size of the structure is too big, it increases the size of the structure otherwise
-- atom :: GenM ()
-- atom = do
--   (_, maxSize) <- ask
--   curSize <- get
--   when (curSize >= maxSize) mzero
--   put (curSize + 1)
        
          
-- | Randomize on external core types. An environment might be needed in case there is a reference to the heap as an identifier in e.g. a data type
mkRandomVal :: Env -> Ty -> IM Value
mkRandomVal env (Tcon qual_tcon)  = case zDecodeQualified qual_tcon of
  -- Make a random integer
  "ghc-prim:GHC.Types.Int" -> rndInt >>= return . Num . toInteger
  -- Make a random "id" type
  id -> do
    type_constructors <- fetchDataCons id env
    sumTypeMkRandom type_constructors env
        
mkRandomVal env (Tapp (Tcon qual_tcon1) (Tcon qual_tcon2)) = do
  type_constructors <- fetchDataCons (zDecodeQualified qual_tcon1) env
  io $ putStrLn $ "Type constructors of " ++ (zDecodeQualified qual_tcon1) ++ " are: " ++ show type_constructors
  return $ Wrong "TODO"
  
mkRandomVal env ty = return . Wrong $ " mkRandomVal: I don't know how to make a random val for the type " ++ showExtCoreTypeVerbose ty
  
-- | Looks up a definition of a sum type by a qualified identifier and returns a list of its constructors.
fetchDataCons :: Id -> Env -> IM [DataCon]
fetchDataCons id env = do
  -- look for the data type
  msumtype <- lookupId id env
  io $ putStrLn $ "fetchDataCons  " ++ show msumtype
  return $ case msumtype of
    (Right (SumType datacons)) -> datacons
    _ -> []

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
  val <- mkRandomVal env ty
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
mkHeapRef :: Value -> IM HeapReference
mkHeapRef = memorizeVal

-- | From the documentation of Haskell's Int:
-- "A fixed-precision integer type with at least the range [-2^29 .. 2^29-1]. The exact range for a given implementation can be determined by using Prelude.minBound and Prelude.maxBound from the Prelude.Bounded class. "
rndInt :: IM Int
rndInt = do
  min_bound <- getSetting min_int_bound
  max_bound <- getSetting max_int_bound  
  int <- io $ getStdRandom (randomR (min_bound, max_bound))
  --io $ putStrLn $ "Generating random int between " ++ show min_bound ++ " and " ++ show max_bound ++ " - " ++ show int
  return int

-- | And use the instances provided by the random package
rndBool :: IO Bool
rndBool = getStdRandom (randomR (minBound, maxBound))
