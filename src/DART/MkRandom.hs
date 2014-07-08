{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
-- This module contains functions that generate random values for a given type.
-----------------------------------------------------------------------------

module DART.MkRandom where

--------------------------------------------------------------------------------
-- Control
import Control.Applicative
import Control.Monad.Trans.Maybe
import Control.Monad.Random
import Control.Monad.Reader
--------------------------------------------------------------------------------
-- Random
import System.Random
--------------------------------------------------------------------------------
-- Language
import Language.Core.Interpreter.Structures
import Language.Core.Interpreter
import Language.Core.Interpreter.Util(showValue)

--------------------------------------------------------------------------------
-- DART
import Language.Core.Ty(printSignature)

--------------------------------------------------------------------------------
-- Prelude
import Data.Maybe(catMaybes)

import Data.List((!!), findIndices)
          
-- | When a TypeConstructor is applied to another TypeConstructor, we get another TypeConstructor. This function reduces the argument type constructor to a type to form a DataCon
applyTyCon :: Value -> Value -> IM Value
applyTyCon (TyCon dc tycon1)  (TyCon dc2 tycon2) = return $ Wrong "TODO"

-- | Given a type in external core, randomly produce a value of its type. An environment might be needed in case there is a reference to the heap as an identifier in e.g. a data type may contain its type constructors.
mkRandomVal :: Env -> Ty -> IM Value
mkRandomVal env (Tcon qual_tcon)  = do
  io $ putStr "mkRandomVal .. "
  case zDecodeQualified qual_tcon of
    "ghc-prim:GHC.Types.Int" -> do -- a random integer
      rndInt >>= return . Num . toInteger
    -- found an identifier, fetch type constructors
    id -> do
      type_constructors <- fetchDataCons id env
      genBoltzmann type_constructors env -- get a value using our boltzmann sampler

-- application of a type constructor qtycon1 to qtycon2
mkRandomVal env (Tapp (Tcon zqtycon1) (Tcon zqtycon2)) = do
  let
    qtycon_1 = zDecodeQualified zqtycon1
    qtycon_2 = zDecodeQualified zqtycon2
    
  io $ putStrLn $ " applying  " ++ show qtycon_1
  io $ putStrLn $ " to  " ++ show qtycon_2
  
  -- we should have info about `qtycon1`
  tycon1 <- fetchTyCon qtycon_1 env
  tycon2 <- fetchTyCon qtycon_2 env
  
  datacons <- applyTyCon tycon1 tycon2
  
  (TyCon tycon1@(MkDataCon tycon1_id expected_types@(ty:tys)) built_type_id) <- fetchTyCon qtycon_1 env
  
  io $ putStrLn $ (show tycon1)
  io $ putStrLn $ (show tycon2)
--  io $ putStrLn $ (show tys)
  --io $ putStrLn $ " print  " ++ show expected_types
  tycon_ty <- printSignature expected_types
  io $ putStrLn $ " Making a value of type " ++ built_type_id ++ " from tycon :: " ++ show tycon_ty
    
  -- check how many types tycon1 is expecting. In case it is expecting only one
  -- then we have built our final type (it is being applied to tycon2).
  case (length expected_types) of
    1 -> do
      -- what type does tycon1 build and what are its constructors? 
      --tycons <- fetchDataCons built_type_id env
      let
        fetchSumType :: Id -> Env -> IM Value
        fetchSumType id env = lookupId id env >>= \v -> case v of
          Right (SumType cns) -> return $ SumType cns
          w -> error $ "Expecting " ++ id ++ " to be a sum type, but found: " ++ show w
          --w -> return . Wrong $ "Expecting " ++ id ++ " to be a sum type, but found: " ++ show w
      
      SumType tycons <- fetchSumType built_type_id env

      --type_bind = TypeInstantiation (ty,tycon2_id)
            
      io $ putStrLn $ "We have a sum type with " ++ (show . length $ tycons) ++ " constructors" ++ ": " ++ show tycons
      
      io $ putStrLn $ show ty ++ " is really a " ++ qtycon_2
      
      return $ Wrong $ ", " ++ show tycons
    -- OTHERWISE, TODO2
    n -> do
      io $ putStrLn $ "Type constructors of " ++ qtycon_1 ++ " are: " ++ show n
      
      return $ Wrong "TODO2"
  
mkRandomVal env ty = return . Wrong $ " mkRandomVal: I don't know how to make a random val for the type " ++ showExtCoreTypeVerbose ty
  

-- | Given a list of type constructors, and an environment of type bindings, fabricate a value.
--newtype FreeTypeBind = FreeTypeBind { typeInstantiation :: (Ty,Id) } 

-- | Given a qualified type constructor name – e.g, (:) – seek the data constructor and also the qualified type it builds – e.g., [] for cons 
fetchTyCon :: Id -> Env -> IM Value
fetchTyCon id env = do
  tycon <- lookupId id env
  return $ case tycon of
    (Right tycon@(TyCon datacons ty_id)) -> tycon
    (Right e@(Wrong _)) -> e
    v -> Wrong $ "The impossible happened @fetchTyCon looking upon " ++ id ++ ", got: " ++ show v

-- | Looks up a definition of a sum type by a qualified identifier and returns a list of its constructors.
fetchDataCons :: Id -> Env -> IM [DataCon]
fetchDataCons id env = do
  -- look for the data type
  msumtype <- lookupId id env
  --io $ putStrLn $ "fetchDataCons  " ++ show msumtype
  return $ case msumtype of
    (Right (SumType datacons)) -> datacons
    (Right (TyCon datacons _)) -> [datacons]
    _ -> []

pickTypeConstructor :: [DataCon] -> IM DataCon
pickTypeConstructor tcs = do
  typecons_idx <- io . getStdRandom $ randomR (0,length tcs - 1)
  let typecons = tcs !! typecons_idx -- tcs[typecons_idx]
  return typecons
  
-- -- | Given a list of type constructors for a data type, generate a 
-- -- new value using a boltzmann sampler around a size 
-- -- For an example of this technique, see Brent Yorgey's post, which inspired this smapler: http://byorgey.wordpress.com/2013/04/25/random-binary-trees-with-a-size-limited-critical-boltzmann-sampler-2/
genBoltzmann :: [DataCon] -> Env -> IM Value
genBoltzmann tcs env = do
  io $ putStr "Called boltzman ..." 
  status <- gets boltzmannSamplerStatus
  case status of
    UnitializedSampler -> do
      io $ putStrLn $ "\tgenerating new ..."
      initializeBoltzmann
      value' <- genBoltzmann tcs env      
      valSize <- gets boltzmannSamplerSize
      minSize <- getSetting data_min_size  
           
      io $ putStr $ "Generated boltzmann of size " ++ (show valSize)
--      io $ putStr $ "Checking on min size " ++ (show valSize) ++ " >= " ++ (show minSize) ++ "? ..." 
  
      case (valSize >= minSize) of
        True -> do
          io $ putStrLn $ " ... good, of size = " ++ (show valSize)
          (showValue value') >>= \v -> io $ putStrLn $ "VALUE = "  ++ v
--          resetBoltzmann
          boltzmannSucess
          return value'
        _ -> do
          io $ putStrLn " ... bad" 
          boltzmannFail >> genBoltzmann tcs env
    InitializedSampler -> do
      io $ putStr $ "\tdoing step ... "
      sample <- markSample
      case sample of
        SampleOK -> gen
        SampleTooBig -> boltzmannFail >> genBoltzmann tcs env
 where
   initializeBoltzmann = do
--     io $ putStr $ "Intializing sample ... "
     modify (\st -> st { boltzmannSamplerStatus = InitializedSampler })
     modify (\st -> st { boltzmannSamplerSize = 0 })
--     io $ putStrLn $ "done"
     
   -- failed to generate a value, reset the sampler
   boltzmannStop :: IM ()
   boltzmannStop = do
     io $ putStr $ "\tStopping Boltzmann ... "
     modify (\st -> st { boltzmannSamplerStatus = UnitializedSampler })
     modify (\st -> st { boltzmannSamplerSize = 0 })
     io $ putStrLn $ "done"
   
   boltzmannFail = boltzmannStop
   boltzmannSucess = boltzmannStop
     
   gen :: IM Value 
   gen = do
     -- 1. Pick a type constructor `tc` between the list of type constructors `tcs`
     -- The type picked might have a list of type parameters expected_types or not. 
     -- 2. Generate random values for every expected type
     
     tycon@(MkDataCon tycon_id expected_types) <- pickTypeConstructor tcs          
     
     let pp id = drop (1 + (last $ findIndices (== '.') id))  id -- get lastname
     io $ putStrLn $ pp tycon_id -- print tycon
     
     valSize <- gets boltzmannSamplerSize
     
     -- 2. we might have already failed – in that case we'll report the impossible.

     io $ putStrLn $ "tyConMkRandom for size " ++ (show valSize)
     status <- gets boltzmannSamplerStatus
     case status of 
       UnitializedSampler -> return . Wrong $ "The impossible happened: BoltzmannSampler is not initialized"
       _ -> do         
       io $ putStrLn $ " Found " ++ (show status)

       -- mkRandomCheckBoltzmann :: Ty -> IM (Maybe Value)
       let mkRandomCheckBoltzmann ty = do
             status <- gets boltzmannSamplerStatus
             io $ putStrLn $ "Making pointer, found " ++ (show status)
             case status of
               UnitializedSampler -> return Nothing
               InitializedSampler -> do
                 io $ putStrLn $ " Making new pointer for size " ++ (show valSize)
                 io $ putStrLn $ " tyRndValPtr of " ++ (show ty)
                 val <- mkRandomVal env ty 
                 (showValue val) >>= \sv -> io $ putStrLn $ "\ttyConMkRandom for size " ++ (show valSize) ++ " => " ++ sv
                 return . Just $ val
                     
       randomVals <- mapM mkRandomCheckBoltzmann expected_types
       
       let 
         checkedRandomVals = catMaybes randomVals
         shouldProceed = (length expected_types) == (length checkedRandomVals)
       case shouldProceed of
         False -> return . Wrong $ "The impossible happened: BoltzmannSampler is not initialized"
         True -> mapM mkValuePointer checkedRandomVals >>= return . TyConApp tycon 
     
markSample :: IM BoltzmannSample
markSample = do  
  maxSize <- getSetting data_max_size
  currentSize <- gets boltzmannSamplerSize
--  io $ putStrLn $ "Updating boltzmann, size= " ++ show (currentSize)
  
  -- if the size is greater, don't generate further
  case (currentSize >= maxSize) of
    True -> do
      io $ putStrLn "TooBig"
      return $ SampleTooBig
    False -> do 
      modify $ \st -> st {boltzmannSamplerSize = currentSize + 1}
      --io $ putStrLn $ "Increased size (OKBoltzmannStep)"
      io $ putStr $ " OK, size = " ++ (show currentSize) ++ " ... "
      return $ SampleOK
      
data BoltzmannSample = SampleOK | SampleTooBig
   
 -- guar
    
--   siz <- gets gen_val_size  
--   maybeGenVal <- (genBoltzmannLB tcs env) --gets gen_val
-- --  maybeGenVal <- gets gen_val
--   io $ putStr $ "Evaluating genBoltzmann at size= " ++ (show siz) ++ " ..."
--   case maybeGenVal of
--     Nothing -> do
--       io $ putStrLn "Got nothing, will reset .."
--       resetSize 
--       genBoltzmann tcs env
--     (Just (Wrong "Too big")) ->  do
--       io $ putStrLn $ "We're settling again."
--       resetSize
--       genBoltzmann tcs env
--     (Just v) -> do
--       size' <- gets gen_val_size
--       io $ putStrLn $ "Got something of size " ++ (show size')
--       return v
--       --genBoltzmann tcs env 
-- --- (genBoltzmannLB tcs env) `mplus` (genBoltzmann tcs env)
--   where
--     resetSize :: IM ()
--     resetSize = do
--       io $ putStrLn "Resetting size to 0"
-- --      modify (\st -> st { gen_val = Just (Wrong "")})
--       modify (\st -> st { gen_val_size = 0 })
  
-- | Creates a value using a type constructor, exhausting every type argument
-- an environment might be needed in case the types in the type constructors
-- contain references to some data type in the heap as an identifier
-- TODO: MkDataCon should contain a list of ConcreteType and no Ty's
tyConMkRandomX :: DataCon -> Env -> IM Value
tyConMkRandomX dc@(MkDataCon id []) env = return $ TyConApp dc []
tyConMkRandomX dc@(MkDataCon id tys) env = do
  io $ putStrLn $ "tyConMkRandom"
  --ptrs <- mapM (flip tyRndValPtr env) tys -- :: [Pointer] where the generated random vals are
  ptrs <- mapM (\ty -> do
                   io $ putStrLn $ " Making new pointer"
                   tyRndValPtr ty env) tys -- :: [Pointer] where the generated random vals are
  return $ TyConApp dc ptrs

-- | Makes a random value from a type and returns a pointer to it
tyRndValPtr :: Ty -> Env -> IM Pointer
tyRndValPtr ty env = do
  -- if the boltzmann sampler is unitialized, wait for it to be initialized
  -- so we can actually have permission to make any type  
  io $ putStrLn $ "tyRndValPtr of " ++ (show ty)
  val <- mkRandomVal env ty
  heap_ref@(_,addr) <- memorizeVal val
  return . MkPointer $ addr

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
