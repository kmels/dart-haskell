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
-- Language Core
import Language.Core.Interpreter.Structures
import Language.Core.Interpreter
import Language.Core.Interpreter.Util(showValue)

--------------------------------------------------------------------------------
-- DART
import DART.CmdLine(debugM)
import Language.Core.Ty(printSignature)

--------------------------------------------------------------------------------
-- Prelude
import Data.Char(chr)
import Data.List((!!), findIndices)
import Data.Maybe(catMaybes)
          
-- | When a TypeConstructor is applied to another TypeConstructor we get another TypeConstructor. 
-- This function reduces the argument type constructor to a type to form a DataCon
applyTyCon :: Value -> Value -> Ty -> IM [DataCon]
applyTyCon (TyCon dc1 ty1)  (TyCon dc2 ty2) this_type = 
  case dc1 of
    MkDataCon "ghc-prim:GHC.Types.[]" _ tys -> do
      let 
        cons_name = "ghc-prim:GHC.Types.:"
        type_of_the_list = Tvar $ ty2
        dcc = [
            dc1 { signature = [type_of_the_list], applied_types = (this_type:tys)}, -- nil
            MkDataCon cons_name [type_of_the_list, this_type] (this_type:tys) 
            ]
      return dcc
      
    MkDataCon dc1_id expected@(ty:[]) tys -> do
      io . putStrLn . show $ dc1_id
      io $ putStrLn $ show ty2
      --if we don't know ty.. we safely replace it with ty2
      let dcc = [dc1 { signature = [Tvar(ty2)], applied_types = (this_type:tys)}]
      
      io $ putStrLn $ show dcc
      return dcc
    --_ -> return $ error "TODO"

-- | Given a type in external core, randomly produce a value of its type. An environment might be needed in case there is a reference to the heap as an identifier in e.g. a data type may contain its type constructors.
mkRandomVal :: Env -> Ty -> IM Value
mkRandomVal env ty@(Tvar tyname) = do
  case tyname of
    "ghc-prim:GHC.Types.Char" -> rndChar >>= return . Char
    _ -> return . Wrong $ "mkRandomVal: I don't know how to make a random val for the type " ++ showExtCoreTypeVerbose ty


mkRandomVal env (Tcon qual_tcon)  = do
  io $ putStr "mkRandomVal .. "
  case zDecodeQualified qual_tcon of
    "ghc-prim:GHC.Types.Int" ->  -- a random integer
      rndInt >>= return . Num . toInteger
    
    -- found an identifier, fetch type constructors
    id -> do
      type_constructors <- fetchDataCons id env
      genBoltzmann type_constructors env -- get a value using our boltzmann sampler

-- application of a type constructor qtycon1 to qtycon2
mkRandomVal env this_type@(Tapp (Tcon zqtycon1) (Tcon zqtycon2)) = do
  let
    qtycon_1 = zDecodeQualified zqtycon1
    qtycon_2 = zDecodeQualified zqtycon2    
  
  -- we should have info about `qtycon1`
  tycon1 <- fetchTyCon qtycon_1 env
  tycon2 <- fetchTyCon qtycon_2 env
  
  datacons <- applyTyCon tycon1 tycon2 this_type
  genBoltzmann datacons env

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

  
-- | Creates a value using a type constructor, exhausting every type argument
-- an environment might be needed in case the types in the type constructors
-- contain references to some data type in the heap as an identifier
-- TODO: MkDataCon should contain a list of ConcreteType and no Ty's
tyConMkRandomX :: DataCon -> Env -> IM Value
tyConMkRandomX dc@(MkDataCon id [] _) env = return $ TyConApp dc []
tyConMkRandomX dc@(MkDataCon id tys _) env = do
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

-- | Generates a random char
-- From Data.Char's documentation: The character type Char is an enumeration whose values represent Unicode (or equivalently ISO/IEC 10646) characters (see http://www.unicode.org/ for details). This set extends the ISO 8859-1 (Latin-1) character set (the first 256 characters), which is itself an extension of the ASCII character set (the first 128 characters). A character literal in Haskell has type Char.
-- To convert a Char to or from the corresponding Int value defined by Unicode, use toEnum and fromEnum from the Enum class respectively (or equivalently ord and chr).

rndChar :: IM Char
rndChar = 
  let
    min_int_bound = 32 -- 1
    max_int_bound = 126 -- 1112064
  in do
    int <- io $ getStdRandom (randomR (min_int_bound, max_int_bound))
    return . chr $ int

data BoltzmannSample = SampleOK | SampleTooBig

-- | Given a list of type constructors for a data type, generate a 
-- new value using a boltzmann sampler around a size 
-- For an example of this technique, see Brent Yorgey's post, which inspired this smapler: http://byorgey.wordpress.com/2013/04/25/random-binary-trees-with-a-size-limited-critical-boltzmann-sampler-2/

genBoltzmann :: [DataCon] -> Env -> IM Value
genBoltzmann tcs env = do
  debugM $ "Sampling with " ++ (show . length $ tcs) ++ " type constructors; " ++ (show tcs)
  status <- gets samplerStatus
  case status of
    -- initializes sampler, fails if sample is too small
    UnitializedSampler -> do
      -- initialize
      --io $ putStrLn $ "\tgenerating new ..."      
      modify (\st -> st { samplerStatus = InitializedSampler })
      modify (\st -> st { samplerDataSize = 0 })
      
      value' <- genBoltzmann tcs env
      
      sampleSize <- gets samplerDataSize
      minSize <- getSetting data_min_size
           
      debugM $ "Sampler generated value of size " ++ (show sampleSize)
  
      case (sampleSize >= minSize) of
        True -> do
          (showValue value') >>= \v -> io $ putStrLn $ "VALUE = "  ++ v
          
          boltzmannStop -- don't make more data
          gets samplerValue >>= return
        _ -> do
          debugM $ " Sample too small, size = " ++ (show sampleSize)
          boltzmannFail >> genBoltzmann tcs env
          
    -- fails if sample size is too big
    InitializedSampler -> do
      --io $ putStr $ "\tdoing step ... "
      sample <- markSample
      sampleSize <- gets samplerDataSize
      case sample of
        SampleOK -> do
          debugM $ " OK, size = " ++ (show sampleSize)
          mkSample tcs env >> gets samplerValue
        SampleTooBig -> do
          debugM $ " Sample too big, size = " ++ (show sampleSize)
          boltzmannFail >> genBoltzmann tcs env
 where     
   -- failed to generate a value, reset the sampler
   boltzmannStop :: IM ()
   boltzmannStop = do
     --io $ putStr $ "\tStopping Boltzmann ... "
     modify (\st -> st { samplerStatus = UnitializedSampler })
     --io $ putStrLn $ "done"
   
   boltzmannFail = modify (\st -> st { samplerStatus = UnitializedSampler })
     
     
-- | 1. Pick a type constructor `tc` between the list of type constructors `tcs` 
-- 2. The type constructor `tc` expects a list of types, for which we generate random values
mkSample :: [DataCon] -> Env -> IM ()
mkSample tcs env = do
     tycon@(MkDataCon tycon_id expected_types _) <- pickTypeConstructor tcs     
     let prettyPrint id = drop (1 + (last $ findIndices (== '.') id))  id -- get lastname
     debugM $ "Sampler chose type constructor: " ++ (prettyPrint tycon_id)
     debugM $ "Sampler chose type constructor: " ++ (tycon_id)     
     debugM $ "Making values for " ++ (show . length $ expected_types) ++ " expected types"
     
     randomVals <- mkSampleData env expected_types

     case (length randomVals == length expected_types) of
       True -> do         
         sampleVal <- mapM mkValuePointer randomVals >>= return . TyConApp tycon
         modify (\st -> st { samplerValue = sampleVal })
       False -> return () -- extra calls... 

mkSampleData :: Env -> [Ty] -> IM [Value]
mkSampleData env [] = return []
mkSampleData env (ty:tys) = do
  v <- mkRandomVal env ty
  st' <- get
  case (samplerStatus st') of
    InitializedSampler -> do
      vs <- mkSampleData env tys
      return $ (v:vs)
    UnitializedSampler -> do
      return []

-- | Makes a random value for a type, with the sample size in mind
-- if the sample size is already too big (will fail since we already marked the sample)
-- don't make a value.
mkRandomCheckBoltzmann :: Env -> Ty -> IM (Maybe Value)
mkRandomCheckBoltzmann env ty = do
  sampleSize <- gets samplerDataSize
  io $ putStrLn $ " Making new pointer for size " ++ (show sampleSize)
  maybeVal <- mkRandomVal env ty
  -- if we fail, don't make more data
  status <- gets samplerStatus
  io $ putStrLn $ "Made random data, found state " ++ (show status)
  case status of
    UnitializedSampler -> gets samplerValue >>= return . Just
    InitializedSampler -> do            
      case maybeVal of
        Wrong e -> error e
        val -> do
          (showValue val) >>= \sv -> io $ putStrLn $ "\t data at size " ++ (show sampleSize) ++ " => " ++ sv
          return . Just $ val      

markSample :: IM BoltzmannSample
markSample = do  
  maxSize <- getSetting data_max_size
  currentSize <- gets samplerDataSize
  -- if the size is greater, don't generate further
  case (currentSize >= maxSize) of
    True -> do
      return $ SampleTooBig
    False -> do 
      modify $ \st -> st {samplerDataSize = currentSize + 1}
      return $ SampleOK
      

