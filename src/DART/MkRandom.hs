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
import Language.Core.Ty(printSignature)

--------------------------------------------------------------------------------
-- Prelude
import Data.Char(chr)
import Data.List((!!), findIndices)
import Data.Maybe(catMaybes)
          
-- | When a TypeConstructor is applied to another TypeConstructor we get another TypeConstructor. 
-- This function reduces the argument type constructor to a type to form a DataCon
applyTyCon :: Value -> Value -> IM [DataCon]
applyTyCon (TyCon dc ty1)  (TyCon dc2 ty2) = case dc of
  MkDataCon dc_id expected@(ty:[]) -> do
    io . putStrLn . show $ dc_id
    io $ putStrLn $ show ty2
    --if we don't know ty.. we safely replace it with ty2
    return $ [dc { tyConExpectedTys = [Tvar(ty2)]}]
  _ -> return $ error "TODO"

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

-- | Generates a random char
-- From Data.Char's documentation: The character type Char is an enumeration whose values represent Unicode (or equivalently ISO/IEC 10646) characters (see http://www.unicode.org/ for details). This set extends the ISO 8859-1 (Latin-1) character set (the first 256 characters), which is itself an extension of the ASCII character set (the first 128 characters). A character literal in Haskell has type Char.
-- To convert a Char to or from the corresponding Int value defined by Unicode, use toEnum and fromEnum from the Enum class respectively (or equivalently ord and chr).

rndChar :: IM Char
rndChar = 
  let
    min_int_bound = 1
    max_int_bound = 1112064
  in do
    int <- io $ getStdRandom (randomR (min_int_bound, max_int_bound))
    return . chr $ int

data BoltzmannSample = SampleOK | SampleTooBig

-- | Given a list of type constructors for a data type, generate a 
-- new value using a boltzmann sampler around a size 
-- For an example of this technique, see Brent Yorgey's post, which inspired this smapler: http://byorgey.wordpress.com/2013/04/25/random-binary-trees-with-a-size-limited-critical-boltzmann-sampler-2/

genBoltzmann :: [DataCon] -> Env -> IM Value
genBoltzmann tcs env = do
  io $ putStr "Called boltzman ..." 
  status <- gets boltzmannSamplerStatus
  case status of
    -- initializes sampler, fails if sample is too small
    UnitializedSampler -> do
      -- initialize
      --io $ putStrLn $ "\tgenerating new ..."      
      modify (\st -> st { boltzmannSamplerStatus = InitializedSampler })
      modify (\st -> st { samplerDataSize = 0 })
      
      value' <- genBoltzmann tcs env
      
      valSize <- gets samplerDataSize
      minSize <- getSetting data_min_size
           
      io $ putStr $ "Generated boltzmann of size " ++ (show valSize)
  
      case (valSize >= minSize) of
        True -> do
          io $ putStrLn $ " ... good, of size = " ++ (show valSize)
          (showValue value') >>= \v -> io $ putStrLn $ "VALUE = "  ++ v
          
          --boltzmannSucess
          return value'
        _ -> do
          io $ putStrLn " ... failed, too small" 
          boltzmannFail >> genBoltzmann tcs env
          
    -- fails if sample size is too big
    InitializedSampler -> do
      io $ putStr $ "\tdoing step ... "
      sample <- markSample
      case sample of
        SampleOK -> mkSample tcs env
        SampleTooBig -> boltzmannFail >> genBoltzmann tcs env
 where     
   -- failed to generate a value, reset the sampler
   boltzmannStop :: IM ()
   boltzmannStop = do
     io $ putStr $ "\tStopping Boltzmann ... "
     modify (\st -> st { boltzmannSamplerStatus = UnitializedSampler })
     modify (\st -> st { samplerDataSize = 0 })
     io $ putStrLn $ "done"
   
   boltzmannFail = modify (\st -> st { boltzmannSamplerStatus = UnitializedSampler })
     
   boltzmannSucess = boltzmannStop
     
  
mkRandomCheckBoltzmann :: Env -> Ty -> IM (Maybe Value)
mkRandomCheckBoltzmann env ty = do
  status <- gets boltzmannSamplerStatus
  valSize <- gets samplerDataSize
  
  io $ putStrLn $ "Making pointer, found " ++ (show status)
  case status of
    UnitializedSampler -> return Nothing
    InitializedSampler -> do
      io $ putStrLn $ " Making new pointer for size " ++ (show valSize)
      -- io $ putStrLn $ " tyRndValPtr of " ++ (show ty)
      maybeVal <- mkRandomVal env ty 
      case maybeVal of
        Wrong e -> error e
        val -> do
          -- (showValue val) >>= \sv -> io $ putStrLn $ "\ttyConMkRandom for size " ++ (show valSize) ++ " => " ++ sv
          return . Just $ val      

-- | 1. Pick a type constructor `tc` between the list of type constructors `tcs` 
-- 2. The type constructor `tc` expects a list of types, for which we generate random values
mkSample :: [DataCon] -> Env -> IM Value 
mkSample tcs env = do     
     tycon@(MkDataCon tycon_id expected_types) <- pickTypeConstructor tcs     
     let pp id = drop (1 + (last $ findIndices (== '.') id))  id -- get lastname     
     io $ putStrLn $ pp tycon_id -- print tycon     
     
     -- 2. we might have already failed – in that case we'll report the impossible.
     --io $ putStrLn $ "tyConMkRandom for size " ++ (show valSize)
     
     status <- gets boltzmannSamplerStatus
     case status of 
       UnitializedSampler -> return . Wrong $ "The impossible happened: BoltzmannSampler is not initialized"
       _ -> do
       
       -- io $ putStrLn $ " Found " ++ (show status)
                 
       io $ putStrLn $ "Making values for " ++ (show . length $ expected_types) ++ " expected types"
       randomVals <- mapM (mkRandomCheckBoltzmann env) expected_types
       
       let 
         checkedRandomVals = catMaybes randomVals
         shouldProceed = (length expected_types) == (length checkedRandomVals)
       case shouldProceed of
         False -> return . Wrong $ "The impossible happened: BoltzmannSampler is not initialized"
         True -> mapM mkValuePointer checkedRandomVals >>= return . TyConApp tycon 
     
markSample :: IM BoltzmannSample
markSample = do  
  maxSize <- getSetting data_max_size
  currentSize <- gets samplerDataSize

  -- if the size is greater, don't generate further
  case (currentSize >= maxSize) of
    True -> do
      io $ putStrLn "TooBig"
      return $ SampleTooBig
    False -> do 
      modify $ \st -> st {samplerDataSize = currentSize + 1}
      --io $ putStrLn $ "Increased size (OKBoltzmannStep)"
      io $ putStr $ " OK, size = " ++ (show currentSize) ++ " ... "
      return $ SampleOK
      

