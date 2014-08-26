----------------------------------------------------------------------------
-- |
-- Module      :  Language.Core.Interpreter.Structures
-- Copyright   :  (c) Carlos López-Camey, University of Freiburg
-- License     :  BSD-3
--
-- Maintainer  :  c.lopez@kmels.net
-- Stability   :  stable
--
--
-- Defines fundamental structures and functions for Language.Core.Interpreter
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}

-- TODO: Move to Language.Core
module Language.Core.Interpreter.Structures(
  io
  , increase_number_of_reductions
  -- heap operations
  , store, newAddress, memorize, memorizeVal, memorizeThunk, mkVal, mkThunk, mkHeapReference, mkValuePointer
  , allocate
  -- timeouting
  , isTimeout, clearTimeout
  -- pretty printing
  -- settings
  , getSetting
  , DARTSettings(..)
  , DARTState(..)
  , Heap, Env, TypeEnv, HeapAddress, HeapReference
  , IM
  , Thunk (..), DataCon(..) , Value(..), Pointer(..), PredicateBranch(..)
--  , ModuleFunction(..)
  , HaskellExpression(..)
  , BoltzmannSamplerStatus(..)
  , module Control.Monad.State.Lazy
  , module Language.Core.Core
  , module Language.Core.Util
) where

--------------------------------------------------------------------------------
-- control
import           Control.Monad.Primitive
import           Control.Monad.State.Lazy
--------------------------------------------------------------------------------
-- base type funs
import           Data.Either(partitionEithers,rights)
import           Data.List(intersperse)
import           Data.Char (isUpper)
import           Data.List(findIndices)
--------------------------------------------------------------------------------
-- DART
import           DART.DARTSettings
-- Language.Core
import           Language.Core.Core
import           Language.Core.Ty(showTySig)
import           Language.Core.Util --(showType,showExtCoreType,showExp,wrapName)
import           System.IO.Unsafe(unsafePerformIO)
--------------------------------------------------------------------------------
-- System
import           Data.Time.Clock
import           Unsafe.Coerce(unsafeCoerce)
--------------------------------------------------------------------------------

-- mutable hash tables; 
-- package [hashtables](http://hackage.haskell.org/package/hashtables)
import qualified Data.HashTable.IO as H
  
-- | A State passed around the interpreter
data DARTState = DState {  
  -- benchmarking
 benchmarks :: [(Id,NominalDiffTime)]
 , libraries_env :: Env
 , heap :: Heap -- our memory 
 , pbranches_record :: [PredicateBranch] -- also keep Env, but without libs; only stuff within the scope.
 , heap_count :: Int, -- address counter, counts those previously deleted too
                    -- useful to generate new variable names
 number_of_reductions :: !Int, -- when an expression is evaluated, this value increments by 1, useful to print debug headings
 number_of_reductions_part :: !Int -- when in debugging mode, this value is increased everytime an step is done in the evaluation of the expression represented by the number `number_of_reductions`, prints debug subheadings
 , tab_indentation :: !Int -- useful when to know how many tabs we shoud prepend
 , settings :: DARTSettings
 
   -- time when a computation started
   -- useful for timeouts
 , start_time :: UTCTime
 
 -- state of testing
 , test_name :: Maybe (Qual Var) 
-- , generator :: GenM Value
 , samplerStatus :: BoltzmannSamplerStatus
 , samplerDataSize   :: Int
 , samplerValue   :: Value
-- , runSampler :: Boltzmann Value
}

--newtype Boltzmann a {
--  runBoltzmann :: DARTState -> IM (a, DARTState)
--}

data BoltzmannSamplerStatus = InitializedSampler | UnitializedSampler deriving Show

type Heap = H.CuckooHashTable HeapAddress (Either Thunk Value)
type HeapAddress = Int
type Env = [(Id,HeapAddress)]
type TypeEnv = [(Id,Ty)]
type HeapReference = (Id,HeapAddress)

-- | We'll record execution paths; every path node comes from a branching after a pattern match analysis is performed. See method recordBranch in Language.Core.Interpterer.Evaluable
data PredicateBranch = PBranch {
  pbranch_exp :: Exp,
  pbranch_value :: Value
  } | EnvironmentalPBranch {
     pbranch_id :: Id,
     pbranch_env :: Env,
     pbranch_value :: Value
  }

-- --
-- fun2 = let
--   a = error $ "ERROR"
--   b = 2
--   in a `seq` b
   
-- | Define a monad IM (for Interpreter Monad) where we keep a value of type DARTState containing state variables such as the heap and settings such as the number of reduction for debugging purposes. 
type IM = StateT DARTState IO 

data Value = Wrong String
           | Num Integer
           | Rat Rational -- arbitrary-precision rational numbers
           | Boolean Bool
           | Char Char
           | String String
           | Fun (Id -> Env -> IM Value) Description
           | Pair Pointer Pointer --HERE, heap addresses
           | TyConApp DataCon [Pointer] -- a data constructor applicated to some values, possible expecting some more types
           | Pointer Pointer
           | FreeTypeVariable String -- useful when converting a to SomeClass a (we ignore parameters, and it's useful to save them)
           | MkListOfValues [(String,Value)] -- When a value definition is recursive, depends on other values
           | SumType [DataCon] -- A data type with reference to its constructors, created only from type constructors when reading modules (see Interpreter/Acknowledge).
           | TyCon DataCon Id -- data constructor annotated with the qualified name of the type it builds. For example (:) is a type constructor for the list type, "[]".
           
newtype Pointer = MkPointer { ptr_address :: HeapAddress } deriving Show

data Thunk = Thunk Exp Env -- a thunk created during the evaluation of a value definition
--           | VdefgThunk Exp -- has no environment, it will be passed by the module for efficiency
instance Show Thunk where 
  show (Thunk exp env) = let ?tab_indentation = 0 
                         in "Thunk(exp=" ++ showExp exp ++ ")"
  --show (VdefgThunk exp) = let ?tab_indentation = 0 
  --                        in "VdefgThunk(exp=" ++ showExp exp ++ ")"


-- | Some expression from the command line that is evaluable within the scope of the
-- provided file(s)
data HaskellExpression = HaskellExpression String Module

-- | A polykinded type constructor
data DataCon = MkDataCon {
  datacon_name :: Id, -- qualified name
  signature :: [Ty], --types it expects
  context :: TypeEnv --bounded types
  } deriving Eq

type Description = String

-- | Equality of values
instance Eq Value where 
  (Wrong s) == Wrong s' = s == s'
  (Num i) == (Num i') = i == i'
  (Fun _ _) == (Fun _ _) = False  -- equality of functions in dart is not intensional
  (Boolean b) == (Boolean b') = b == b'
  o == p = False
  
-- showAddress :: HeapAddress -> IM String
-- showAddress address = lookupMem address >>= \v -> case v of
--   Left thunk -> return "Thunk"
--   Right val -> case val of
--     -- look for functions that depend on an address, computations to happen within the IM monad
--     (TyConApp tc addresses) -> showTyConApp tc addresses        
--     otherVal -> return $ show otherVal -- wrong, num, string, fun, etc..

io :: MonadIO m => IO a -> m a
io = liftIO

-- | Make a thunk of an expression so it can be later evaluated
mkThunk :: Exp -> Env -> Either Thunk Value
mkThunk exp env = Left $ Thunk exp env

-- | Lift a value to memory value
mkVal :: Value -> Either Thunk Value
mkVal = Right

-- | Stores a value or a thunk in the given address
store :: HeapAddress -> Either Thunk Value -> Id -> IM HeapReference
store address val id  = do
  -- put the value in the heap
  h <- gets heap
  io $ H.insert h address val
  --watchReductionM $ "Memorized " ++ id ++ " in " ++ show address ++ " as " ++ show val
  return (id,address)


-- | Stores a value or a thunk in a new address
memorize :: Either Thunk Value -> Id -> IM HeapReference
memorize val id = newAddress >>= \adr -> store adr val id 

-- | Makes a Pointer from
mkValuePointer :: Value -> IM Pointer
mkValuePointer val = memorizeVal val >>= \heap_ref@(_,addr) -> return . MkPointer $ addr

memorizeVal :: Value -> IM HeapReference
memorizeVal val = mkVarName >>= memorize (mkVal val)

memorizeThunk :: Thunk -> IM HeapReference
memorizeThunk thunk = mkVarName >>= memorize (Left thunk)

--memorizeThunkAs :: Thunk -> Id -> IM HeapReference
--memorizeThunkAs thunk id = newAddress >>= \adr -> memorize adr (Left thunk) id

-- | Creates new variable for the expression, memorizes it and returns a heap reference
mkHeapReference :: Exp -> Env -> IM HeapReference
mkHeapReference exp env = mkVarName >>= memorize (mkThunk exp env)

-- | Gets a new address, that stores nothing
newAddress :: IM HeapAddress
newAddress = incVarCount >> gets heap_count

incVarCount :: IM ()
incVarCount = gets heap_count >>= \hc -> modify (\st -> st { heap_count = hc + 1 })

-- | Allocates `n` new addresses
allocate :: Int -> IM [HeapAddress]
allocate 0 = return []
allocate 1 = newAddress >>= return . mkList where mkList x = [x]
allocate n = do
  a <- newAddress
  as <- allocate $ n - 1
  return $ a:as

-- | Prints a debug message with a new line at the end
debugM :: String -> IM ()
debugM msg = do 
  s <- gets settings
  ti <- gets tab_indentation  
  when (debug s) $
    let tab = replicate ti '\t' 
    in io . putStrLn $ (tab ++ msg) 
    
-- | Creates a variable name. This function is not exported since every time it is used,
-- the variable count should be increased (done normally by memorize)
mkVarName :: IM String
mkVarName = gets heap_count >>= return . (++) "dartTmp" . show 

increase_number_of_reductions :: DARTState -> DARTState
increase_number_of_reductions s = s { number_of_reductions = number_of_reductions s + 1 }


instance Show Value where
  show (Wrong s) = "error: " ++ s
  show (Num i) = show i
  show (Rat r) = show r
  show (Boolean b) = show b  
  show (Char c) = show c
  show (String s) = show s
  show (Pair a b) = show (a,b)
  -- If the function description is prepended by a $, it's a monomophied function, we should everything until we find the last upper case letter
  show (Fun f ('$':s)) = 
    let lastUpperIndex = last . findIndices isUpper
    in  drop (lastUpperIndex s) s
    
  show (Fun f s) = s  
  show (TyConApp tc addresses) = "TyConApp(" ++ show tc ++ ", " ++ show addresses ++ ")"
  show (Pointer address) = "Pointer to " ++ show address
  show (FreeTypeVariable type_var) = type_var
  show (MkListOfValues vals) = let
    myIntersperse sep = foldr ((++) . (++) sep) []
    in myIntersperse "\n\t" (map show vals)
  show (SumType cons) = "SumType of " ++ myIntersperse "|" constructor_names
    where
      myIntersperse sep = foldr ((++) . (++) sep) []
      constructor_names = map (\(MkDataCon id _ _) -> id) cons
  show (TyCon tycon ty_name) | show tycon == "[]" = "[]"
  show (TyCon (MkDataCon datacon_name' datacon_signature' []) ty_name) = (idName datacon_name') ++ " :: " ++ (showTySig datacon_signature') ++ " ::=> " ++ (idName ty_name)
  show (TyCon (MkDataCon datacon_name' datacon_signature' ty_env) ty_name) = "... —" ++ (idName datacon_name') ++ " :: " ++ (showTySig datacon_signature') ++ " ::=> " ++ (idName ty_name) ++ ", –– applied types: " ++ (concatMap (\(i,t) -> show i ++ showType t) ty_env)
  
--    | otherwise = "TypeConstructor " ++ show tycon ++ " of " ++ ty_name

-- Pretty print data constructors
instance Show DataCon where
  -- list?
  show (MkDataCon "ghc-prim:GHC.Types.[]" [] _) = "[]"
  show (MkDataCon "ghc-prim:GHC.Types.[]" ty _) = "[] :: " ++ show ty
  show (MkDataCon id [] _) = idName id
  show (MkDataCon id signature' applied_types') = idName id ++ ", signature = " ++ (show signature'') ++ ", applied types = " ++ show (applied_types') where
    signature'' :: String
    signature'' | length signature' == 1 = show signature' 
           | otherwise = concatMap (\t -> show t ++ " -> ") signature'
           

-- | Take a qualified name and return only its last name. E.g. idName "main.Module.A" = "A"
idName :: Id -> String
idName id = let 
  name = drop (lastDotIndex id + 1) id 
  in case name of 
    ":" -> "(:)"
    _ -> name
  where
    isDot = ((==) '.')
    dotIndexes = findIndices isDot
    lastDotIndex = last . dotIndexes
    
-- | Re computes the start time of a DARTState, should be called when
-- a new computation is going to begin
clearTimeout :: IM () 
clearTimeout = io getCurrentTime >>= \t -> modify (\st -> st { start_time = t })

-- | Checks whether the computations is taking long enough in order
-- to stop. 
isTimeout :: IM Bool
isTimeout = do
  st <- gets start_time
  now <- io getCurrentTime
  max_timeout <- getSetting max_time_per_function
  let dayTime = now `diffUTCTime` st
      passed  = now `diffUTCTime` st
      nominal_max = fromInteger $ (unsafeCoerce max_timeout :: Integer)
  return $ (passed > nominal_max)
  
getSetting :: (DARTSettings -> a) -> IM a 
getSetting f = gets settings >>= return . f
