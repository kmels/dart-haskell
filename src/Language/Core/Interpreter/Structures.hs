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
-- Defines fundamental structures and functions on them intendeted for Language.Core.Interpreter
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}

module Language.Core.Interpreter.Structures(
  io
  , increase_number_of_reductions
  -- heap operations
  , store, newAddress, memorize, memorizeVal, memorizeThunk, mkVal, mkThunk, mkHeapReference
  , allocate
  -- timeouting
  , isTimeout, clearTimeout
  -- pretty printing
  -- settings
  , getSetting
  , DARTSettings(..)
  , DARTState(..)
  , Heap, Env, HeapAddress, HeapReference
  , IM
  , Thunk (..), DataCon(..) , Value(..), Pointer(..)
  , Language.Core.Core.Id
--  , ModuleFunction(..)
  , HaskellExpression(..)
  , module Control.Monad.State
  , module Language.Core.Core
  , module Language.Core.Util
) where

--------------------------------------------------------------------------------
-- control
import           Control.Monad.Primitive
import           Control.Monad.State
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
import           Language.Core.Util --(showType,showExtCoreType,showExp,wrapName)
import           System.IO.Unsafe(unsafePerformIO)
--------------------------------------------------------------------------------
-- System
import           Data.Time.Clock
--------------------------------------------------------------------------------

-- mutable hash tables; 
-- package [hashtables](http://hackage.haskell.org/package/hashtables)
import qualified Data.HashTable.IO as H

-- | A State passed around the interpreter
data DARTState = DState {  
  -- benchmarking
 benchmarks :: [(Id,NominalDiffTime)]
 
 , heap :: Heap, -- our memory 
 heap_count :: Int, -- address counter, counts those previously deleted too
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
 
}

type Heap = H.CuckooHashTable HeapAddress (Either Thunk Value)
--type HeapTypes = H.CuckooHashTable HeapAddress Type
--type Heap = H.CuckooHashTable HeapAddress (GeneralType)
type HeapAddress = Int
type Env = [(Id,HeapAddress)]
type HeapReference = (Id,HeapAddress)

-- | Define a monad IM (for Interpreter Monad) where we keep a value of type DARTState containing state variables such as the heap and settings such as the number of reduction for debugging purposes. 

type IM = StateT DARTState IO 

data Value = Wrong String
           | Num Integer
           | Rat Rational -- arbitrary-precision rational numbers
           | Boolean Bool
           | Char Char
           | String String
           | Fun (Id -> Env -> IM Value) Description
           -- |  List [Value]
           | Pair Pointer Pointer --HERE, heap addresses
           | TyConApp DataCon [Pointer] -- heap addresses, a type constructor application to some values
           | Pointer Pointer
           | FreeTypeVariable String -- useful when converting a to SomeClass a (we ignore parameters, and it's useful to save them)
           | MkListOfValues [(String,Value)] -- When a value definition is recursive, depends on other values
           | SumType [DataCon] -- A data type with reference to its constructors, created only from type constructors when reading modules (see Interpreter/Acknowledge).

newtype Pointer = MkPointer { ptr_address :: HeapAddress } deriving Show

data Thunk = Thunk Exp Env -- a thunk created during the evaluation of a value definition
--           | VdefgThunk Exp -- has no environment, it will be passed by the module for efficiency
instance Show Thunk where 
  show (Thunk exp env) = let ?tab_indentation = 0 
                         in "Thunk(exp=" ++ showExp exp ++ ")"
  --show (VdefgThunk exp) = let ?tab_indentation = 0 
  --                        in "VdefgThunk(exp=" ++ showExp exp ++ ")"

--data ModuleFunction = ModuleFunction Vdef Module

-- | Some expression from the command line that is evaluable within the scope of the
-- provided file(s)
data HaskellExpression = HaskellExpression String Module
-- cons = TyConApp (MkDataCon "Cons" [a]) [1,Nil]

-- RENAME THIS (DataCon)

-- | A data type constructor that has normally a qualified name and a list of
-- types that it expects. 
data DataCon = MkDataCon {
  dataConId :: Id,
  dataConTys :: [Ty]
  }

type Description = String

instance Eq Value where 
  (Wrong s) == Wrong s' = s == s'
  (Num i) == (Num i') = i == i'
  (Fun _ _) == (Fun _ _) = False  -- too bad we are not intensional as in intensional type equality
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

mkThunk :: Exp -> Env -> Either Thunk Value
mkThunk exp env = Left $ Thunk exp env

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
      constructor_names = map (\(MkDataCon id _) -> id) cons

instance Show DataCon where
  show (MkDataCon id []) = idName id
  show (MkDataCon id types) = idName id ++ " :: " ++ types' where
    types' :: String
    types' | length types == 1 = show types 
           | otherwise = concatMap (\t -> show t ++ " -> ") types
           

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
  max_timeout <- getSetting timeout_seconds
  let dayTime = now `diffUTCTime` st
  return $ (now `diffUTCTime` st > fromInteger max_timeout)
  
getSetting :: (DARTSettings -> a) -> IM a 
getSetting f = gets settings >>= return . f
