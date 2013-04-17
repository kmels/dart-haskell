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
  , idName
  , increase_number_of_reductions
  -- heap operation
  , memorize, memorizeVal, mkVal, mkThunk, mkHeapReference, lookupMem
  -- pretty printing
  , showM
  , DARTState(..)
  , Heap, Env, HeapAddress, HeapReference
  , IM
  , Thunk (..), DataCon(..) , Value(..), Pointer(..)
  , Language.Core.Core.Id
  , ModuleFunction(..)
  , HaskellExpression(..)
  , module Control.Monad.State
  , module Language.Core.Core
  , module Language.Core.Util
) where

-- Prelude and control
import           Control.Monad.Primitive
import           Control.Monad.State
import           Data.Char (isUpper)
import           Data.Either(partitionEithers,rights)
import           Data.List(intersperse)
import           Data.List(findIndices)
import           Prelude hiding (showList)
-- DART
import           DART.InterpreterSettings
-- Language.Core
import           Language.Core.Core
import           Language.Core.Util --(showType,showExtCoreType,showExp,wrapName)
import           System.IO.Unsafe(unsafePerformIO)
-- mutable hash tables; 
-- package [hashtables](http://hackage.haskell.org/package/hashtables)
import qualified Data.HashTable.IO as H
--import qualified Data.HashTable.ST.Cuckoo as C

-- | A State passed around the interpreter
data DARTState = DState {
 heap :: Heap, -- our memory 
 heap_count :: Int, -- address counter, counts those previously deleted too
                    -- useful to generate new variable names
 number_of_reductions :: !Int, -- when an expression is evaluated, this value increments by 1, useful to print debug headings
 number_of_reductions_part :: !Int -- when in debugging mode, this value is increased everytime an step is done in the evaluation of the expression represented by the number `number_of_reductions`, prints debug subheadings
 , tab_indentation :: !Int -- useful when to know how many tabs we shoud prepend
 , settings :: InterpreterSettings
 
 -- state of testing
 , test_name :: Maybe (Qual Var) 
}

type Heap = H.CuckooHashTable HeapAddress (Either Thunk Value)
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

newtype Pointer = MkPointer { address :: HeapAddress } deriving Show

data Thunk = Thunk Exp Env -- a thunk created during the evaluation of a value definition
--           | VdefgThunk Exp -- has no environment, it will be passed by the module for efficiency
instance Show Thunk where 
  show (Thunk exp env) = let ?tab_indentation = 0 
                         in "Thunk(exp=" ++ showExp exp ++ ")"
  --show (VdefgThunk exp) = let ?tab_indentation = 0 
  --                        in "VdefgThunk(exp=" ++ showExp exp ++ ")"

data ModuleFunction = ModuleFunction Vdefg Module
-- | Some expression from the command line that is evaluable within the scope of the
-- provided file(s)
data HaskellExpression = HaskellExpression String Module
-- cons = TyConApp (MkDataCon "Cons" [a]) [1,Nil]

-- RENAME THIS (DataCon)

-- | A data type constructor that has normally a qualified name and a list of
-- types that it expects. 
data DataCon = MkDataCon Id [Ty]

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

-- | There are some values that contain addresses for which we must, in order to
-- pretty print the given value, look up their actual value in the heap
showM :: Value -> IM String
showM (TyConApp tc ptrs) = showTyConApp tc ptrs
showM val = return $ show val

instance Show Value where
  show (Wrong s) = "WRONG: " ++ s
  show (Num i) = show i
  show (Rat r) = show r
  show (Boolean b) = show b  
  show (Char c) = show c
  show (String s) = show s
  show (Pair a b) = show (a,b)
  show (Fun f ('$':s)) = drop (lastUpperIndex s) s where -- monomophied function, cut from the last upper case
    lastUpperIndex = last . findIndices isUpper
  show (Fun f s) = s  
--  show (List vs) = show vs  
  show (TyConApp tc addresses) = "TyConApp(" ++ show tc ++ ", " ++ show addresses ++ ")"
  show (Pointer address) = "Pointer to " ++ show address
  show (FreeTypeVariable type_var) = type_var
  show (MkListOfValues vals) = let
    myIntersperse sep = foldr ((++) . (++) sep) []
    in myIntersperse "\n\t" (map show vals)

instance Show DataCon where
  show (MkDataCon id []) = idName id
  show (MkDataCon id types) = idName id ++ " :: " ++ types' where
    types' :: String
    types' | length types == 1 = show types 
           | otherwise = concatMap (\t -> show t ++ " -> ") types

-- Take a qualified name and return only its last name. E.g. idName "main.Module.A" = "A"

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

wrapInParenthesis s = "(" ++ s ++ ")"

io :: MonadIO m => IO a -> m a
io = liftIO

mkThunk :: Exp -> Env -> Either Thunk Value
mkThunk exp env = Left $ Thunk exp env

mkVal :: Value -> Either Thunk Value
mkVal = Right

memorizeVal :: Value -> IM HeapReference
memorizeVal val = mkVarName >>= memorize (mkVal val)

-- | Stores a value/thunk in memory, returns the given name with the address where
-- it was stored
memorize :: Either Thunk Value -> Id -> IM HeapReference
memorize val id  = do
  -- Find the an allocation address
  hc <- gets heap_count
  let address = hc + 1
  modify (\st -> st { heap_count = address })
  
  -- Put the value in the heap
  h <- gets heap
  io $ H.insert h address val
  --watchReductionM $ "Memorized " ++ id ++ " in " ++ show address ++ " as " ++ show val
  return (id,address)

-- | Prints a debug message with a new line at the end
debugM :: String -> IM ()
debugM msg = do 
  s <- gets settings
  ti <- gets tab_indentation  
  when (debug s) $
    let tab = replicate ti '\t' 
    in io . putStrLn $ (tab ++ msg) 
    
-- | Creates new variable for the expression, memorizes it and returns a heap reference
mkHeapReference :: Exp -> Env -> IM HeapReference
mkHeapReference exp env  = mkVarName >>= memorize (mkThunk exp env)

-- | Creates a variable name
mkVarName :: IM String
mkVarName = gets heap_count >>= return . (++) "dartTmp" . show 

increase_number_of_reductions :: DARTState -> DARTState
increase_number_of_reductions s = s { number_of_reductions = number_of_reductions s + 1 }

showList :: [Either Thunk Value] -> String
showList elems = case partitionEithers elems of
  ([],[]) -> "" -- no thunks, no vals
  ([],(head:t:[])) -> "[" ++ show head ++ showTail t ++ "]" -- no thunks
  _ -> show elems
  where
    showTail :: Value -> String    
--    showTail (TyConApp (MkDataCon "ghc-prim:GHC.Types.:" _) ((Right th):(Right tt):[])) = "," ++ show th ++ showTail tt
--    showTail (TyConApp (MkDataCon "ghc-prim:GHC.Types.[]" _) []) = ""
    showTail w@(Wrong _) = "," ++ show w
    showTail xs = "????\t\t\t" ++ show xs ++ " \t\t\t"

showTyConApp :: DataCon-> [Pointer] -> IM String
showTyConApp tycon pointers = do
  values <- mapM lookupPtr pointers
  return $ showTyConVals tycon values
  where
    showTyConVals :: DataCon-> [Either Thunk Value] -> String
    showTyConVals (MkDataCon "ghc-prim:GHC.Types.[]" []) [] = "[]" -- empty list
    showTyConVals (MkDataCon "ghc-prim:GHC.Types.:" _) cns = showList cns -- lists
    showTyConVals (MkDataCon "ghc-prim:GHC.Tuple.Z2T" _) [x,y] = show (x,y) -- tuples
    -- otherwise
    showTyConVals (MkDataCon tycon_name []) vals = idName tycon_name ++ " " ++ showVals vals
    showTyConVals (MkDataCon tycon_name _) vals = idName tycon_name ++ " " ++ showVals vals

showVals :: [Either Thunk Value] -> String
showVals vs = case partitionEithers vs of
  ([],vals) -> concatMap (wrapCons) vals
  (tnks,vals) -> concatMap (\tnk -> show tnk ++ " ") tnks ++ " ; " ++ concatMap (wrapCons) vals
  where
    wrapCons :: Value -> String
    wrapCons t@(TyConApp (MkDataCon _ []) _) = show $ t -- if tycon expects no types, don't wrap
    wrapCons t@(TyConApp _ _) = wrapInParenthesis . show $ t
    wrapCons v = show v ++ " "

lookupPtr :: Pointer -> IM (Either Thunk Value)
lookupPtr (MkPointer address) = lookupMem address

lookupMem :: HeapAddress -> IM (Either Thunk Value)
lookupMem address = do
  h <- gets heap
  val <- io $ H.lookup h address
  --watchReductionM $ " lookupMem: " ++ show val   
  maybe fail return val 
  where 
    fail :: IM (Either Thunk Value)
    fail = return . Right . Wrong $ "lookupH could not find heap reference " ++ show address
