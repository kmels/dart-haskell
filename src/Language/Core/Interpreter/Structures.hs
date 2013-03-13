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

module Language.Core.Interpreter.Structures(
  io
  , idName
  , increase_number_of_reductions
  -- heap operation
  , memorize, mkVal, mkThunk, mkHeapReference
  , DARTState(..)
  , Heap, Env, HeapAddress, HeapReference
  , IM
  , Thunk (..), TyCon (..) , Value(..)
  , Language.Core.Core.Id
  , module Control.Monad.State
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
import           Language.Core.Util(showType,showExtCoreType,showExp,wrapName)

-- mutable hash tables; 
-- package [hashtables](http://hackage.haskell.org/package/hashtables)
import qualified Data.HashTable.IO as H
import qualified Data.HashTable.ST.Cuckoo as C

-- | A State passed around the interpreter
data DARTState = DState {
 heap :: Heap, -- our memory 
 heap_count :: Int, -- address counter, counts those previously deleted too
                    -- useful to generate new variable names
 number_of_reductions :: !Int -- when an expression is evaluated, this value increments by 1
 , tab_indentation :: !Int -- useful when to know how many tabs we shoud prepend
 , settings :: InterpreterSettings
}

type Heap = H.CuckooHashTable HeapAddress (Either Thunk Value)
type HeapAddress = Int
type Env = [(Id,HeapAddress )]
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
           | Pair (Either Thunk Value) (Either Thunk Value) --HERE
           | TyConApp TyCon [Either Thunk Value] -- a type constructor application to some values
           | Pointer HeapAddress

data Thunk = Thunk Exp
instance Show Thunk where show _ = "Thunk"

-- cons = TyConApp (AlgTyCon "Cons" [a]) [1,Nil]

-- RENAME THIS (DataCon)

data TyCon = AlgTyCon Id [Ty] -- if Left, then this tycon expects at least one value of type Ty; if Value, this TyCon replaced a Ty for a Value
--data DataCon = MkDataCon Id [Value] --when a TyCon has no Lefts, we shall create a data con

type Description = String

instance Eq Value where 
  (Wrong s) == Wrong s' = s == s'
  (Num i) == (Num i') = i == i'
  (Fun _ _) == (Fun _ _) = False  -- too bad we are not intensional as in intensional type equality
  (Boolean b) == (Boolean b') = b == b'
  o == p = False

instance Show Value where
  show (Wrong s) = "Wrong " ++ s
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
  show (TyConApp tc vals) = showTyConApp tc vals
  show (Pointer address) = "Pointer to " ++ show address

instance Show TyCon where
  show (AlgTyCon id []) = idName id
  show (AlgTyCon id types) = idName id ++ " :: " ++ types' where
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

mkThunk :: Exp -> Either Thunk Value
mkThunk = Left . Thunk

mkVal :: Value -> Either Thunk Value
mkVal = Right

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
  -- debugM $ "Memorized " ++ id ++ " in " ++ show address ++ " as " ++ show val
  return (id,address)

-- | Creates new variable for the expression, memorizes it and returns a heap reference
mkHeapReference :: Exp -> IM HeapReference
mkHeapReference exp = mkVarName >>= memorize (mkThunk exp)

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
    showTail (TyConApp (AlgTyCon "ghc-prim:GHC.Types.:" _) ((Right th):(Right tt):[])) = "," ++ show th ++ showTail tt
    showTail (TyConApp (AlgTyCon "ghc-prim:GHC.Types.[]" _) []) = ""
    showTail xs = "????\t\t\t" ++ show xs ++ " \t\t\t"

showTyConApp :: TyCon -> [Either Thunk Value] -> String
showTyConApp (AlgTyCon "ghc-prim:GHC.Types.[]" []) [] = "[]" -- empty list
showTyConApp (AlgTyCon "ghc-prim:GHC.Types.:" _) cns = showList cns -- lists
showTyConApp (AlgTyCon "ghc-prim:GHC.Tuple.Z2T" _) [x,y] = show (x,y) -- tuples
-- otherwise
showTyConApp (AlgTyCon tycon_name []) vals = idName tycon_name ++ " " ++ showVals vals
showTyConApp (AlgTyCon tycon_name _) vals = idName tycon_name ++ " " ++ showVals vals

showVals :: [Either Thunk Value] -> String
showVals vs = case partitionEithers vs of
  ([],vals) -> concatMap (wrapCons) vals
  (tnks,vals) -> concatMap (\tnk -> show tnk ++ " ") tnks ++ " ; " ++ concatMap (wrapCons) vals
  where
    wrapCons :: Value -> String
    wrapCons t@(TyConApp (AlgTyCon _ []) _) = show $ t -- if tycon expects no types, don't wrap
    wrapCons t@(TyConApp _ _) = wrapInParenthesis . show $ t
    wrapCons v = show v ++ " "
