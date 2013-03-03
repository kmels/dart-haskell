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
-- Data types useful for Language.Core.Interpreter
-----------------------------------------------------------------------------

> {-# LANGUAGE FlexibleInstances #-}

> module Language.Core.Interpreter.Structures where
> import Data.List(findIndices)
> import DART.InterpreterSettings

This is an interpreter for External Core

> import Language.Core.Core
> import Language.Core.Util(showType,showExtCoreType,showExp,wrapName)

> import           Control.Monad.State
> import           Control.Monad.Primitive

For the heap, we use the package [hashtables](http://hackage.haskell.org/package/hashtables)

> import qualified Data.HashTable.IO as H
> import qualified Data.HashTable.ST.Cuckoo as C

> type Heap = H.CuckooHashTable Id (Either Thunk Value)

Define a monad IM (for Interpreter Monad) where we keep a value of type DARTState containing state variables such as the heap and settings such as the number of reduction for debugging purposes. 

> data DARTState = DState {
>   heap :: Heap,
>   number_of_reductions :: !Int -- when an expression is evaluated, this value increments by 1
>   , tab_indentation :: !Int -- useful in debug to know how many tabs we shoud prepend
>   , settings :: InterpreterSettings
> }

> type IM = StateT DARTState IO 

> -- DARTState = { heap :: Heap, types ::  

> data Value = Wrong String
>            | Num Integer
>            | Rat Rational -- arbitrary-precision rational numbers
>            | Boolean Bool
>            | Char Char
>            | String String
>            | Fun (Value -> IM Value) Description
>            -- |  List [Value]
>            | Pair Value Value
>            | TyConApp TyCon [Value]
>            | TyCon TyCon -- so that we can store type constructors in the heap

> data Thunk = Thunk Exp 
> instance Show Thunk where show _ = "Thunk"

> data TyCon = AlgTyCon Id [Ty] -- if Left, then this tycon expects at least one value of type Ty; if Value, this TyCon replaced a Ty for a Value
> --data DataCon = MkDataCon Id [Value] --when a TyCon has no Lefts, we shall create a data con

> type Description = String

> instance Eq Value where 
>   (Wrong s) == Wrong s' = s == s'
>   (Num i) == (Num i') = i == i'
>   (Fun _ _) == (Fun _ _) = False  -- too bad we are not intensional as in intensional type equality
>   (Boolean b) == (Boolean b') = b == b'
>   o == p = False

> instance Show Value where
>   show (Wrong s) = "Wrong " ++ s
>   show (Num i) = show i
>   show (Fun f s) = s
>   show (Boolean b) = show b
>   show (Rat r) = show r
>   show (String s) = show s
> --  show (List vs) = show vs
>   show (Char c) = show c
>   show (TyConApp (AlgTyCon "ghczmprim:GHC.Tuple.Z2T" _) [x,y]) = show (x,y)
>   show (TyConApp tc@(AlgTyCon c _) vals@(hv:tv)) | c == "ghc-prim:GHC.Taypes.:" = show vals
>                                          | c == "ghc-prim:GHC.Types.[]a" = show vals
>                                          | otherwise = idName c ++ vals' where 
>     show' tca@(TyConApp _ _) = " " ++ wrapInParenthesis (show tca) 
>     vals' = concatMap (\v -> " " ++ show v) vals
>   show (TyCon tycon@(AlgTyCon id _)) | idName id == "[]" = "[]"
>                                      | otherwise = show tycon
>   show (Pair a b) = show (a,b)

> instance Show TyCon where
>   show (AlgTyCon id []) = idName id
>   show (AlgTyCon id types) = idName id ++ " :: " ++ types' where
>     types' :: String
>     types' | length types == 1 = show types 
>            | otherwise = concatMap (\t -> show t ++ " -> ") types

Take a qualified name and return only its last name. E.g. idName "main.Module.A" = "A"

> idName :: Id -> String
> idName id = let 
>   name = drop (lastDotIndex id + 1) id 
>   in case name of 
>     ":" -> "(:)"
>     _ -> name
>   where
>     isDot = ((==) '.')
>     dotIndexes = findIndices isDot
>     lastDotIndex = last . dotIndexes

> wrapInParenthesis s = "(" ++ s ++ ")"
