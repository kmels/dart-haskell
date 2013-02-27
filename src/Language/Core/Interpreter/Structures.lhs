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

This is an interpreter for External Core

> import Language.Core.Core
> import Language.Core.Util(showType,showExtCoreType,showExp,showMname,wrapName)

> import           Control.Monad.State
> import           Control.Monad.Primitive

For the heap, we use the package [hashtables](http://hackage.haskell.org/package/hashtables)

> import qualified Data.HashTable.IO as H
> import qualified Data.HashTable.ST.Cuckoo as C

> type Heap = H.CuckooHashTable Id (Either Thunk Value)

We'll also need to keep track of the declared types and their constructors, for which we'll also use a hashtable, where a type is identified by its qualified name.

> -- type Types = H.CuckooHashTable Id Type

Define a monad IM (for Interpreter Monad), inspired by the *M* monad in [P. Wadler, The essence of Functional Programming](http://homepages.inf.ed.ac.uk/wadler/topics/monads.html).

> type IM = StateT Heap IO 

> -- DARTState = { heap :: Heap, types :: 

> instance Show Exp where
>          show = showExp 

> data Value = Wrong String
>            | Num Integer
>            | Rat Rational -- arbitrary-precision rational numbers
>            | Boolean Bool
>            | Char Char
>            | String String
>            | Fun (Value -> IM Value) Description
>            | List [Value]
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
>   show (Fun f s) = wrapName "Fun" s
>   show (Boolean b) = show b
>   show (Rat r) = show r
>   show (String s) = show s
>   show (List vs) = show vs
>   show (Char c) = [c]
>   show (TyConApp tc@(AlgTyCon n _) vals) = idName n ++ " " ++ vals' where
>     vals' = concatMap show' vals
>   show (TyCon tycon) = show tycon
>   --show (AlgTyCon n args) = "AlgTyCon" ++ n ++ ", " ++ show args
> --type Environment = [(Id,IM Value)]

> instance Show TyCon where
>   show (AlgTyCon id []) = idName id
>   show (AlgTyCon id types) = idName id ++ " " ++ types' where
>     types' :: String
>     types' = concatMap (\t -> show t ++ " -> ") types

Take a qualified name and return only its last name. E.g. idName "main.Module.A" = "A"

> idName :: Id -> String
> idName id = drop (lastDotIndex id + 1) id where
>   isDot = ((==) '.')
>   dotIndexes = findIndices isDot
>   lastDotIndex = last . dotIndexes

> wrapInParenthesis s = "(" ++ s ++ ")"
> show' s = show s ++ " "
