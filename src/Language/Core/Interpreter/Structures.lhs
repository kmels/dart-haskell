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

> data Thunk = Thunk Exp 
> instance Show Thunk where show _ = "Thunk"

> -- data Type = TyConApp TyCon [Type]
> -- data TyCon = AlgTyCon Id [DataCon]
> -- data DataCon = MkDataCon Id Type

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
>   show (String s) = s
>   show (List vs) = show vs
>   show (Char c) = [c]
> --type Environment = [(Id,IM Value)]
