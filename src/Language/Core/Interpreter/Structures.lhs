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

For the heap, we use the package [hashtables](http://hackage.haskell.org/package/hashtables)

> import qualified Data.HashTable.IO as H
> import qualified Data.HashTable.ST.Cuckoo as C

> import           Control.Monad.State
> import           Control.Monad.Primitive

> type Heap = H.CuckooHashTable Id Value

Define a monad IM (for Interpreter Monad), inspired by the *M* monad in [P. Wadler, The essence of Functional Programming](http://homepages.inf.ed.ac.uk/wadler/topics/monads.html).

> type IM = StateT Heap IO 

> instance Show Exp where
>          show = showExp 

> data Value = Wrong String
>            | ExtCoreExp Exp
>            | Num Integer
>            | Fun (Value -> IM Value) Description

> type Description = String

> instance Show Value where
>   show (Wrong s) = "Wrong " ++ s
>   show (ExtCoreExp exp) = "ExtCoreExp " ++ show exp
>   show (Num i) = show i
>   show (Fun f s) = wrapName "Fun" s

> --type Environment = [(Id,IM Value)]