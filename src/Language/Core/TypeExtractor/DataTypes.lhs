----------------------------------------------------------------------------
-- |
-- Module      :  Language.Core.TypeExtractor.DataTypes
-- Copyright   :  (c) Carlos López-Camey, University of Freiburg
-- License     :  BSD-3
--
-- Maintainer  :  c.lopez@kmels.net
-- Stability   :  stable
--
--
-- Data types used by Language.Core.TypeExtractor
-----------------------------------------------------------------------------

> module Language.Core.TypeExtractor.DataTypes where

> data GeneralType = CType ConcreteType | Lambda LambdaAbstraction | NoType String deriving Show

> data ConcreteType = PList PrimitiveList | PType PrimitiveType deriving Show

> data PrimitiveList = PrimitiveList PrimitiveType deriving Show

> data PrimitiveType = PrimitiveCharType String 
>                      | PrimitiveBoolType String
>                        deriving Show


> data LambdaAbstraction = LambdaAbstraction ConcreteType GeneralType deriving Show

> data GenericList = GenericList
