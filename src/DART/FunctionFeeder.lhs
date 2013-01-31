----------------------------------------------------------------------------
-- |
-- Module      :  DART.FunctionFeeder
-- Copyright   :  (c) Carlos López-Camey, University of Freiburg
-- License     :  BSD-3
--
-- Maintainer  :  c.lopez@kmels.net
-- Stability   :  stable
--
--
-- This module contains functions that feed a FunctionApplication
-----------------------------------------------------------------------------

> module DART.FunctionFeeder where

We'll need to differentiate types, for that we will pattern match on them. To be able to pattern match on them, we need to parse them. TypeExtractor converts types managed by  Language.Core.Parser into types described in Language.Core.TypeExtractor.

> import Language.Core.TypeExtractor

FunctionApplication is a type wrapper to a value definition (from External Core) with a Tapp. It represents a lambda abstraction.

> import Language.Core.ValueDefinition

And particular useful combinators

> import Text.Encoding.Z(zDecodeString)

> feedFunction :: FunctionApplication -> Maybe GeneralType
> feedFunction (FunApp i r exp) = let
>   gtype = extractType . zDecodeString  $ "a" ++ show i
>   in gtype >>= \type' -> case type' of 
>     g@(Lambda (LambdaAbstraction p1 p2)) -> Just g
