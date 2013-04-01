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

We'll need to differentiate types, for that we will pattern match on them. To be able to pattern match on them, we parse them first. TypeExtractor converts types given by Language.Core.Parser into types described in Language.Core.TypeExtractor.

> import Language.Core.TypeExtractor
> import Language.Core.TypeExtractor.DataTypes

FunctionApplication is a type wrapper to a value definition (from External Core) with a Tapp. It represents a lambda abstraction.

> import Language.Core.Vdefg

And particular useful combinators

> import Text.Encoding.Z(zDecodeString)
> import Language.Core.Util(showExtCoreType,showExp)
> import Debug.Trace

>{- feedFunction :: FunctionApplication -> Maybe GeneralType
> feedFunction (FunApp i r exp) = let
>   gtype = extractZDecodedType $ (i ++ r) --reify
>   in gtype >>= \type' -> case type' of 
>     g@(Lambda (LambdaAbstraction p1 p2)) -> trace ("\t\tDEBUG -- Reified type: " ++ show g ++ "\n\t\tDEBUG -- Z-decoded Expression: "++ (zDecodeString . showExp $ exp)) $ Just g -}
