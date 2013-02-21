----------------------------------------------------------------------------
-- |
-- Module      :  Language.Core.ValueDefinition
-- Copyright   :  (c) Carlos López-Camey, University of Freiburg
-- License     :  BSD-3
--
-- Maintainer  :  c.lopez@kmels.net
-- Stability   :  stable
--
--
-- This module contains functions that work upon Language.Core.Core.Vdef
-- 
-- A value definition as defined by the external core syntax (BNF grammar):
--
-- vdefg := %rec { vdef { ; vdef } } | vdef 
-- vdef := qvar :: ty = exp
--
-- ty is a type (Language.Core.Core.Ty) and exp an expression (Language.Core.Core.Exp)
----------------------------------------------------------------------------- 

> module Language.Core.ValueDefinition where

> import Language.Core.Core
> import Language.Core.Util

> import Text.Encoding.Z -- DELETE

Let's create some data types for type safety purposes. First, lambda abstraction (function application)

> data FunctionApplication = FunApp String String Exp

> instance Show FunctionApplication where
>   show (FunApp t1 t2 exp) = ":: " ++ t1 ++ " -> " ++ t2 ++ " = " ++ showExp exp

Extract an expression from a value definition

> vdefExp :: Vdef -> Exp
> vdefExp (Vdef (_, _, exp)) = exp

Useful functions to filter types of value definitions.

> vdefNonRecursive :: Vdefg -> Maybe Vdef
> vdefNonRecursive (Nonrec vdef) = Just vdef
> vdefNonRecursive _ = Nothing

> vdefgToMaybeTapp :: Vdefg -> Maybe FunctionApplication
> vdefgToMaybeTapp vdefg = vdefNonRecursive vdefg >>= vdefTapp >>= \tapp -> case tapp of
>   (Vdef (_, (Tapp i r), e)) -> Just $ FunApp (showExtCoreType i) (showExtCoreType r) e


Given a value definition, return a expression if the vdef is a function application (lambda)

> vdefTapp :: Vdef -> Maybe Vdef
> vdefTapp tapp@(Vdef (_, (Tapp _ _), exp)) = Just tapp
> vdefTapp _ = Nothing

Given a value definition, we return its full identifier. That is, containing the package and the module name together with the function name.

> vdefgId :: Vdefg -> String
> vdefgId (Nonrec (Vdef (qvar, _, _))) = qualifiedVar qvar
> vdefgId (Rec []) = ""
> vdefgId (Rec ((Vdef (qvar, _, _)):xs)) = qualifiedVar qvar ++ " and " ++ vdefgName (Rec xs)

Given a Value definition, return its name within its module.

> vdefgName :: Vdefg -> String
> vdefgName (Nonrec (Vdef ((_,id), _, _))) = id
> vdefgName (Rec []) = ""
> vdefgName (Rec ((Vdef ((_,id), _, _)):xs)) = id
