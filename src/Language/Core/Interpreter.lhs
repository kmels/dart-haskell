----------------------------------------------------------------------------
-- |
-- Module      :  Language.Core.Interpreter
-- Copyright   :  (c) Carlos López-Camey, University of Freiburg
-- License     :  BSD-3
--
-- Maintainer  :  c.lopez@kmels.net
-- Stability   :  stable
--
--
-- An interpreter for external core expressions
-----------------------------------------------------------------------------

> module Language.Core.Interpreter where

> import Language.Core.Core
> import Language.Core.Util(showType,showExtCoreType,showExp)
> import Language.Core.TypeExtractor(extractType)
> import Language.Core.TypeExtractor.DataTypes

> evalVdefg :: Vdefg -> String
> evalVdefg (Rec vdefs) = "Recursive eval not yet implemented\n\t" ++ concatMap (\(Vdef ((mname,var),ty,exp)) -> var ++ " :: " ++ showType ty ++ "\n\t"++ showExp exp) vdefs
> evalVdefg (Nonrec (Vdef ((mname,var), ty, exp))) = 
>   let 
>      extractedType = extractType ty 
>   in case extractedType of
>     Nothing -> "Could not parse type " ++ showExtCoreType ty ++ "; therefore I did not interpret"
>     Just (CType (PType pt)) -> "Will evaluate " ++ var ++ "::" ++ show pt ++ "\n\tExp: " ++ showExp exp ++ "\n\tResult: " ++ extCoreEval exp
>     Just ty -> "I still don't know how to evaluate values of type " ++ show ty

> extCoreEval :: Exp -> String
> extCoreEval exp = " nil"
