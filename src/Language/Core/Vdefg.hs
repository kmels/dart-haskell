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

module Language.Core.Vdefg where

import Language.Core.Core
import Language.Core.Util
import DART.InterpreterSettings
import Text.Encoding.Z -- DELETE

-- | Let's create some data types for type safety purposes. First, lambda abstraction (function application)

data FunctionApplication = FunApp String String Exp

--instance Show FunctionApplication where
--  show (FunApp t1 t2 exp) = ":: " ++ t1 ++ " -> " ++ t2 ++ " = " ++ showExp exp

-- | Extract an expression from a value definition

vdefExp :: Vdef -> Exp
vdefExp (Vdef (_, _, exp)) = exp

vdefQualId :: Vdef -> String
vdefQualId (Vdef (qual_var, _ , _)) = qualifiedVar qual_var
 
-- | Useful functions to filter types of value definitions.

vdefNonRecursive :: Vdefg -> Maybe Vdef
vdefNonRecursive (Nonrec vdef) = Just vdef
vdefNonRecursive _ = Nothing

vdefgToMaybeTapp :: Vdefg -> Maybe FunctionApplication
vdefgToMaybeTapp vdefg = vdefNonRecursive vdefg >>= vdefTapp >>= \tapp -> case tapp of
  (Vdef (_, (Tapp i r), e)) -> Just $ FunApp (showExtCoreType i) (showExtCoreType r) e


-- | Given a value definition, return a expression if the vdef is a function application (lambda)

vdefTapp :: Vdef -> Maybe Vdef
vdefTapp tapp@(Vdef (_, (Tapp _ _), exp)) = Just tapp
vdefTapp _ = Nothing

-- | Given a value definition, we return its full identifier. That is, containing the package and the module name together with the function name.

-- vdefgId :: Vdefg -> String
-- vdefgId (Nonrec (Vdef (qvar, _, _))) = qualifiedVar qvar
-- vdefgId (Rec []) = ""
-- vdefgId (Rec ((Vdef (qvar, _, _)):xs)) = qualifiedVar qvar ++ " and " ++ vdefgName (Rec xs)

-- | Given a value definition, return its name (or names if recursive) 
vdefgNames :: Vdefg -> [String]
vdefgNames (Nonrec (Vdef ((_,id), _, _))) = [id]
vdefgNames (Rec []) = []
vdefgNames (Rec ((Vdef ((_,id), _, _)):xs)) = id:(vdefgNames (Rec xs))

-- | Given a value definition, return its name (or names if recursive) 
vdefgQualVars :: Vdefg -> [Qual Var]
vdefgQualVars (Nonrec (Vdef (qvar, _, _))) = [qvar]
vdefgQualVars (Rec []) = []
vdefgQualVars (Rec ((Vdef (qvar, _, _)):xs)) = qvar:(vdefgQualVars (Rec xs))

{-isTmp :: Vdefg -> Bool
isTmp vdefg = 
  let
    isTmp' :: Qual Var -> Bool
    isTmp' (Nothing, _) = True 
    isTmp' _ = False -- has a package, or module in its qualified name
  in case vdefg of
    (Nonrec v@(Vdef (qvar, _, _))) -> isTmp' qvar
    (Rec vdefs) -> not . any (isTmp . Nonrec) $ vdefs
-}
