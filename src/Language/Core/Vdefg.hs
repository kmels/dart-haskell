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

import Data.List(find)
import Language.Core.Core
import Language.Core.Util
import Text.Encoding.Z

-- | Let's create some data types for type safety purposes. First, lambda abstraction (function application)
data FunctionApplication = FunApp String String Exp

-- | Extract an expression from a value definition
vdefExp :: Vdef -> Exp
vdefExp (Vdef (_, _, exp)) = exp

-- | Given a value definition, returns the full qualified name 
vdefQualId :: Vdef -> String
vdefQualId (Vdef (qual_var, _ , _)) = zDecodeQualified qual_var

-- | Similar to vdefQualId but it ignores the module name
vdefName :: Vdef -> String
vdefName (Vdef (qual_var, _, _)) = snd qual_var
 
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

vdefId :: Vdef -> String
vdefId (Vdef (qid, _, _)) = zDecodeQualified qid

-- | Given a value definition, return its name (or names if recursive) 
vdefgNames :: Vdefg -> [String]
vdefgNames (Nonrec (Vdef ((_,id), _, _))) = [id]
vdefgNames (Rec []) = []
vdefgNames (Rec ((Vdef ((_,id), _, _)):xs)) = id:(vdefgNames (Rec xs))

-- | Looks for a value definition whose name (not qualified) is identical to the given one
findVdefByName :: String -> Vdefg -> Maybe Vdef
findVdefByName name (Nonrec def)   | vdefName def == name = Just def
findVdefByName name (Nonrec def)   | otherwise = Nothing
findVdefByName name (Rec (def:_))  | vdefName def == name = Just def
findVdefByName name (Rec (_:defs)) | otherwise = findVdefByName name (Rec defs)
findVdefByName name (Rec []) = Nothing

-- | Looks for value definitions with the given identifier within the module 
-- i.e. a not qualified name. 
findVdefg :: Module -> Id -> Maybe Vdefg
findVdefg _ "" = Nothing
findVdefg m@(Module _ _ []) id = Nothing
findVdefg m@(Module mname _ (vdef:vs)) id = case containsId vdef of
  True -> Just vdef
  _ -> findVdefg (Module mname [] vs) id  
  where
    containsId :: Vdefg -> Bool
    containsId vdefg = any ((==) id) $ vdefgNames vdefg
    
  -- fnames = concatMap vdefgNames vdefgs -- [String]
  -- fnames_vdefgs = zip fnames vdefgs 
  -- maybeVdefg = find ((==) id . fst) fnames_vdefgs >>= return . snd -- :: Maybe Vdefg
  -- in maybeVdefg
  
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
