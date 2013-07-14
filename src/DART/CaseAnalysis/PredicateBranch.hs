{-# LANGUAGE ImplicitParams #-}
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
-- Defines the predicate branch data type
-----------------------------------------------------------------------------

module DART.CaseAnalysis.PredicateBranch where

--------------------------------------------------------------------------------
-- Interpreter
import Language.Core.Interpreter.Apply
import Language.Core.Interpreter.Structures
import DART.CmdLine(watchSMT)
showExp' e = let ?tab_indentation = 0 in showExp e

-- | In order to pretty print a PredicateBranch, we must have access to the heap, for there are some variables that have to be looked upon.
-- This function is used to output the input file of the SMT solver Z3.
prettyPrintBranch :: PredicateBranch -> IM String
prettyPrintBranch (EnvironmentalPBranch id env val) = do
  -- The expression that represents the predicate ought to be
  -- looked upon in the heap
  lookupId id env >>= return . show 
prettyPrintBranch (PBranch exp val) = case exp of
  (App f (Var v)) -> 
    -- we have a function f applied to var v;
    -- check if function is a binary one
    return $ case f of 
      (App (App (Appt (Var qualified_fun_name) ty) _monophier) v) ->         
        zDecodeQualified qualified_fun_name ++ " .. " ++ showExp' v
      otherwise -> show otherwise
--      "App to qvar: " ++ showQualified qvar
  (App exp (App exp1 exp2)) -> return $ "App to app " ++ showExp' exp1 ++ " -> .. " ++ showExp' exp2
  _ -> let ?tab_indentation = 0 in return $ "Other thing: " ++ showExp exp

-- | When case analysis is done, and a pattern matches the value
-- we are analyzing, there's a branch in the program execution path
-- that we're going to record with this method.
recordBranch :: Exp -> Value -> Env -> IM ()
recordBranch exp val env = do
  let pbranch = case exp of
        (Var qvar) -> EnvironmentalPBranch (zDecodeQualified qvar) env val
        _ -> PBranch exp val
  
  pbranch_string <- prettyPrintBranch pbranch
  watchSMT $ "... recording predicate branch expression, " ++ pbranch_string
  modify $ addBranch pbranch
  
  where
    addBranch :: PredicateBranch -> DARTState -> DARTState
    addBranch pbranch st = st {
      pbranches_record = pbranch:(pbranches_record st)
    }
