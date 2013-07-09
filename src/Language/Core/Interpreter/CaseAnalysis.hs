{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE DoAndIfThenElse #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Language.Core.Interpreter.CaseAnalysis
-- Copyright   :  (c) Carlos López-Camey, University of Freiburg
-- License     :  BSD-3
--
-- Maintainer  :  c.lopez@kmels.net
-- Stability   :  stable
--
--
-- Part of the interpreter that does case analysis. 
-----------------------------------------------------------------------------

module Language.Core.Interpreter.CaseAnalysis where

import Control.Applicative
import DART.CmdLine(watchReductionM)
import Language.Core.Interpreter.Structures

data CaseAnalysis = CaseAnalysisResult {
  analysis_expression :: Exp, -- the expression alts are matched against with
  matched_alternative :: Maybe Alt, -- the alternative that matches the analysis expression's value 
  expression_ref :: HeapReference -- heap ref to the analysis expression
  }
