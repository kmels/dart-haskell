----------------------------------------------------------------------------
-- |
-- Module      :  DART.ModuleTester
-- Copyright   :  (c) Carlos López-Camey, University of Freiburg
-- License     :  BSD-3
--
-- Maintainer  :  c.lopez@kmels.net
-- Stability   :  stable
--
--
-- Tests value definitions in a module
-----------------------------------------------------------------------------
module DART.ModuleTester where

import DART.CmdLine(debugMStep,debugM)
import Data.Maybe
import Language.Core.Interpreter.Acknowledge(acknowledgeModule)
import Language.Core.Interpreter.Structures

data TestResult = TestResult{
  vdefg_name :: Qual Var,
  test_expression :: Exp,
  test_value :: Value
} deriving Show

testExpression :: Exp -> IM TestResult
testExpression = undefined

testModule :: Module -> Env -> IM [(Id,TestResult)]
testModule m@(Module mname tdefs vdefgs) env = do
  debugMStep $ "Testing module " ++ show mname
  module_env <- acknowledgeModule m
  test_results <- mapM testVdefg vdefgs -- [Maybe TestResult]  
  let results = catMaybes test_results
  return $ zip (map (qualifiedVar . vdefg_name) results) results
  
-- | Given a generic value definition, test the definition if it is testable
testVdefg :: Vdefg -> IM (Maybe TestResult)
testVdefg = undefined

showTest :: TestResult -> IM String
showTest = return . show 
