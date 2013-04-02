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
module DART.ModuleTester(
  -- testing
  testModule,
  -- util
  showTest,
  -- exports
  module DART.ModuleTester.Testable
) where

import DART.ModuleTester.Testable
import DART.CmdLine(debugMStep,debugM)
import Data.Maybe
import Language.Core.Interpreter.Acknowledge(acknowledgeModule)
import Language.Core.Interpreter.Structures

testExpression :: Exp -> IM TestResult
testExpression = undefined

testModule :: Module -> Env -> IM [(Id,TestResult)]
testModule m@(Module mname tdefs vdefgs) env = do
  debugMStep $ "Testing module " ++ show mname
  module_env <- acknowledgeModule m
  let testVdefg = \v -> testMaybe v Nothing Nothing env
  test_results <- mapM testVdefg vdefgs -- [Maybe TestResult]  
  let results = catMaybes test_results
  return $ zip (map (qualifiedVar . vdefg_name) results) results

showTest :: TestResult -> IM String
showTest = return . show 
