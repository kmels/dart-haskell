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
  testModule, testHaskellExpression,
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

-- | Given a module and the necessary environment to eval its definitions,
-- we feed functions with arguments of proper type to get test results
-- for each of them
testModule :: Module -> Env -> IM [(Id,TestResult)]
testModule m@(Module mname tdefs vdefgs) libs = do
  debugMStep $ "Testing module " ++ show mname

  module_env <- acknowledgeModule m -- where is this used ????? 
  let env = module_env ++ libs
  let testVdefg vdefg = testMaybe vdefg Nothing Nothing env
  
  -- do the testing!
  test_results <- mapM testVdefg vdefgs -- [Maybe TestResult]  
  
  let results = catMaybes test_results
      mkVdefName = qualifiedVar . vdefg_name
  return $ zip (map mkVdefName results) results

testHaskellExpression :: Module -> String -> Env -> IM (Maybe (Id,TestResult))
testHaskellExpression m@(Module mname tdefs vdefgs) expression_string env = do
  result <- testMaybe (HaskellExpression expression_string m) Nothing Nothing env 
  maybe (return Nothing) (return . Just . (,) expression_string) result
  
showTest :: TestResult -> IM String
showTest = return . show 
