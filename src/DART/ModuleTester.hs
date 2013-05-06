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

import DART.CmdLine(debugMStep,debugM)
import DART.ExtCore.TypeExtractor
import DART.ModuleTester.Testable
import Data.Maybe
import Language.Core.Interpreter.Acknowledge(acknowledgeModule)
import Language.Core.Interpreter.Structures
import Language.Core.Vdefg(findVdefByName,vdefId)
import Language.Core.Module(moduleFindVdefByName)

testExpression :: Exp -> IM TestResult
testExpression = undefined

-- | Given a module and the necessary environment to eval its definitions,
-- we feed functions with arguments of proper type to get test results
-- for each of them
testModule :: Module -> Env -> IM [(Id,TestResult)]
testModule m@(Module mname tdefs vdefgs) libs = do
  debugMStep $ "Testing module " ++ show mname

  module_env <- acknowledgeModule m
  let env = module_env ++ libs
  --let testVdefg vdefg = testMaybe vdefg Nothing Nothing env
  
  -- do the testing!
  test_results <- mapM (flip testVdefg env) vdefgs -- [Maybe TestResult]  
  
  let results = catMaybes test_results
      mkVdefName = qualifiedVar . vdefg_name
  return $ zip (map mkVdefName results) results

testVdefg :: Vdefg -> Env -> IM (Maybe TestResult)
testVdefg (Nonrec vdef) env = do
  debugMStep $ "Testing definition " ++ vdefId vdef
  testVdef vdef env
testVdefg (Rec vdefs) env = do
  debugMStep $ "Testing recursive definition"
  mapM (flip testVdefg env . Nonrec) vdefs >>= return . Just . TestResultList . catMaybes

testVdef :: Vdef -> Env -> IM (Maybe TestResult)
testVdef (Vdef (qvar,ty,exp)) env = do
  debugM $ "Extracted type: " ++ show typ
  -- check whether the type of this definition is testable.
  case typ of
    -- concrete types e.g. Int, Char, [Int], [Bool], etc., are not testable
    Just (CType concrete_type) -> return Nothing
    -- a lambda abstraction is testable
    Just l@(Lambda _) -> testMaybe l (Just qvar) (Just exp) env        
    -- parse error
    Nothing -> error $ "Could not parse type : " ++ showExtCoreType ty 
    -- otherwise.. we don't know yet
    Just x -> error $ " Undefined Testable: " ++ show x
  where 
    typ = extractType ty
-- testModuleFunction :: ModuleFunction -> Env -> IM (Maybe (Id,TestResult))
-- testModuleFunction (ModuleFunction vdef m) env = do
--   m_env <- acknowledgeModule m 
--   testVdef vdef (env ++ m_env)

testHaskellExpression :: Module -> String -> Env -> IM (Maybe (Id,TestResult))
testHaskellExpression m@(Module mname tdefs vdefgs) expression_string env = 
  case (m `moduleFindVdefByName` expression_string) of
    Just vdef -> do
      debugMStep ("Testing function " ++ expression_string)
      maybeResult <- testVdef vdef env
      return $ maybeResult >>= Just . (,) expression_string
      
                  -- testMaybe (ModuleFunction vdefg m) Nothing Nothing env
    Nothing -> return Nothing --TODO (JIT..)

--   result <- testMaybe (HaskellExpression expression_string m) Nothing Nothing env 
--   maybe (return Nothing) (return . Just . (,) expression_string) result
  
-- (HaskellExpression expression_string m@(Module mname _ vdefgs)) _ _ env = 
--     case (m `findVdef` expression_string) of
--       Just vdefg -> debugMStep ("Testing function " ++ expression_string)
--                     >> testMaybe (ModuleFunction vdefg m) Nothing Nothing env
--       Nothing -> return Nothing --TODO
      
showTest :: TestResult -> IM String
showTest = return . show 
