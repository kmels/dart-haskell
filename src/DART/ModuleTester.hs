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

import DART.CmdLine(debugMStep,debugM,beVerboseM)
import DART.ExtCore.TypeExtractor
import DART.ModuleTester.Testable
import DART.MkRandom
import Data.Maybe
import Language.Core.Interpreter.Acknowledge(acknowledgeModule)
import Language.Core.Interpreter(apply,eval)
import Language.Core.Interpreter.Structures
import Language.Core.Ty(funTyArgs)
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

  return . collectResults . catMaybes $ test_results
  where
    collectResults :: [TestResult] -> [(Id,TestResult)]
    collectResults [] = []
    collectResults (t@(TestResult name _ val):ts) = (qualifiedVar name,t):(collectResults ts)
    collectResults (t@(TestResultList ts):ts') = collectResults ts ++ collectResults ts'

testVdefg :: Vdefg -> Env -> IM (Maybe TestResult)
testVdefg (Nonrec vdef) env = do
  debugMStep $ "Testing definition " ++ vdefId vdef
  testVdef vdef env
testVdefg (Rec vdefs) env = do
  debugMStep $ "Testing recursive definition"
  mapM (flip testVdefg env . Nonrec) vdefs >>= return . Just . TestResultList . catMaybes


-- | Tries to tests a definition. Returns a test if, and only if, the type of the value definition
-- is a function type
testVdef :: Vdef -> Env -> IM (Maybe TestResult)
testVdef (Vdef (qvar,ty,vdef_exp)) env = 
  -- is the type a function type? i.e. at least of arity 1
  case funTyArgs ty of
    Nothing -> do
      debugM $ "Will not test " ++ showQualified qvar ++ ", not a function type"
      return Nothing
    Just fun_signature_types -> do
      -- make random values      
      heap_refs <- mapM (\ty -> mkHeapRef $ tyGetRandom ty env) fun_signature_types
      debugM $ "Did " ++ (show . length) heap_refs ++ " random values"
      
      -- build the initial function from expression and get the result
      fun <- eval vdef_exp env
      applied_fun_result <- feedFun fun heap_refs
      
      -- return a test result from the result value
      return $ Just $ mkTestResult qvar vdef_exp applied_fun_result
  where
    -- | Given a value and a list of arguments, feed the value
    -- if the value is a function until the list of arguments
    -- is exhausted
    feedFun :: Value -> [HeapReference] -> IM Value        
    -- the case where we have a function and no arguments, error
    feedFun (Fun _ fdesc) []  = return . Wrong $ "The impossible happened at feedFun, there is not enough arguments to feed function: "++ fdesc
    -- the case where we have a function and at least one argument, feed
    feedFun fun@(Fun _ _) (arg_heapref@(arg_id,_):other_heaprefs) = do
      debugM $ "Feeding function with some random argument"
      result_value <- apply fun arg_id (arg_heapref:env)
      feedFun result_value other_heaprefs
      -- the case where we have a value that is no function, and no arguments
    feedFun val [] = return val
    feedFun val args = return . Wrong $ "The impossible happened at feedFun, val= "++ show val ++ " and args = " ++ show args ++ " with |args| " ++ (show . length) args

-- | Given a module and an identifier, test the definition for that identifier
testHaskellExpression :: Module -> String -> Env -> IM (Maybe (Id,TestResult))
testHaskellExpression m@(Module mname tdefs vdefgs) expression_string env = 
  case (m `moduleFindVdefByName` expression_string) of
    Just vdef -> do
      debugMStep ("Testing function " ++ expression_string)
      maybeResult <- testVdef vdef env
      return $ maybeResult >>= Just . (,) expression_string            
    Nothing -> do
      debugMStep ("Could not test " ++ expression_string)
      return Nothing --TODO (JIT..)

--   result <- testMaybe (HaskellExpression expression_string m) Nothing Nothing env 
--   maybe (return Nothing) (return . Just . (,) expression_string) result
  
-- (HaskellExpression expression_string m@(Module mname _ vdefgs)) _ _ env = 
--     case (m `findVdef` expression_string) of
--       Just vdefg -> debugMStep ("Testing function " ++ expression_string)
--                     >> testMaybe (ModuleFunction vdefg m) Nothing Nothing env
--       Nothing -> return Nothing --TODO
      
showTest :: TestResult -> IM String
showTest tr@(TestResult qvar exp val) = do
  return $ show $ tr
showTest tr = return $ show $ "LIST"
