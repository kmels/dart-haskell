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
-- Module in charge of testing functions
-----------------------------------------------------------------------------
module DART.ModuleTester(
  -- testing
  testModule, testHaskellExpression
  -- util
  , showTestedFun, showFunTest
  -- exports
  , module DART.ModuleTester.Testable
) where

import DART.CmdLine(debugMStep,debugM,beVerboseM,watchTestM)
import DART.MkRandom
import DART.ModuleTester.Testable
import Data.Maybe
import Language.Core.Interpreter(apply,eval)
import Language.Core.Interpreter.Acknowledge(acknowledgeModule)
import Language.Core.Interpreter.Structures
import Language.Core.Interpreter.Util(showValue)
import Language.Core.Module(moduleFindVdefByName)
import Language.Core.Ty(funTyArgs,typeSignature)
import Language.Core.Vdefg(findVdefByName,vdefId)

--------------------------------------------------------------------------------
-- Prelude
import Data.List(partition)

testExpression :: Exp -> IM TestResult
testExpression = undefined

-- | Given a module and the necessary environment to eval its definitions,
-- we feed functions with arguments of proper type to get test results
-- for each of them
testModule :: Module -> Env -> IM [TestedFun] -- [(Id,TestResult)]
testModule m@(Module mname tdefs vdefgs) libs = do
  debugMStep $ "Testing module " ++ show mname

  module_env <- acknowledgeModule m
  let env = module_env ++ libs
  --let testVdefg vdefg = testMaybe vdefg Nothing Nothing env
  
  -- do the testing!
  test_results <- mapM (flip testVdefg env) vdefgs -- [VdefgTest]
  return . collectFunTests $ test_results
  where
    collectFunTests :: [VdefgTest] -> [TestedFun]
    collectFunTests vdefg_tests = join [fun_tests | VdefgTest _ fun_tests <- filter isTestedVdefg vdefg_tests]
              
    -- collectResults :: [TestResult] -> [(Id,TestResult)]
    -- collectResults [] = []
    -- collectResults (t@(TestResult name _ val):ts) = (qualifiedVar name,t):(collectResults ts)
    -- collectResults (t@(TestResultList ts):ts') = collectResults ts ++ collectResults ts'

-- | Tests a single definition with the given environment, produces a TestResult in case the definition has the proper type, a function type
-- Only one test is performed
testVdefg :: Vdefg -> Env -> IM VdefgTest
testVdefg vdefg@(Nonrec vdef) env = do
  debugMStep $ "Testing definition " ++ vdefId vdef
  fun_test <- testFun vdef env
  
  return $ case fun_test of 
    NoFunTest -> NoVdefgTest -- we have no test result
    FunTest tested_fun -> VdefgTest vdefg [tested_fun]
    
testVdefg vdefg@(Rec vdefs) env = do
  debugMStep $ "Testing recursive definition"
  fun_tests <- mapM (flip testFun env) vdefs
  let
    tested_funs = [tested_fun | FunTest tested_fun <- filter isTestedFun fun_tests]
    
  return $ case tested_funs of
    [] -> NoVdefgTest -- we have no test results!
    tested_funs -> VdefgTest vdefg tested_funs

-- | Tests a value definition more than once, gathers the results and returns them
-- replicateTestVdefg :: Vdefg 

-- | Tries to tests a definition. Returns a test if, and only if, the type of the value definition
-- is a function type
testFun :: Vdef -> Env -> IM FunTest
testFun def@(Vdef (qvar,ty,vdef_exp)) env = 
  -- is the type a function type? i.e. at least of arity 1
  case funTyArgs ty of
    Nothing -> do
      debugM $ "Will not test " ++ showQualified qvar ++ ", not a function type"
      return NoFunTest
    Just fun_signature_types -> do
      let
        -- if the function has type Int -> Int -> Bool
        -- we want to generate two random ints, drop the last type in the signature
        fun_type_args = init fun_signature_types

        -- Test a function once, that is, feed the given random values
        -- to the given function
        testFunOnce :: Value -> [Value] -> IM TestResult
        testFunOnce fun args = do
          -- make heap refs
          heap_refs <- mapM memorizeVal args
          
          applied_fun_result <- feedFun fun heap_refs
          arg_strs <- mapM showValue args
          watchTestM $ " Applying args: " ++ show arg_strs
          applied_fun_result_str <- showValue applied_fun_result
          watchTestM $ " Got result: " ++ applied_fun_result_str
  
          return $ case applied_fun_result of
            (Wrong s) -> FailedTest {
              test_argument_values = args 
              , failed_test_message = s }
            val -> TestSuccess {
              test_argument_values = args
              , test_result_value = val }
              
      typ_sig <- typeSignature fun_signature_types
      debugM $ "Detected function with type signature: " ++ typ_sig
      -- eval the expression (we already know it has the function type)
      fun@(Fun _ _) <- eval vdef_exp env
      
      test_results <- replicateM 5 $ do
        arg_vals <- mapM (mkRandomVal env) fun_type_args
        debugM $ "Did " ++ (show . length) arg_vals ++ " random values"
        testFunOnce fun arg_vals
                
      return . FunTest $ TestedFun def test_results
  where
    -- | Given a value and a list of argument heap references, feed the value
    -- if the value is a function until the list of arguments
    -- is exhausted
    feedFun :: Value -> [HeapReference] -> IM Value
    -- the case where we have a function and no arguments, error
    feedFun (Fun _ fdesc) []  = return . Wrong $ "The impossible happened at feedFun, there is not enough arguments to feed function: "++ fdesc
    -- the case where we have a function and at least one argument, feed
    feedFun fun@(Fun _ _) (arg_heapref@(arg_id,_):other_heaprefs) = do
      argument_value <- eval arg_id (arg_heapref:env)
      argument_str <- showValue argument_value
      watchTestM $ "Feeding function with random argument: " ++ argument_str
      result_value <- apply fun arg_id (arg_heapref:env)
      
      feedFun result_value other_heaprefs
      -- the case where we have a value that is no function, and no arguments
    feedFun val [] = return val
    feedFun e@(Wrong _) _ = return e
    feedFun val args = return . Wrong $ "The impossible happened at feedFun, val= "++ show val ++ " and args = " ++ show args ++ " with |args| " ++ (show . length) args

-- | Given a module and an identifier, test the definition for that identifier
testHaskellExpression :: Module -> String -> Env -> IM FunTest -- (Maybe (Id,TestResult))
testHaskellExpression m@(Module mname tdefs vdefgs) id env = 
  case (m `moduleFindVdefByName` id) of
    Just value_definition -> do
      debugMStep ("Testing function " ++ id)
            
      testFun value_definition env
        
--      return $ maybeResult >>= Just . (,) expression_string
    Nothing -> do
      debugMStep ("Could not test " ++ id)
      return NoFunTest --TODO (JIT..)

--   result <- testMaybe (HaskellExpression expression_string m) Nothing Nothing env 
--   maybe (return Nothing) (return . Just . (,) expression_string) result
  
-- (HaskellExpression expression_string m@(Module mname _ vdefgs)) _ _ env = 
--     case (m `findVdef` expression_string) of
--       Just vdefg -> debugMStep ("Testing function " ++ expression_string)
--                     >> testMaybe (ModuleFunction vdefg m) Nothing Nothing env
--       Nothing -> return Nothing --TODO


-- showTest :: TestResult -> IM String
-- showTest tr@(TestResult qvar exp val) = do
--   val_str <- showValue val
--   return $ showQualified qvar ++ " => " ++ val_str
-- showTest tr = return $ show $ "LIST"

showTestedFun :: TestedFun -> IM String
showTestedFun (TestedFun vdef test_results) =
  case test_fails of
    [] -> return $ id ++ " passed " ++ (show . length $ test_successes) ++ " tests"
    _ ->
      let 
        failed_str = id ++ " failed in " ++ (show . length $ test_fails) ++ " tests" 
      in do
        failed_test_strs <- mapM showTestResult test_fails
        return $ failed_str ++ show failed_test_strs
  where
    id = vdefId vdef
    (test_successes, test_fails) = partition isSuccessfulTest test_results
  
showTestResult :: TestResult -> IM String
showTestResult (TestSuccess _ _) = return "Test success"
showTestResult (FailedTest arg_vals message) = do
  arg_strs <- mapM showValue arg_vals
  return $ "Failed with arguments: " ++ show arg_strs

showFunTest :: FunTest -> IM String
showFunTest NoFunTest = return $ "Function is not testable"
showFunTest (FunTest tested_fun) = showTestedFun tested_fun
