{-# LANGUAGE NamedFieldPuns #-}
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

import DART.CmdLine(debugMStep,debugMessage,beVerboseM,watchTestM)
import DART.MkRandom
import DART.ModuleTester.Testable
import Data.Maybe
import DART.Util.StringUtils(separateWithNewLines)
import Language.Core.Interpreter(apply,eval)
import Language.Core.Interpreter.Acknowledge(acknowledgeModule)
import Language.Core.Interpreter.Structures
import Language.Core.Interpreter.Util(showValue)
import Language.Core.Module(moduleFindVdefByName)
import Language.Core.Ty(funTyArgs,printSignature)
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

-- | Tests a single definition with the given environment, produces a TestResult in case the definition has the proper type, a function type
-- Only one test is performed
testVdefg :: Vdefg -> Env -> IM VdefgTest
testVdefg vdefg@(Nonrec vdef) env = do
  debugMStep $ "Testing " ++ vdefId vdef
  fun_test <- testFun vdef env
  
  return $ case fun_test of 
    NoFunTest{m} -> NoVdefgTest m -- we have no test result
    FunTest tested_fun -> VdefgTest vdefg [tested_fun]
    
testVdefg vdefg@(Rec vdefs) env = do
  debugMStep $ "Testing recursive definition"
  fun_tests <- mapM (flip testFun env) vdefs
  let
    tested_funs = [tested_fun | FunTest tested_fun <- filter isTestedFun fun_tests]
    
  return $ case tested_funs of
    [] -> NoVdefgTest "We have no test results for recursive definition" -- we have no test results!
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
      let m = "Will not test " ++ zDecodeQualified qvar ++ ", it is not a function type"
      debugMessage m
      return $ NoFunTest m
    Just fun_signature -> do
      let
        -- if the function has type e.g. Int -> Int -> Bool
        -- we want to generate feed two ints only
        fun_arg_tys = init fun_signature

        -- Tests function with random values
        testOnce :: Value -> [Value] -> IM TestResult
        testOnce fun args = do
          heap_refs <- mapM memorizeVal args  -- allocate args          
          applied_fun_result <- feedFun fun heap_refs -- apply          
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
              
      typ_sig_str <- printSignature fun_signature
      debugMessage $ "Detected function with type signature: " ++ typ_sig_str
      
      -- eval the expression (it should have the function type)
      fun <- eval vdef_exp env >>= \probably_fun -> 
        return $ case probably_fun of
          Wrong s -> error s
          f@(Fun _ _) -> f
          
      ntests <- getSetting number_of_tests
      test_results <- replicateM ntests $ do
        args_maybe <- mkArgVals fun_arg_tys env
        
        case args_maybe of
          Left (Wrong err_val) -> return $ FailedTest [] err_val
          Right args -> do
            debugMessage $ "Did " ++ (show . length) args ++ " random args"        
            testOnce fun args
                
      return . FunTest $ TestedFun def test_results
  where
    mkArgVals :: [Ty] -> Env -> IM (Either Value [Value])
    mkArgVals [] _ = return . Right $ []
    mkArgVals (t:ts) env = do
      val_maybe <- mkRandomVal env t
      case val_maybe of
        Wrong e -> return . Left . Wrong $ "Generator shortcoming. Couldn't generate value for type " ++ showType t
        val -> do
          -- make further vals
          ts_vals_maybe <- mkArgVals ts env 
          case ts_vals_maybe of
            err@(Left _) -> return err
            Right ts_vals -> return . Right $ val:ts_vals
    
    -- | Given a value and a list of argument heap references, feed the value
    -- if the value is a function until the list of arguments
    -- is exhausted
    feedFun :: Value -> [HeapReference] -> IM Value
    -- the case where we have a function and no arguments, error
    feedFun (Fun _ fdesc) []  = return . Wrong $ "The impossible happened at feedFun, there is not enough arguments to feed function: "++ fdesc
    -- the case where we have a function and at least one argument, feed
    feedFun fun@(Fun _ _) (frst_ref@(frst,_):other_args) = do
      first_arg_maybe <- eval frst (frst_ref:env) -- might be an eval error!
      arg_str <- showValue first_arg_maybe
      
      case first_arg_maybe of
        eval_err@(Wrong _) -> return . Wrong $ "Can't apply argument value: " ++ arg_str
        first_arg -> do          
          watchTestM $ "Feeding function with random argument: " ++ arg_str
          result <- apply fun frst (frst_ref:env)      
          feedFun result other_args
    
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
      let m = "Couldn't find definition of " ++ id ++ " in module " ++ show mname
      debugMStep m
      return $ NoFunTest m --TODO (JIT..)

showTestedFun :: TestedFun -> IM String
showTestedFun (TestedFun vdef test_results) =
  case test_fails of
    [] -> return $ id ++ " passed all (" ++ (show . length $ test_successes) ++ ") tests"
    (test_fail:_) ->
      let 
        tst 1 = "1 test"
        tst n = show n ++ " tests"
        failed_str = id ++ " failed in " ++ (tst . length $ test_fails) ++ " \n" 
      in do
        failed_test_strs <- mapM showTestResult test_fails
        return $ failed_str ++ separateWithNewLines failed_test_strs
  where
    id = vdefId vdef
    (test_successes, test_fails) = partition isSuccessfulTest test_results
  
showTestResult :: TestResult -> IM String
showTestResult (TestSuccess _ _) = return "Test success"
showTestResult (FailedTest arg_vals message) = do
  arg_strs <- mapM showValue arg_vals
  return $ message ++ ", with arguments: \n" ++ separateWithNewLines arg_strs

showFunTest :: FunTest -> IM String
showFunTest (NoFunTest m) = return $ "Function not tested: " ++ m
showFunTest (FunTest tested_fun) = showTestedFun tested_fun
