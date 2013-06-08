----------------------------------------------------------------------------
-- |
-- Module      :  DART.ModuleTester.Testable
-- Copyright   :  (c) Carlos López-Camey, University of Freiburg
-- License     :  BSD-3
--
-- Maintainer  :  c.lopez@kmels.net
-- Stability   :  stable
--
--
-- Contains a type class that describes data types that can be tested
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}
module DART.ModuleTester.Testable where

import DART.CmdLine(watchTestM,debugMStep,debugM)
import DART.MkRandom
import Data.List(find)
import Language.Core.Core
import Language.Core.Interpreter
import Language.Core.Interpreter.Acknowledge
import Language.Core.Vdefg (vdefgNames, findVdefg)
import Text.Encoding.Z(zEncodeString)

-- | When we test a vdefg, we want to have the function tested more than once
-- in case the value definition is not testable, it's represented as NoTest
data FunTest = FunTest TestedFun | NoFunTest
data TestedFun = TestedFun Vdef [TestResult]
data VdefgTest = VdefgTest Vdefg [TestedFun] | NoVdefgTest

-- | A test result comprehends the result of feeding a function with 
-- random values and checking whether it flows normally or not
data TestResult = TestSuccess {
    test_argument_values :: [Value]
    , test_result_value :: Value
  } | FailedTest {
    test_argument_values :: [Value]
    , failed_test_message :: String
  }
   
-- | We'd like to filter the successful tests
isSuccessfulTest :: TestResult -> Bool
isSuccessfulTest (FailedTest _ _) = False
isSuccessfulTest _ = True
  
-- | We'd like to filter the value definitions that were tested
isTestedFun :: FunTest -> Bool
isTestedFun NoFunTest = False
isTestedFun _ = True

-- | We'd like to filter the actual test results for a value definition
isTestedVdefg :: VdefgTest -> Bool
isTestedVdefg NoVdefgTest = False
isTestedVdefg _ = True

--  vdefg_name :: Qual Var,
--  test_expression :: Exp,
--  test_value :: Value
--} | TestResultList [TestResult]

-- | A class for types `T` that comprehend values for which we can compute a test.
-- If an instance is declared, there must exist a function that evaluates 
-- an expression given a value of type `T`
class TestableType t where
  testType :: t -> Exp -> Env -> IM Value

-- instance TestableType LambdaAbstraction where
--   testType (LambdaAbstraction concrete_type general_type) lambda_exp env = do
--     --watchTestM $ " Generating a " ++ concrete_type
--     fun <- eval lambda_exp env
--     heap_ref@(rndval_id,_) <- mkRandomHR concrete_type env
--     apply fun rndval_id (heap_ref:env)

-- class MaybeTestable a where
--   testMaybe :: a -> Maybe (Qual Var) -> Maybe Exp -> Env -> IM (Maybe TestResult)

-- instance MaybeTestable GeneralType where  
--   testMaybe (Lambda lambda_abstraction) (Just qual_var) (Just exp) env = testType lambda_abstraction exp env >>= return . Just . mkTestResult qual_var exp
--   testMaybe ty _ _ _ = error $ "undefined "  ++ show ty
  
--mkTestResult :: Qual Var -> Exp -> Value -> TestResult
--mkTestResult n e v = TestResult n e v

