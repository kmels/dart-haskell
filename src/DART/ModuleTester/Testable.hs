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
