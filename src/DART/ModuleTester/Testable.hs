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
import DART.ExtCore.TypeExtractor
import DART.MkRandom
import Data.List(find)
import Language.Core.Core
import Language.Core.Interpreter
import Language.Core.Interpreter.Acknowledge
import Language.Core.Vdefg (vdefgNames, findVdefg)

data TestResult = TestResult{
  vdefg_name :: Qual Var,
  test_expression :: Exp,
  test_value :: Value
} 

instance Show TestResult where
  show (TestResult name exp val) = qualifiedVar name ++ " => " ++ show val

-- | A class for types `T` that comprehend values for which we can compute a test.
-- If an instance is declared, there must exist a function that evaluates 
-- an expression given a value of type `T`
class TestableType t where
  testType :: t -> Exp -> Env -> IM Value

instance TestableType LambdaAbstraction where
  testType (LambdaAbstraction concrete_type general_type) lambda_exp env = case concrete_type of
    PList _ -> error "undefined PList"
    PType ty@(PrimitiveIntType ty_str) -> do
      watchTestM $ " Testing Int: " ++ ty_str
      fun <- eval lambda_exp env
      heap_ref@(rndval_id,_) <- mkRandomHR ty
      apply fun rndval_id (heap_ref:env)
    PType primType -> error $ "undefined PType " ++ show primType

class MaybeTestable a where
  testMaybe :: a -> Maybe (Qual Var) -> Maybe Exp -> Env -> IM (Maybe TestResult)

instance MaybeTestable GeneralType where  
  testMaybe (Lambda lambda_abstraction) (Just qual_var) (Just exp) env = testType lambda_abstraction exp env >>= return . Just . mkTestResult qual_var exp
  testMaybe ty _ _ _ = error $ "undefined "  ++ show ty
  
mkTestResult :: Qual Var -> Exp -> Value -> TestResult
mkTestResult n e v = TestResult n e v

