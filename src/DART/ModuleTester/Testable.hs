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
import Language.Core.Vdefg (vdefgNames, findVdef)

data TestResult = TestResult{
  vdefg_name :: Qual Var,
  test_expression :: Exp,
  test_value :: Value
} 

instance Show TestResult where
  show (TestResult name exp val) = qualifiedVar name ++ " => " ++ show val

class TestableType t where
  testExp :: t -> Exp -> Env -> IM Value

class MaybeTestable a where
  testMaybe :: a -> Maybe (Qual Var) -> Maybe Exp -> Env -> IM (Maybe TestResult)

instance MaybeTestable HaskellExpression where
  testMaybe (HaskellExpression expression_string m@(Module mname _ vdefgs)) _ _ env = 
    case (m `findVdef` expression_string) of
      Just vdefg -> debugMStep ("Testing function " ++ expression_string)
                    >> testMaybe (ModuleFunction vdefg m) Nothing Nothing env
      Nothing -> return Nothing --TODO
  
instance MaybeTestable ModuleFunction where
  testMaybe (ModuleFunction vdefg m@(Module mname tdefs vdefgs)) _ _ env = do    
    module_env <- acknowledgeModule m
    testMaybe vdefg Nothing Nothing (module_env ++ env)

instance MaybeTestable Vdefg where
  testMaybe (Rec vdefs) _ _ env = return Nothing -- error "Undefined Rec" --test vdefs
  testMaybe (Nonrec vdef) _ _ env = testMaybe vdef Nothing Nothing env
  
instance MaybeTestable Vdef where
  testMaybe (Vdef (qual_var,ty,exp)) _ _ env = do
    debugMStep $ "Testing value definition : " ++ qualifiedVar qual_var
    case extractType ty of 
      Just l@(Lambda _) -> testMaybe l (Just qual_var) (Just exp) env
      Nothing -> return Nothing
      Just x -> error $ " Undefined Testable: " ++ show x
  
instance TestableType LambdaAbstraction where
  testExp (LambdaAbstraction concrete_type general_type) lambda_exp env = case concrete_type of
    PList _ -> error "undefined PList"
    PType ty@(PrimitiveIntType ty_str) -> do
      watchTestM $ " Testing Int: " ++ ty_str
      fun <- eval lambda_exp env
      heap_ref@(rndval_id,_) <- mkRandomHR ty
      apply fun rndval_id (heap_ref:env)
    PType primType -> error $ "undefined PType " ++ show primType
  
instance MaybeTestable GeneralType where  
  testMaybe (Lambda lambda_abstraction) (Just qual_var) (Just exp) env = testExp lambda_abstraction exp env >>= return . Just . mkTestResult qual_var exp
  testMaybe ty _ _ _ = error $ "undefined "  ++ show ty
  
mkTestResult :: Qual Var -> Exp -> Value -> TestResult
mkTestResult n e v = TestResult n e v

