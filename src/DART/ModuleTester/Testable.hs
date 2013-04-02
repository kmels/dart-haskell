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

import DART.CmdLine(watchTestM)
import DART.ExtCore.TypeExtractor
import DART.MkRandom
import Language.Core.Core
import Language.Core.Interpreter
data TestResult = TestResult{
  vdefg_name :: Qual Var,
  test_expression :: Exp,
  test_value :: Value
} deriving Show

class TestableType t where
  testExp :: t -> Exp -> Env -> IM Value

class MaybeTestable a where
  testMaybe :: a -> Maybe (Qual Var) -> Maybe Exp -> Env -> IM (Maybe TestResult)
    
instance MaybeTestable Vdefg where
  testMaybe (Rec vdefs) _ env = error "Undefined Rec" --test vdefs
  testMaybe (Nonrec vdef) _ env = testMaybe vdef Nothing env
  
instance MaybeTestable Vdef where
  testMaybe (Vdef (qual_var,ty,exp)) _ _ env = 
    case extractType ty of 
      Nothing -> return Nothing
      Just l@(Lambda _) -> testMaybe l (Just qual_var) (Just exp) env
      Just _ -> undefined
  
instance TestableType LambdaAbstraction where
  testExp (LambdaAbstraction concrete_type general_type) lambda_exp env = case concrete_type of
    PList _ -> error "undefined PList"
    PType ty@(PrimitiveIntType ty_str) -> do
      watchTestM $ " Testing Int: " ++ ty_str
      fun <- eval lambda_exp env
      (arg_id,arg_env) <- mkRandomHR ty
      apply fun arg_id env
    PType primType -> error $ "undefined PType " ++ show primType
  
instance MaybeTestable GeneralType where  
  testMaybe (Lambda lambda_abstraction) (Just qual_var) (Just exp) env = testExp lambda_abstraction exp env >>= return . Just . mkTestResult qual_var exp  
  testMaybe ty _ _ _ = error $ "undefined "  ++ show ty
  
mkTestResult :: Qual Var -> Exp -> Value -> TestResult
mkTestResult n e v = TestResult n e v

