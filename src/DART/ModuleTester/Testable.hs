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

import DART.ExtCore.TypeExtractor
import Language.Core.Core
import Language.Core.Interpreter

data TestResult = TestResult{
  vdefg_name :: Qual Var,
  test_expression :: Exp,
  test_value :: Value
} deriving Show

class Testable a where
  test :: a -> Exp -> IM TestResult

class MaybeTestable a where
  testMaybe :: a -> IM (Maybe TestResult)
    
instance MaybeTestable Vdefg where
  testMaybe (Rec vdefs) = undefined --test vdefs
  testMaybe (Nonrec vdef) = testMaybe vdef
  
instance MaybeTestable Vdef where
  testMaybe (Vdef (qual_var,ty,exp)) = 
    case extractType ty of 
      Nothing -> return Nothing
      Just l@(Lambda _) -> testMaybe (l,exp)
      Just _ -> undefined
  
instance Testable LambdaAbstraction where
  test (LambdaAbstraction concrete_type general_type) = undefined
  
instance MaybeTestable (GeneralType,Exp) where
  testMaybe (Lambda lambda_abstraction,exp) = test lambda_abstraction exp >>= return . Just
  testMaybe _ = undefined
  
