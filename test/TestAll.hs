----------------------------------------------------------------------------
-- |
-- Module      :  Main (located in test/TestAll)
-- Copyright   :  (c) Carlos López-Camey, University of Freiburg
-- License     :  BSD-3
--
-- Maintainer  :  c.lopez@kmels.net
-- Stability   :  stable
--
--
-- Runs all the tests
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}
module Main where

import Control.Monad(when)
import System.Exit (exitFailure)
import Test.HUnit
  
import qualified Language.Core.TestInterpreter  as Interpreter
import qualified Language.Core.TestTy  as ExtCore.Ty

main = do
  counts <- runTestTT $ TestList [Interpreter.test, ExtCore.Ty.test]
  when (errors counts > 0 || failures counts > 0)
    exitFailure
