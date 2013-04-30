----------------------------------------------------------------------------
-- |
-- Module      :  Main (located in test/Language.Core.TestInterpreter)
-- Copyright   :  (c) Carlos López-Camey, University of Freiburg
-- License     :  BSD-3
--
-- Maintainer  :  c.lopez@kmels.net
-- Stability   :  stable
--
--
-- Tests Language.Core.Interpreter and derivates
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}
module Main where

import Control.Monad(when)
import System.Exit (exitFailure)
import Test.HUnit
  
import qualified Language.Core.Interpreter.Libraries.GHC.TestNum  as GHC.Num

main = do
  counts <- runTestTT $ TestList [GHC.Num.test]  
  when (errors counts > 0 || failures counts > 0)
    exitFailure
