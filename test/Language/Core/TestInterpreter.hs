----------------------------------------------------------------------------
-- |
-- Module      :  Language.Core.TestInterpreter
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
module Language.Core.TestInterpreter(test) where

import Test.HUnit(Test(..))
  
import qualified Language.Core.Interpreter.Libraries.GHC.TestNum  as GHC.Num

test :: Test
test = TestList [GHC.Num.test]
