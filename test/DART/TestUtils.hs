{-# LANGUAGE ScopedTypeVariables #-}
 ----------------------------------------------------------------------------
-- |
-- Module      :  DART.Test.Utils
-- Copyright   :  (c) Carlos López-Camey, University of Freiburg
-- License     :  BSD-3
--
-- Maintainer  :  c.lopez@kmels.net
-- Stability   :  stable
--
--
-- This module contains useful functions for testing, it also exports some useful modules
-----------------------------------------------------------------------------
module DART.TestUtils(
  module DART.FileIO
  , module System.IO.Unsafe
  , module Language.Core.Interpreter
  , module DART.InterpreterSettings
  , module DART.Run
  , module Test.HUnit
  , checkExpected, checkExpectedProperties
) where

import DART.FileIO
import Language.Core.Interpreter
import DART.InterpreterSettings
import System.IO.Unsafe
import DART.Run
import Test.HUnit hiding (test,State)

checkExpected :: forall a . (Eq a, Show a) => [(Id,a)] -> [(Id,a)] -> Test
checkExpected results expected = TestList $ map check expected
  where
    -- Checks one expected
    check :: (Eq a, Show a) => (Id,a) -> Test
    check (id,val) = lookup id results ~=? (Just $ val)

checkExpectedProperties :: forall a . (Eq a, Show a) => [(Id,a)] -> [(Id,a -> Bool)] -> Test
checkExpectedProperties results expectedProps = TestList $ map check expectedProps
  where
    -- Checks one expected
    check :: (Eq a, Show a) => (Id,a -> Bool) -> Test
    check (id,testProperty) = case lookup id results of
      Just val -> TestCase . assert . testProperty $ val
      Nothing -> TestCase $ assertFailure $ "The identifier " ++ id ++ " was not found in the results"
    
