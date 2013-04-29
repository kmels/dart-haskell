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
) where

import DART.FileIO
import Language.Core.Interpreter
import DART.InterpreterSettings
import System.IO.Unsafe
import DART.Run
