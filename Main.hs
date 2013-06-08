{-# LANGUAGE ImplicitParams #-}

----------------------------------------------------------------------------
-- |
-- Module      :  Language.Core.Interpreter
-- Copyright   :  (c) Carlos López-Camey, University of Freiburg
-- License     :  BSD-3
--
-- Maintainer  :  c.lopez@kmels.net
-- Stability   :  stable
--
--
-- This program reads a program in Haskell and evaluates value definitions in it.
-----------------------------------------------------------------------------

module Main where

import Control.Monad.State.Lazy
import DART.CmdLine
import DART.DARTSettings
import DART.Run
import System.Console.CmdArgs

main :: IO () 
main = do
  cmdArgs <- cmdArgs interpret -- arguments from the command line
  configArgs <- configSettings -- settings from ~/.dart-haskell config file
  settings <- cmdArgs `mergeConfigSettings` configArgs
  initDART settings >>= evalStateT runDART 
