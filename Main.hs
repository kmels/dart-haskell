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

import           DART.InterpreterSettings
import           DART.Run
import           System.Console.CmdArgs
import           Control.Monad.State.Lazy

main :: IO () 
main = cmdArgs interpret >>= initDART >>= evalStateT runDART 
