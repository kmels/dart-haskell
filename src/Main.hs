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

import Language.Core.Core
import System.Environment
import Language.Core.Util(showType)
import Language.Core.Interpreter.Structures

import Text.Encoding.Z
import DART.MkRandomValue
import Data.Maybe
import Language.Core.Vdefg
import DART.FunctionFeeder
import qualified Language.Core.Interpreter as Interpreter
import Language.Core.Interp
import Control.Monad.State.Lazy
import qualified Data.HashTable.IO as H
import DART.CmdLine
import DART.FileIO
import DART.InterpreterSettings
import System.Console.CmdArgs

main :: IO () 
main = cmdArgs interpret >>= newState >>= evalStateT processModule 

-- | Creates an initial state

newState :: InterpreterSettings -> IO DARTState
newState s = do
  h <- io H.new -- get a new heap
  return $ DState {
    heap = h
    , number_of_reductions = 0
    , tab_indentation = 0
    , settings = s
  }

processModule :: IM ()
processModule = do
  st <- get
  settgs <- gets settings
  debugM $ "Reading " ++ file settgs  ++ " .."
  module'@(Module mdlname tdefs vdefgs) <- io . readModule $ file settgs
  debugM $ "Read module " ++ show mdlname

  -- Time to evaluate
  
  debugM $ "Loading libraries "
  let ?settings = settgs
  (_,libs) <- io $ runStateT (Interpreter.loadLibraries) st
  
  case (eval settgs) of
    -- What should we eval?
    "" -> do  -- not specified
      (_,state) <- io $ runStateT (Interpreter.evalModule module') libs
      let h = heap state
      when (not . show_heap $ ?settings) $ io . putStrLn $ "WARNING: You did not specify a function name to eval (flags --eval or -e), neither the flag --show-heap. That is why this program has no output"
      when (show_heap $ ?settings) $ io . printHeap $ h
    fun_name -> do -- eval fun_name
      (result,state) <- io $ runStateT (module' `Interpreter.evalModuleFunction` fun_name) libs
      when (show_heap $ ?settings) $ io . printHeap $ (heap state)
      io . putStrLn $ show result -- will be a Value iff fun_name is not null

-- | Decode any string encoded as Z-encoded string and print it

putZDecStrLn = putStrLn . zDecodeString

showVdefg :: Vdefg -> String
showVdefg (Rec vdefs) = "Rec " ++ concatMap showVdef vdefs
showVdefg (Nonrec vdef) = "Nonrec " ++ showVdef vdef

showVdef :: Vdef -> String
showVdef (Vdef ((mname,var),ty,exp)) = var ++ " :: " ++ showType ty

