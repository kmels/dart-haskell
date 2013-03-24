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

import           Language.Core.Core
import           System.Environment
import           Language.Core.Util(showType)
import           Language.Core.Interpreter.Structures

import           Control.Monad.State.Lazy
import           DART.CmdLine
import           DART.FileIO
import           DART.FunctionFeeder
import           DART.InterpreterSettings
import           DART.MkRandomValue
import qualified Data.HashTable.IO as H
import           Data.Maybe
import           Language.Core.Interp
import qualified Language.Core.Interpreter as I
import qualified Language.Core.Interpreter.Libraries as Libs
import           Language.Core.Vdefg
import           System.Console.CmdArgs
import           System.Directory(getCurrentDirectory)
import           Text.Encoding.Z
main :: IO () 
main = cmdArgs interpret >>= newState >>= evalStateT processModule 

-- | Creates an initial state

newState :: InterpreterSettings -> IO DARTState
newState s = do
  h <- io H.new -- get a new heap
  current_dir <- getCurrentDirectory 
  let --list = current_dir ++ "/lib/base/Data/List.hs"  
       --char = current_dir ++ "/lib/base/Data/Char.hs"  
--    enum = current_dir ++ "/lib/base/GHC/Enum.hcr"  
    absolute_includes = (map ((++) (current_dir ++ "/")) $ include s) -- ++ 
                        -- ++ [enum]
  return $ DState {
    heap = h
    , heap_count = 0
    , number_of_reductions = 0
    , tab_indentation = 1
    , settings = s { include = (absolute_includes) }
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
  (env',mem) <- io $ runStateT (I.loadLibrary Libs.ghc_base) st -- implemented functions
  lib_envs <- mapM loadFilePath (include settgs)  -- acknowledged from source code
  
  let env = concat lib_envs ++ env'
--  loadFile_ "lib/base/Data/List.hs"
  
  case (evaluate_function settgs) of
    -- What should we eval?
    "" -> do  -- not specified
      (vals,state) <- io $ runStateT (I.evalModule module' env) mem
      let h = heap state
      io . putStrLn $ "WARNING: You did not specify a function name to eval (flags --eval or -e), that's why I evaluated all values"
      
      let prettyPrint :: (Id,Value) -> IM String
          prettyPrint (id,val) = showM val >>= return . show . (,) id
          
      prettyPrintedVals <- mapM prettyPrint vals
      
      io $ mapM_ putStrLn prettyPrintedVals
      when (show_heap $ ?settings) $ io . printHeap $ h
    fun_name -> do -- eval fun_name
      (result,state) <- io $ runStateT (I.evalModuleFunction module' fun_name env) mem
      when (show_heap $ ?settings) $ io . printHeap $ (heap state)
      io . putStrLn $ show result -- will be a Value iff fun_name is not null

-- | Decode any string encoded as Z-encoded string and print it

putZDecStrLn = putStrLn . zDecodeString



