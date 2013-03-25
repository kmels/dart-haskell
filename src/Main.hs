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
main = cmdArgs interpret >>= initDART >>= evalStateT runDART 

-- | Creates an initial state given the arguments given
-- in the command line and parsed by CmdArgs
initDART :: InterpreterSettings -> IO DARTState
initDART s = do
  h <- io H.new -- create a fresh new heap
  current_dir <- getCurrentDirectory 
  let --list = current_dir ++ "/lib/base/Data/List.hs"  
       --char = current_dir ++ "/lib/base/Data/Char.hs"
    ghc_list = current_dir ++ "/lib/base/Prelude.hcr"  
    data_list = current_dir ++ "/lib/base/Data/List.hcr"  
    enum = current_dir ++ "/lib/base/GHC/Enum.hcr"  
    absolute_includes = (map ((++) (current_dir ++ "/")) $ include s) -- ++ 
                        ++ [enum]
                        ++ [ghc_list]
  return $ DState {
    heap = h
    , heap_count = 0
    , number_of_reductions = 0
    , number_of_reductions_part = 0
    , tab_indentation = 1
    , settings = s { include = (absolute_includes) }
  }

-- | After an initial state is created, evaluates according to the settings
runDART :: IM ()
runDART = do  
  settgs <- gets settings  
  
  -- Load includes  
  debugMStep $ "Loading includes "
  let includes = include settgs
  lib_envs <- mapM loadFilePath includes -- read source code files
  
  -- Load GHC definitions like GHC.Num.+
  ghc_defs <- I.loadLibrary Libs.ghc_base

  -- Evaluate specified module
  let pathToModule = file settgs
  debugMStep $ "Reading module " ++ pathToModule  ++ " .."
  m@(Module mdlname tdefs vdefgs) <- io . readModule $ file settgs

  -- What should we eval? a function or the whole module?
  evaluate m (concat lib_envs ++ ghc_defs) (evaluate_function settgs)
  where
    evaluate :: Module -> Env -> String -> IM () 
    -- | no function specified  
    evaluate m env [] = do 
      vals <- I.evalModule m env -- interpret values
      
      -- funtion to pretty print
      let prettyPrint :: (Id,Value) -> IM String
          prettyPrint (id,val) = showM val >>= return . (++) (id ++ " => ")
        
      mapM prettyPrint vals >>= io . mapM_ putStrLn
      
      h <- gets heap
      st <- gets settings
      when (show_heap st) $ io . printHeap $ h
  
    -- | eval fun_name
    evaluate m env fun_name = do 
      result <- I.evalModuleFunction m fun_name env
      
      -- do we print the heap?
      h <- gets heap
      st <- gets settings
      when (show_heap $ st) $ io . printHeap $ h
      
      -- output computed result
      showM result >>= io . putStrLn

-- | Decode any string encoded as Z-encoded string and print it
putZDecStrLn = putStrLn . zDecodeString
