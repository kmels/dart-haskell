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

import           Control.Monad.State.Lazy
import           DART.CmdLine
import           DART.FileIO
import           DART.FunctionFeeder
import           DART.InterpreterSettings
import           DART.MkRandomValue
import qualified DART.ModuleTester as T
import qualified Data.HashTable.IO as H
import           Data.Maybe
import           Language.Core.Core
import           Language.Core.Interp
import qualified Language.Core.Interpreter as I
import           Language.Core.Interpreter.Acknowledge(acknowledgeModule)
import qualified Language.Core.Interpreter.Libraries as Libs
import           Language.Core.Interpreter.Structures
import           Language.Core.Util(showType)
import           Language.Core.Vdefg
import           System.Console.CmdArgs
import           System.Directory(getCurrentDirectory)
import           System.Environment
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
       base = current_dir ++ "/lib/base/GHC/Base.hcr"
  --    ghc_list = current_dir ++ "/lib/base/Prelude.hs"  
       data_list = current_dir ++ "/lib/base/Data/List.hcr"  
--       data_list = current_dir ++ "/lib/base-4.6.0.0/GHC/Show.hcr" 
       enum = current_dir ++ "/lib/base/GHC/Enum.hs"  
       absolute_includes = (map ((++) (current_dir ++ "/")) $ include s) -- ++ 
                        ++ [enum]
                        ++ [base] ++ [data_list]
       --                 ++ [ghc_list]
  return $ DState {
    heap = h
    , heap_count = 0
    , number_of_reductions = 0
    , number_of_reductions_part = 0
    , tab_indentation = 1
    , settings = s { include = (absolute_includes) }    
    , test_name = Nothing
  }

-- | After an initial state is created, evaluates according to the settings
runDART :: IM ()
runDART = do  
  settgs <- gets settings  
  
  -- Load includes  
  debugMStep ("Loading includes ")
  
  let includes = include settgs
  lib_envs <- mapM loadFilePath includes -- read source code files
  
  -- Load GHC definitions like GHC.Num.+
  ghc_defs <- I.loadLibrary Libs.ghc_base

  -- Evaluate specified module
  let pathToModule = file settgs
  let ?be_verbose = verbose settgs
  debugMStep $ "Reading module " ++ pathToModule  ++ " .."
  
  m@(Module mdlname tdefs vdefgs) <- io . readModule $ file settgs
  module_env <- acknowledgeModule m
  
  let env = concat lib_envs ++ ghc_defs ++ module_env
      eval_funname = evaluate_function settgs      
  -- What should we eval? a function or the whole module?  
  evaluate m env eval_funname
  
  -- What should we test? a function or the whole module?  
  when (not (null eval_funname)) $ test m env (test_funcion settgs)
  
  where
    test :: Module -> Env -> String -> IM ()
    -- | No function specified
    test m env [] = do
      results <- T.testModule m env
      
      let prettyPrint :: (Id,T.TestResult) -> IM String
          prettyPrint (id,test_result) = T.showTest test_result >>= return . (++) (id ++ ": \n")

      debugMStep $ "Module test results "
      mapM prettyPrint results >>= io . mapM_ putStrLn
      
      h <- gets heap
      whenFlag show_heap $ io . printHeap $ h
    
    -- test specified function
    test m env fun_name =             
      let prettyPrint :: Maybe (Id,T.TestResult) -> IM String
          prettyPrint Nothing = return $ "No test result "
          prettyPrint (Just (id,test_result)) = T.showTest test_result >>= return . (++) (id ++ ": \n")
      in do
        res <- T.testFunction m fun_name env >>= prettyPrint
        debugMStep $ "Test results of " ++ fun_name
        io . putStrLn $ res
        (gets heap >>= \h -> whenFlag show_heap $ io . printHeap $ h)
      
    evaluate :: Module -> Env -> String -> IM () 
    -- | no function specified  
    evaluate m env [] = do
      vals <- I.evalModule m env -- interpret values
      
      -- funt ion to pretty print
      let prettyPrint :: (Id,Value) -> IM String
          prettyPrint (id,val) = showM val >>= return . (++) (id ++ " => ")
        
      debugMStep $ "Module definitions evaluation: "
      mapM prettyPrint vals >>= io . mapM_ putStrLn
      
      h <- gets heap
      st <- gets settings
      when (show_heap st) $ io . printHeap $ h
  
    -- | eval fun_name
    evaluate m env fun_name = do 
      debugM $ "evaluate fun_name; env.size == " ++ (show . length $ env)
      result <- I.evalModuleFunction m fun_name env
      
      -- do we print the heap?
      h <- gets heap
      st <- gets settings
      when (show_heap $ st) $ io . printHeap $ h
      
      -- output computed result
      debugMStep $ "Evaluation of " ++ fun_name
      showM result >>= io . putStrLn

-- | Decode any string encoded as Z-encoded string and print it
putZDecStrLn = putStrLn . zDecodeString
