{-# LANGUAGE ImplicitParams #-}

----------------------------------------------------------------------------
-- |
-- Module      :  DART.Run
-- Copyright   :  (c) Carlos López-Camey, University of Freiburg
-- License     :  BSD-3
--
-- Maintainer  :  c.lopez@kmels.net
-- Stability   :  stable
--
--
-- Runs the interpreter and the dart tester
-----------------------------------------------------------------------------

module DART.Run where

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
import           Language.Core.Interpreter.Util(showM)
import           Language.Core.Util(showType)
import           Language.Core.Vdefg
import           System.Directory(getCurrentDirectory)
import           System.Environment
import           Text.Encoding.Z
import           DART.CmdLine

-- | Creates an initial state given the arguments given
-- in the command line and parsed by CmdArgs
initDART :: InterpreterSettings -> IO DARTState
initDART settings = do
  h <- io H.new -- create a fresh new heap
  current_dir <- getCurrentDirectory 
  
  let prependCurrentDir = (++) (current_dir ++ "/")    
  let absolute_includes = map prependCurrentDir $ default_includes ++ (include settings)
                             
  return $ DState {
    heap = h
    , heap_count = 0
    , number_of_reductions = 0
    , number_of_reductions_part = 0
    , tab_indentation = 1
    , settings = settings { include = (absolute_includes) }    
    , test_name = Nothing
  }

-- | Returns a list of *relative* paths pointing to default included libraries e.g. base
-- Use case: if we always want the function split to be in scope for programs that we are testing, then we should load Data.List in the base package
-- The file paths are path to .hcr files, since these modules sometimes need different arguments to be compiled with -fext-core
default_includes :: [FilePath]
default_includes = [
    "/lib/base/GHC/Base.hcr"
  , "/lib/base/GHC/Base.hcr"
  , "/lib/base/Data/Tuple.hcr"
  , "/lib/base/GHC/Show.hcr"
  , "/lib/base/GHC/Enum.hcr"
  , "/lib/base/Data/Maybe.hcr"
  , "/lib/base/GHC/List.hcr"
  , "/lib/base/Data/List.hcr"
  ]
         
-- | Assumming no library has been loaded, this function looks for the settings (often coming from the command line, except when testing), loads the includes,
-- the base library and the builtin functions for the interpreter to work.
-- It retuns an environment, a list of heap references that is.
mkLibsEnv :: IM Env
mkLibsEnv = do  
  settings <- gets settings
    
  -- get the list of includes and acknowledge definitions in heap
  let includes = include settings
  lib_envs <- mapM loadFilePath includes -- :: [Env]
  
  -- builtin funs, e.g. GHC.Num.+
  ghc_builtin_funs <- I.loadLibrary Libs.ghc_base    
  return $ ghc_builtin_funs ++ concat lib_envs

-- | After an initial state is created, evaluates according to the settings
runDART :: IM ()
runDART = do  
  settgs <- gets settings      
  
  -- Evaluate specified module
  let pathToModule = file settgs
  let ?be_verbose = verbose settgs
  debugMStep $ "Reading module " ++ pathToModule  ++ " .."
  
  m@(Module mdlname tdefs vdefgs) <- io . readModule $ file settgs
  module_env <- acknowledgeModule m
  
  debugMStep ("Loading includes ")
  libs_env <- mkLibsEnv
  
  let env = module_env ++ libs_env
      eval_funname = evaluate_function settgs
      test_funname = test_function settgs
  
  -- What should we eval? a function or the whole module?
  unless (not $ null test_funname) $
    evaluate m env eval_funname
  
  -- What should we test? a function or the whole module?  
  unless (not $ null eval_funname) $ 
    test m env test_funname
  
  where
    test :: Module -> Env -> String -> IM ()
    -- | No function specified
    test m env [] = do
      results <- T.testModule m env
      
      let prettyPrint :: (Id,T.TestResult) -> IM String
          prettyPrint (id,test_result) = T.showTest test_result >>= return . (++) (id ++ ": \n")

      io . putStrLn $ "**************************************************"      
      io . putStrLn $ "Module test results "
      io . putStrLn $ "**************************************************"      
      mapM prettyPrint results >>= io . mapM_ putStrLn
      
      h <- gets heap
      whenFlag show_heap $ io . printHeap $ h
    
    -- test specified function
    test m env fun_name =             
      let prettyPrint :: Maybe (Id,T.TestResult) -> IM String
          prettyPrint Nothing = return $ "No test result "
          prettyPrint (Just (id,test_result)) = T.showTest test_result >>= return . (++) (id ++ ": \n")
      in do
        res <- T.testHaskellExpression m fun_name env >>= prettyPrint
        io . putStrLn $ "**************************************************"
        io . putStrLn $ "Test results of " ++ fun_name
        io . putStrLn $ "**************************************************"
        io . putStrLn $ res
        (gets heap >>= \h -> whenFlag show_heap $ io . printHeap $ h)
      
    evaluate :: Module -> Env -> String -> IM () 
    -- | no function specified  
    evaluate m env [] = do
      vals <- I.evalModule m env -- interpret values
      
      -- funt ion to pretty print
      let prettyPrint :: (Id,Value) -> IM String
          prettyPrint (id,val) = showM val >>= return . (++) (id ++ " => ")
      
      io . putStrLn $ "**************************************************"      
      io . putStrLn $ "Module definitions evaluation: "
      io . putStrLn $ "**************************************************"
      
      mapM prettyPrint vals >>= io . mapM_ putStrLn
      
      h <- gets heap
      st <- gets settings
      when (show_heap st) $ io . printHeap $ h
  
    -- | eval fun_name
    evaluate m env fun_name = do 
      debugM $ "evaluate fun_name; env.size == " ++ (show . length $ env)
      result <- I.evalHaskellExpression m fun_name env
      
      -- do we print the heap?
      h <- gets heap
      st <- gets settings
      when (show_heap $ st) $ io . printHeap $ h
      
      -- output computed result
      io . putStrLn $ "**************************************************"
      io . putStrLn $ "Evaluation of " ++ fun_name      
      io . putStrLn $ "**************************************************"
      showM result >>= io . putStrLn

