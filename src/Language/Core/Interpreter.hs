{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE DoAndIfThenElse #-}

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
-- Interprets External Core expressions non-strictly
-----------------------------------------------------------------------------

module Language.Core.Interpreter(
  evalModule, evalModuleFunction,
  loadLibrary,
  module Language.Core.Interpreter.Evaluable
  )where

import           Language.Core.Interpreter.Apply
import           Language.Core.Interpreter.Acknowledge(acknowledgeTypes, 
                                                       acknowledgeVdefgs,
                                                       acknowledgeVdefg,
                                                       acknowledgeVdefgWithin)
import           Language.Core.Interpreter.Util(return')
import           Language.Core.Interpreter.Structures
import           Language.Core.Interpreter.Evaluable
  
import           Control.Applicative((<|>))
import qualified Data.HashTable.IO as H
import           Data.Maybe
import           Language.Core.Core
import           Language.Core.TypeExtractor(extractType)
import           Language.Core.TypeExtractor.DataTypes
import           Language.Core.Util(qualifiedVar,showVdefg,showType,showExtCoreType,showExp,bindVarName,showBind)
import           Language.Core.Vdefg (isTmp,vdefgId,vdefgName)

import           Control.Monad.State.Lazy
import           DART.CmdLine
import           DART.FileIO
import           DART.InterpreterSettings
import           Data.Time.Clock(getCurrentTime,diffUTCTime)
import           Text.Encoding.Z(zDecodeString)
-- data & control
import           Data.List(intersectBy)

{-Given a module which contains a list of value definitions, *vd*, evaluate every *vd* and return a heap with their interpreted values.

Value definition to mapped values
-----------------------------------------------------
| Value definition type              | mapped to    |
| Concrete type e.g. Int             | Num val      |
| Concrete type e.g. Int - Int      | ?            |
-----------------------------------------------------
-}


evalModule :: (?settings :: InterpreterSettings) => Module -> Env -> IM [(Id, Value)]
evalModule m@(Module name tdefs vdefgs) libs_env = do
  debugM $ "Evaluating module with env: " ++ show libs_env
  -- recognize type and value definitions
  tycons_env <- acknowledgeTypes m
  vdefs_env <- acknowledgeVdefgs m --libs_env
  
  -- time to evaluate, set an environment and evaluate only those defs that are not temp
  let 
    env = tycons_env ++ vdefs_env ++ libs_env
    exposed_vdefgs = filter (not . isTmp) vdefgs    
  heap_refs <- mapM (flip doEvalVdefg env) exposed_vdefgs
  
  -- lookup values in memory
  vals <- mapM (flip (evalHeapAddress . snd) []) heap_refs --fun_heap_refs
  return $ zip (map fst heap_refs) vals

-- | Given a module and a function name, we evaluate the function in that module and return the heap. 

evalModuleFunction :: (?settings :: InterpreterSettings) => Module -> String -> Env -> IM Value
evalModuleFunction m fun_name env = eval (ModuleFunction fun_name m) env
  
--bindAltVars val@(Num n) (Acon _ _ [(var,_)] _) = var `bindTo` val
--bindAltVars t v = io . putStrLn $ " Don't know how to bind values " ++ show t ++ " to " ++ show v

isAcon :: Alt -> Bool
isAcon (Acon _ _ _ _) = True
isAcon _ = False


-- | Loads nothing ATM, but it'll be useful
loadLibrary :: (?settings :: InterpreterSettings) => [(Id, Either Thunk Value)] -> IM Env
loadLibrary funs = mapM (uncurry $ flip memorize) funs

