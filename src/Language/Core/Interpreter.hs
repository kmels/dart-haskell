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
  evalModule, evalHaskellExpression,
  loadLibrary,
  -- from apply
  lookupId, 
  module Language.Core.Interpreter.Acknowledge,
  module Language.Core.Interpreter.Evaluable,
  module Language.Core.Interpreter.Structures
  )where

import           Language.Core.Interpreter.Apply
import           Language.Core.Interpreter.Acknowledge(acknowledgeModule,
                                                       acknowledgeTypes, 
                                                       acknowledgeVdefgs,
                                                       acknowledgeVdefg)
import           Language.Core.Interpreter.Structures
import           Language.Core.Interpreter.Evaluable
  
import           Control.Applicative((<|>))
import qualified Data.HashTable.IO as H
import           Data.Maybe
import           Language.Core.Core
import           Language.Core.Util(qualIsTmp,qualifiedVar,showVdefg,showType,showExtCoreType,showExp,bindVarName,showBind)
import           Language.Core.Vdefg (vdefgQualVars)

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

evalModule :: Module -> Env -> IM [(Id, Value)]
evalModule m@(Module mname tdefs vdefgs) env = do
  debugMStep $ "Evaluating module " ++ show mname
  -- recognize type and value definitions
  --module_env <- acknowledgeModule m
    
  -- time to evaluate, set an environment and evaluate only those defs that are not temp
  let 
    --env = libs_env --module_env ++ libs_env    
    qual_vars = map vdefgQualVars vdefgs -- [[Qual Var]]
    exposed_vdefgs = concat $ filter (not . all qualIsTmp) qual_vars     --[Qual Var]
    vdefg_names = map qualifiedVar exposed_vdefgs -- exposed vdefs, [Qual Var]
    
  vals <- mapM evalModuleDef vdefg_names -- [Value]  
  return $ zip vdefg_names vals
  where
    evalModuleDef :: Id -> IM Value
    evalModuleDef id = debugMStep ("Evaluating " ++ id) >> evalId id env
    
-- | Given a module and a function name, we evaluate the function in that module and return the heap. 
evalHaskellExpression :: Module -> String -> Env -> IM Value
evalHaskellExpression m expression_string env = eval (HaskellExpression expression_string m) env
  
--bindAltVars val@(Num n) (Acon _ _ [(var,_)] _) = var `bindTo` val
--bindAltVars t v = io . putStrLn $ " Don't know how to bind values " ++ show t ++ " to " ++ show v

isAcon :: Alt -> Bool
isAcon (Acon _ _ _ _) = True
isAcon _ = False

-- | Loads nothing ATM, but it'll be useful
loadLibrary :: [(Id, Either Thunk Value)] -> IM Env
loadLibrary funs = mapM (uncurry $ flip memorize) funs

