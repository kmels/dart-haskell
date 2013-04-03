{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE DoAndIfThenElse #-}

----------------------------------------------------------------------------
-- |
-- Module      :  Language.Core.Interpreter.Acknowledge
-- Copyright   :  (c) Carlos López-Camey, University of Freiburg
-- License     :  BSD-3
--
-- Maintainer  :  c.lopez@kmels.net
-- Stability   :  stable
--
--
-- When the interpreter reads a module, it should acknowledge type and value definitions
-- and save them in the heap, before going on to lazily evaluation. 
-----------------------------------------------------------------------------
module Language.Core.Interpreter.Acknowledge(acknowledgeModule,acknowledgeTypes,acknowledgeVdefgs,acknowledgeVdefg, acknowledgeVdefgWithin) where

import Language.Core.Interpreter.Structures
import Language.Core.Util
import Language.Core.Core
import DART.CmdLine(beVerboseM)
import DART.InterpreterSettings

-- | Given a module, recognize type constructors and value definitions
-- and save them in the heap
acknowledgeModule :: Module -> IM Env
acknowledgeModule modl@(Module _ tdefs vdefgs) = do
  tycons_env <- acknowledgeTypes tdefs
  vdefs_env <- acknowledgeVdefgs vdefgs
  return $ tycons_env ++ vdefs_env

-- | Given a module, recognize type constructors and put them in the heap 
-- so that we can build values for custom types afterwards.
acknowledgeTypes :: [Tdef] -> IM Env
acknowledgeTypes tdefs = mapM acknowledgeType tdefs >>= return . concat
  
-- | Given a data type or a newtype definition, memorize their type constructors,
-- create an environment variable for each of them and return an environment that holds
-- all the created heap references
acknowledgeType :: Tdef -> IM Env
acknowledgeType tdef@(Data qdname@(_,dname) tbinds cdefs) = 
  do
    beVerboseM $ "Acknowledging type " ++ qualifiedVar qdname
    mapM insertTyCon cdefs
  where  
    insertTyCon :: Cdef -> IM HeapReference
    insertTyCon tcon@(Constr qcname tbinds' types) = do
      h <- get       
      let 
        tyConName = qualifiedVar qcname
        tyCon = MkDataCon tyConName types        
      memorize (mkVal $ TyConApp tyCon []) (tyConName)
    
-- | Given a module, recognize all of its value definitions, functions, and put them in the heap so that we can evaluate them when required. 
acknowledgeVdefgs :: [Vdefg] -> IM Env
acknowledgeVdefgs vdefgs = do
  envs <- mapM acknowledgeVdefg vdefgs :: IM [Env]
  return . concat $ envs

-- | Acknowledges a generic value definition
acknowledgeVdefg  :: Vdefg -> IM Env
acknowledgeVdefg (Nonrec vdef) = sequence [acknowledgeVdef vdef]
acknowledgeVdefg (Rec vdefs) = mapM (acknowledgeVdef) vdefs

-- | Acknowledges a generic value definition with some environment. 
acknowledgeVdefgWithin :: Env -> Vdefg -> IM [HeapReference]
acknowledgeVdefgWithin env (Nonrec vdef) = sequence [acknowledgeVdefWithin env vdef]
acknowledgeVdefgWithin env (Rec vdefs) = sequence [acknowledgeVdefWithin env vdef | vdef <- vdefs]

-- | Acknowledges a value definition. 
acknowledgeVdef :: Vdef -> IM HeapReference
acknowledgeVdef (Vdef (qvar, ty, exp)) = do
  beVerboseM $ "Acknowledging definition " ++ qualifiedVar qvar
  memorize (Left $ VdefgThunk exp) (qualifiedVar qvar)

-- | Acknowledges a value definition with some environment. 
acknowledgeVdefWithin :: Env -> Vdef -> IM HeapReference
acknowledgeVdefWithin env (Vdef (qvar, ty, exp)) = do
  beVerboseM $ "Acknowledging definition " ++ qualifiedVar qvar
  memorize (Left $ Thunk exp env) (qualifiedVar qvar)
