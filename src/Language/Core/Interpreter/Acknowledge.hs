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
module Language.Core.Interpreter.Acknowledge(acknowledgeModule,acknowledgeTypes,acknowledgeVdefgs,acknowledgeVdefg) where

import DART.CmdLine(beVerboseM)
import DART.InterpreterSettings
import Language.Core.Core
import Language.Core.Interpreter.Structures
import Language.Core.Util
import Language.Core.Vdefg(vdefgNames,vdefQualId)

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
acknowledgeVdefgs vdefgs = acknowledgeVdefgs' vdefgs []
  where
    acknowledgeVdefgs' :: [Vdefg] -> Env -> IM Env
    acknowledgeVdefgs' [vdefg] env = acknowledgeVdefg vdefg env >>= return . flip (++) env
    acknowledgeVdefgs' (v:vs) env = acknowledgeVdefg v env >>= \e -> acknowledgeVdefgs' vs (e ++ env)      

-- | Acknowledges a generic value definition
acknowledgeVdefg  :: Vdefg -> Env -> IM Env
acknowledgeVdefg (Nonrec vdef) env = 
  let
    mkList :: a -> [a]
    mkList x = [x]
  in
   acknowledgeVdef vdef env >>= return . mkList
  --beVerboseM $ "Acknowledging non-recursive definition: " ++ vdefQualId vdef
  --sequence [(flip acknowledgeVdef env) vdef]
acknowledgeVdefg v@(Rec vdefs) env = do
  beVerboseM $ "Acknowledging recursive definitions: " ++ (show . vdefgNames $ v)
  mapM (flip acknowledgeVdef env) vdefs

-- | Acknowledges a value definition. 
acknowledgeVdef :: Vdef -> Env -> IM HeapReference
acknowledgeVdef (Vdef (qvar, ty, exp)) env = do
  beVerboseM $ "Acknowledging value definition " ++ qualifiedVar qvar
  memorize (Left $ Thunk exp env) (qualifiedVar qvar)
