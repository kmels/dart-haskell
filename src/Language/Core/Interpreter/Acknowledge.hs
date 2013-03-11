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
module Language.Core.Interpreter.Acknowledge(acknowledgeTypes,acknowledgeVdefgs) where

import Language.Core.Interpreter.Structures
import Language.Core.Util
import Language.Core.Core
import DART.CmdLine
import DART.InterpreterSettings

-- | Given a module, recognize type constructors and put them in the heap 
-- so that we can build values for custom types afterwards.      
acknowledgeTypes :: (?settings :: InterpreterSettings) => Module -> IM Env
acknowledgeTypes modl@(Module _ tdefs _) = mapM acknowledgeType tdefs >>= return . concat
  
-- | Given a data type or a newtype definition, memorize their type constructors,
-- create an environment variable for each of them and return an environment that holds
-- all the created heap references
acknowledgeType :: (?settings :: InterpreterSettings) => Tdef -> IM Env
acknowledgeType tdef@(Data qdname@(_,dname) tbinds cdefs) = 
  do
    debugM $ "Acknowledging type " ++ qualifiedVar qdname
    mapM insertTyCon cdefs
  where  
    insertTyCon :: Cdef -> IM HeapReference
    insertTyCon tcon@(Constr qcname tbinds' types) = do
      h <- get       
      let 
        tyConName = qualifiedVar qcname
        tyCon = AlgTyCon tyConName types        
      memorize (mkVal $ TyConApp tyCon []) (tyConName)
    
-- | Given a module, recognize all of its value definitions, functions, and put them in the heap so that we can evaluate them when required. 
acknowledgeVdefgs :: (?settings :: InterpreterSettings) => Module -> IM Env
acknowledgeVdefgs m@(Module _ _ vdefgs) = mapM acknowledgeVdefg vdefgs >>= return . concat

-- | Acknowledges value definitions
acknowledgeVdefg  :: Vdefg -> IM Env
acknowledgeVdefg (Nonrec vdef) = sequence [acknowledgeVdef vdef]
acknowledgeVdefg (Rec vdefs) = mapM acknowledgeVdef vdefs

acknowledgeVdef :: Vdef -> IM HeapReference
acknowledgeVdef (Vdef (qvar, ty, exp)) = memorize (mkThunk exp) (qualifiedVar qvar)
