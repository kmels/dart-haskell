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
import Language.Core.Vdefg(vdefId,vdefgNames,vdefQualId)

-- | Given a module, recognize type constructors and value definitions
-- and save them in the heap
acknowledgeModule :: Module -> IM Env
acknowledgeModule modl@(Module mname tdefs vdefgs) = do
  tycons_env <- acknowledgeTypes tdefs -- type constructors
  vdefs_env <- acknowledgeVdefgs vdefgs -- value definitions
  io . putStrLn $ "Types of: " ++ show mname
  mapM (io . putStrLn) $ map fst tycons_env
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
    let type_name = qualifiedVar qdname    
        type_constructors = map mkDataCon cdefs
        
    beVerboseM $ "Acknowledging type " ++ type_name
    
    tyCon_refs <- mapM insertTyCon type_constructors 
    
    -- the sum type itself
    sumtype_ref <- mkDataTypeRef type_constructors type_name
    
    -- make overall env
    return (sumtype_ref:tyCon_refs)
  where
    mkDataTypeRef :: [DataCon] -> Id -> IM HeapReference
    mkDataTypeRef cons tname = memorize (mkVal . SumType $ cons) tname
    
    mkDataCon :: Cdef -> DataCon
    mkDataCon tcon@(Constr qcname tbinds' types) = MkDataCon (qualifiedVar qcname) types
      
    insertTyCon :: DataCon -> IM HeapReference
    insertTyCon tyCon@(MkDataCon tyConName tys) = memorize (mkVal $ TyConApp tyCon []) (tyConName)
    
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
  let mkList x = [x]
  in newAddress >>= storeVdef vdef env >>= return . mkList
   
  --beVerboseM $ "Acknowledging non-recursive definition: " ++ vdefQualId vdef
  --sequence [(flip acknowledgeVdef env) vdef]
acknowledgeVdefg v@(Rec vdefs) env = do
  beVerboseM $ "Acknowledging recursive definitions: " ++ (show . vdefgNames $ v)
  beVerboseM $ "with env: " ++ show (map fst env)
  
  addresses <- allocate $ length vdefs
  let ids = map vdefId vdefs
  let env' = env ++ zip ids addresses
  let vdefsWithAddress = zip vdefs addresses
  
  beVerboseM $ "Made extended environment: " ++ show (map fst env')  
  mapM (\(vdef,address) -> storeVdef vdef env' address) vdefsWithAddress
    

-- | Stores a value definition in the given address. 
storeVdef :: Vdef -> Env -> HeapAddress -> IM HeapReference
storeVdef (Vdef (qvar, ty, exp)) env address= do
  beVerboseM $ "Acknowledging value definition " ++ qualifiedVar qvar
  beVerboseM $ "\twith env = " ++ show (map fst env)
  beVerboseM $ "\tin address = " ++ show address
  store address (Left $ Thunk exp env) (qualifiedVar qvar)
