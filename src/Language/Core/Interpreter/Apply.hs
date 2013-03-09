module Language.Core.Interpreter.Apply where

import           DART.CmdLine
import qualified Data.HashTable.IO as H
import           Language.Core.Interpreter.Structures

-- | Returns a Pointer HeapAddress if `id` is in `env`, otherwise returns Wrong
mkPointer :: Id -> Env -> IM Value
mkPointer id [] = return . Wrong $ " could not find id " ++ id
mkPointer id (e:env) = if (fst e == id) 
                       then let address = snd e in return $ Pointer address
                       else mkPointer id env

lookupId :: Id -> Env -> IM (Either Thunk Value)
lookupId id env = do
  ptr <- mkPointer id env
  case ptr of
    Wrong s -> return $ Right $ Wrong s
    Pointer addr -> lookupMem addr
    _ -> return $ Right $ Wrong "lookupId: The impossible happened"
  
                               
lookupMem :: HeapAddress -> IM (Either Thunk Value)
lookupMem address = do
  h <- gets heap
  val <- io $ H.lookup h address 
  maybe fail return val 
  where 
    fail :: IM (Either Thunk Value)
    fail = return . Right . Wrong $ "lookupH could not find heap reference " ++ show address





-- backup
-- apply :: Value -> Id -> Env -> IM Value
-- apply (Fun f _) id env = do lookupHeapAdr address >>= either f x
--   Applies a function f that expects a value that is the result of evaluating some thunk
--   ptr <- mkPointer id env :: Pointer HeapAddress
--   case ptr of 
--     Pointer address -> f address
--     Wrong s -> return $ Wrong s
--     _ -> return $ Wrong "apply#Fun: The impossible happened"
    
-- apply (TyConApp (AlgTyCon name (ty:tys)) vals) id env = 
--   Applies a (possibly applied) type constructor that expects appliedValue of type ty.
--   The type constructor that we are applying has |vals| applied values
--   Returns a new type constructor that will take |tys| more values
--   do 
--     val <- mkPointer id env
--     return $ TyConApp newTyCon (mkAppValues val)
--   where
--     newTyCon :: TyCon
--     newTyCon = AlgTyCon name tys expects one value less
--     mkAppValues :: Value -> [Either Thunk Value]
--     mkAppValues v = vals ++ [Right v] :: records the (just) applied value *as a pointer*
  
-- apply w@(Wrong _) _ _ = return w
-- apply f m env = return . Wrong $ "Applying " ++ show f ++ " with argument " ++ show m
