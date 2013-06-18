module Language.Core.Interpreter.Apply where

import           DART.CmdLine
import qualified Data.HashTable.IO as H
import           Language.Core.Interpreter.Structures

-- | Returns a Pointer HeapAddress if `id` is in `env` or within libraries_env in DARTState, otherwise returns Wrong
mkPointer :: Id -> Env -> IM (Maybe Pointer)
mkPointer id env = gets libraries_env >>= \libs -> mkPtr id (env ++ libs)
  where
    mkPtr id [] = return Nothing
    mkPtr id (e:env) = if (fst e == id) 
                       then let address = snd e in return . Just . MkPointer $ address
                       else mkPtr id env
    
getPointer :: Id -> Env -> IM Value
getPointer id env = mkPointer id env >>= \p -> case p of 
  Just p -> return $ Pointer p
  Nothing -> return $ Wrong $ " getPointer: could not find id " ++ id

lookupId :: Id -> Env -> IM (Either Thunk Value)
lookupId id env = do
  ptr <- getPointer id env
  case ptr of
    Wrong s -> return $ Right $ Wrong s
    Pointer (MkPointer addr) -> lookupMem addr
    _ -> return $ Right $ Wrong "lookupId: The impossible happened"
  
lookupPtr :: Pointer -> IM (Either Thunk Value)
lookupPtr (MkPointer address) = lookupMem address

lookupMem :: HeapAddress -> IM (Either Thunk Value)
lookupMem address = do
  h <- gets heap
  val <- io $ H.lookup h address
  --watchReductionM $ " lookupMem: " ++ show val   
  maybe fail return val 
  where 
    fail :: IM (Either Thunk Value)
    fail = return . Right . Wrong $ "lookupH could not find heap reference " ++ show address
