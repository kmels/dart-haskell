module Language.Core.Interpreter.Apply where

import           DART.CmdLine
import qualified Data.HashTable.IO as H
import           Language.Core.Interpreter.Structures

-- | Returns a Pointer HeapAddress if `id` is in `env`, otherwise returns Wrong
mkPointer :: Id -> Env -> IM Value
mkPointer id [] = return . Wrong $ " mkPointer: could not find id " ++ id
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
