module Language.Core.Interpreter.Apply where

import           DART.CmdLine
import qualified Data.HashTable.IO as H
import           Language.Core.Interpreter.Structures

-- | Returns a Pointer HeapAddress if `id` is in `env`, otherwise returns Wrong
mkPointer :: Id -> Env -> IM (Maybe Pointer)
mkPointer id [] = return Nothing
mkPointer id (e:env) = if (fst e == id) 
                       then let address = snd e in return . Just . MkPointer $ address
                       else mkPointer id env
                       

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
  
