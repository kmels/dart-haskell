> {-# LANGUAGE ImplicitParams #-}

> module DART.CmdLine where

> import qualified Data.HashTable.IO as H
> import Language.Core.Interpreter.Structures
> import Control.Monad.IO.Class

> io :: MonadIO m => IO a -> m a
> io = liftIO 

> dodebug :: (?debug :: Bool) => String -> IO ()
> dodebug msg = if (?debug) then putStrLn msg  else return ()

> dowatch msg = if (?watch_reduction) then putStrLn msg  else return ()

> dodebugNoLine :: (?debug :: Bool) => String -> IO ()
> dodebugNoLine msg = if (?debug) then putStr msg else return ()

> printHeap :: (?show_tmp_variables :: Bool) => Heap -> IO ()
> printHeap h = do
>   putStrLn $ "-------------------- Heap begins --------------------" 
>   H.mapM_ printVar h
>   putStrLn $ "-------------------- Heap ends --------------------" 
>   where
>     showVal :: Either Thunk Value -> String
>     showVal (Left t) = "Thunk"
>     showVal (Right v) = show v
>
>     printVar (id,val) = if (?show_tmp_variables) 
>                         then putStrLn $ id ++ " => " ++ showVal val
>                         else if (':' `elem` id) -- then id has a package
>                              then putStrLn $ id ++ " => " ++ showVal val
>                              else return ()

Inspired by Prelude.when

> whenFlag              :: (MonadIO m) => Bool -> IO () -> m ()
> whenFlag p s          =  if p then io s else return ()

