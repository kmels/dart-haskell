> {-# LANGUAGE ImplicitParams #-}

> module DART.CmdLine where

> import qualified Data.HashTable.IO as H
> import Language.Core.Interpreter.Structures
> import Control.Monad.IO.Class
> import DART.InterpreterSettings
> import Control.Monad(when)
> import Language.Core.Util(showExp)
> import Language.Core.Core -- Exp

> io :: MonadIO m => IO a -> m a
> io = liftIO 

> dodebug :: (?settings :: InterpreterSettings) => String -> IO ()
> dodebug msg = if (debug ?settings) then putStrLn (tab msg ++ msg)  else return () where
>   tab _ = replicate (debug_tab_level ?settings) '\t'

> debugM :: (?settings :: InterpreterSettings) => String -> IM ()
> debugM = io . dodebug

> dowatch msg = if (?watch_reduction) then putStrLn msg  else return ()

> dodebugNoLine :: (?debug :: Bool) => String -> IO ()
> dodebugNoLine msg = if (?debug) then putStr msg else return ()

> printHeap :: (?settings :: InterpreterSettings) => Heap -> IO ()
> printHeap h = do
>   putStrLn $ "-------------------- Heap begins --------------------" 
>   H.mapM_ printVar h
>   putStrLn $ "-------------------- Heap ends --------------------" 
>   where
>     showVal :: Either Thunk Value -> String
>     showVal (Left t) = "Thunk"
>     showVal (Right v) = show v
>
>     printVar (id,val) = if (show_tmp_variables ?settings) 
>                         then putStrLn $ id ++ " => " ++ showVal val
>                         else if (':' `elem` id) -- then id has a package
>                              then putStrLn $ id ++ " => " ++ showVal val
>                              else return ()

Inspired by Prelude.when

> whenFlag              :: (MonadIO m) => Bool -> IO () -> m ()
> whenFlag p s          =  if p then io s else return ()

> debugSubexpression :: (?settings :: InterpreterSettings) => Exp -> IM ()
> debugSubexpression e = when (show_subexpressions ?settings) $ 
>                        io . dodebug $ "Sub-expression: " ++ showExp e

If we are in the IM Monad, we might want to watch expressions being reduced as they are interpreted. 

If the flag --watch-reduction was specified, prints a debug message.

> watchReduction :: (?settings :: InterpreterSettings) => String -> IM ()
> watchReduction msg | watch_reduction ?settings = debugM msg
>                    | otherwise = return ()
