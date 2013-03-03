
> {-# LANGUAGE ImplicitParams #-}


This program reads a program in Haskell External Core (EC) syntax and evaluates its main function. The module name is Main

> module Main where

> import Language.Core.Core
> import System.Environment
> import Language.Core.Util(showType)
> import Language.Core.Interpreter.Structures

To move

> import Text.Encoding.Z
> import DART.MkRandomValue
> import Data.Maybe
> import Language.Core.Vdefg
> import DART.FunctionFeeder
> import qualified Language.Core.Interpreter as Interpreter
> import Language.Core.Interp
> import Control.Monad.State.Lazy
> import qualified Data.HashTable.IO as H
> import DART.CmdLine
> import DART.FileIO
> import DART.InterpreterSettings
> import System.Console.CmdArgs
> main :: IO () 
> main = do

>   -- Command line handling
>   args <- cmdArgs interpret
>   -- Implicit variables
>   let
>     ?settings = args

>   dodebug $ "Flags: " ++ show args
>   dodebug $ "Reading " ++ file args  ++ " .."
>   module'@(Module mdlname tdefs vdefgs) <- readModule $ file args
>   dodebug $ "Read module " ++ show mdlname

> --  putStrLn "Result from Language.Core.Interp: "
> --  interpResp <- evalProgram [module']
>  -- putStrLn . show $ interpResp


>   -- Time to evaluate
>   dodebug $ "Loading libraries "
>   s <- newState ?settings
>   (_,libs) <- runStateT (Interpreter.loadLibraries) s
>   case (eval args) of
>     -- What should we eval?
>     "" -> do  -- not specified
>       (_,state) <- runStateT (Interpreter.evalModule module') libs
>       let h = heap state
>       when (not . show_heap $ ?settings) $ putStrLn "WARNING: You did not specify a function name to eval (flags --eval or -e), neither the flag --show-heap. That is why this program has no output"
>       when (show_heap $ ?settings) $ printHeap h
>     fun_name -> do -- eval fun_name
>       (result,state) <- runStateT (module' `Interpreter.evalModuleFunction` fun_name) libs
>       when (show_heap $ ?settings) $ printHeap (heap state)
>       putStrLn $ show result -- will be a Value iff fun_name is not null

Decode any string encoded as Z-encoded string and print it

> putZDecStrLn = putStrLn . zDecodeString

> showVdefg :: Vdefg -> String
> showVdefg (Rec vdefs) = "Rec " ++ concatMap showVdef vdefs
> showVdefg (Nonrec vdef) = "Nonrec " ++ showVdef vdef

> showVdef :: Vdef -> String
> showVdef (Vdef ((mname,var),ty,exp)) = var ++ " :: " ++ showType ty

-- | Creates an initial state

> newState :: InterpreterSettings -> IO DARTState
> newState s = do
>   h <- H.new -- get a new heap
>   return $ DState {
>     heap = h
>     , number_of_reductions = 0
>     , settings = s
>   }
