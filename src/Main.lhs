
> {-# LANGUAGE ImplicitParams #-}
> {-# LANGUAGE DeriveDataTypeable #-}

This program reads a program in Haskell External Core (EC) syntax and evaluates its main function. The module name is Main

> module Main where

> import Language.Core.Core
> import System.Environment
> import Language.Core.Util(showType)

To move

> import Text.Encoding.Z
> import DART.MkRandomValue
> import Data.Maybe
> import Language.Core.Vdefg
> import DART.FunctionFeeder
> import qualified Language.Core.Interpreter as Interpreter

> import Control.Monad.State.Lazy
> import qualified Data.HashTable.IO as H
> import DART.CmdLine (dodebug,io,printHeap)
> import DART.FileIO

We'll use the package cmdargs to identify flags, parameters, etc.,  from the command line

The interpreter mode reads an external core file and evaluates the declarations of its module. 

> import System.Console.CmdArgs

> data Interpret = InterpreterMode { 
> file :: String
> , eval :: String
> , debug :: Bool
> , show_heap :: Bool
> --, show_time :: Bool
> , show_expressions :: Bool
> , show_subexpressions :: Bool
> , show_tmp_variables :: Bool 
> , watch_reduction :: Bool
> } deriving (Show, Data, Typeable)

> interpret = InterpreterMode {
>   file = def &= typFile &= groupname "USAGE"
>   , eval = def &= typ "FUNCTION_NAME" &= groupname "USAGE" &= help "The function to evaluate (if not provided, all function declarations will be evaluated)"
>   , debug = def &= groupname "DEBUG" &= help "Be verbose about what this program is doing"
>   , show_heap = def &= groupname "DEBUG" &= help "Shows binded values in the heap"
>   , show_expressions = def &= groupname "DEBUG" &= help "Shows the external core expression for every value being evaluated"
>   , show_subexpressions = def &= groupname "DEBUG" &= help "Shows *every* (external core) expression being evaluated"
> --   , show_time = def &= groupname "DEBUG" &= help "Shows the time in which an evaluation was done (if depends flag is on)"
>   , show_tmp_variables = def &= groupname "DEBUG" &= help "Shows debug messages for temporal variables (if depends flag is on)"
>   , watch_reduction = def &= groupname "DEBUG" &= help "Shows debug messages for the evaluation of a value definition (shows reductions of expressions)"
> } &= summary "Reads a .hcr file and evaluates its declarations. "

> main :: IO () 
> main = do

>   -- Command line handling
>   args <- cmdArgs interpret
>   -- Implicit variables
>   let
>     ?debug = debug args
>     ?show_tmp_variables = show_tmp_variables args
>     ?show_heap = show_heap args
>     ?show_expressions = show_expressions args
>     ?show_subexpressions = show_subexpressions args
>     ?watch_reduction = watch_reduction args

>   dodebug $ "Flags: " ++ show args
>   dodebug $ "Reading " ++ file args  ++ " .."
>   module'@(Module mdlname tdefs vdefgs) <- readModule $ file args
>   dodebug $ "Read module " ++ show mdlname

>   -- Time to evaluate
>   dodebug $ "Loading libraries "
>   heap <- H.new  -- get a heap with some predefined functions
>   (_,libs) <- runStateT (Interpreter.loadLibraries) heap
>   case (eval args) of
>     -- What should we eval?
>     "" -> do  -- not specified
>       (_,heap) <- runStateT (Interpreter.evalModule module') libs
>       when (not ?show_heap) $ putStrLn "WARNING: You did not specify a function name to eval (flags --eval or -e), neither the flag --show-heap. That is why this program has no output"
>       when (?show_heap) $ printHeap heap
>     fun_name -> do -- eval fun_name
>       (result,heap) <- runStateT (module' `Interpreter.evalModuleFunction` fun_name) libs
>       when (?show_heap) $ printHeap heap
>       putStrLn $ show result -- will be a Value iff fun_name is not null


Decode any string encoded as Z-encoded string and print it

> putZDecStrLn = putStrLn . zDecodeString

> showVdefg :: Vdefg -> String
> showVdefg (Rec vdefs) = "Rec " ++ concatMap showVdef vdefs
> showVdefg (Nonrec vdef) = "Nonrec " ++ showVdef vdef

> showVdef :: Vdef -> String
> showVdef (Vdef ((mname,var),ty,exp)) = var ++ " :: " ++ showType ty
