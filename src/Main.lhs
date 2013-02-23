
> {-# LANGUAGE ImplicitParams #-}
> {-# LANGUAGE DeriveDataTypeable #-}

This program reads a program in Haskell External Core (EC) syntax and evaluates its main function. The module name is Main

> module Main where

We'll need to parse EC input. For that we use the extcore package.

> import Language.Core.Parser
> import Language.Core.ParseGlue
> import Language.Core.Core
> import Language.Core.Core

> import System.Environment

> import Language.Core.Util(showType)

To move

> import Text.Encoding.Z
> import DART.MkRandomValue
> import Data.Maybe
> import Language.Core.ValueDefinition
> import DART.FunctionFeeder
> import Language.Core.Interpreter

> import Control.Monad.State.Lazy
> import qualified Data.HashTable.IO as H
> import DART.CmdLine (dodebug,io,printHeap)
> import DART.FileIO (readHcrFile)

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
> , show_tmp_variables :: Bool 
> } deriving (Show, Data, Typeable)

> interpret = InterpreterMode {
>   file = def &= typFile &= groupname "USAGE"
>   , eval = def &= typ "FUNCTION_NAME" &= groupname "USAGE" &= help "The function to evaluate (if not provided, all function declarations will be evaluated)"
>   , debug = def &= groupname "DEBUG" &= help "Be verbose about what this program is doing"
>   , show_heap = def &= groupname "DEBUG" &= help "Shows binded values in the heap"
>   , show_expressions = def &= groupname "DEBUG" &= help "Shows external core expressions being evaluated (if debug flag is on)"
> --   , show_time = def &= groupname "DEBUG" &= help "Shows the time in which an evaluation was done (if depends flag is on)"
>   , show_tmp_variables = def &= groupname "DEBUG" &= help "Shows debug messages for temporal variables (if depends flag is on)"
> } &= summary "Reads a .hcr file and evaluates its declarations. "

Every .hcr file corresponds to a haskell module

> readModule :: FilePath -> IO Module
> readModule fp = readHcrFile fp >>= \c -> case parse c 0 of
>   OkP m -> return m
>   FailP msg -> error msg

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

>   dodebug $ "Flags: " ++ show args
>   dodebug $ "Reading " ++ file args  ++ " .."
>   module'@(Module mdlname tdefs vdefgs) <- readModule $ file args
>   dodebug $ "Read module " ++ show mdlname

>   -- Time to evaluate
>   fresh_heap <- H.new
>   case (eval args) of
>     -- If all functions were evaluated
>     "" -> do
>       (_,heap) <- runStateT (evalModule module') fresh_heap
>       when (not ?show_heap) $ putStrLn "WARNING: You did not specify a function name to eval (flags --eval or -e), neither the flag --show-heap. That is why this program has no output"
>       when (?show_heap) $ printHeap heap
>     -- If a function was evaluated
>     fun_name -> do
>       (result,heap) <- runStateT (module' `evalModuleFunction` fun_name) fresh_heap
>       when (?show_heap) $ printHeap heap
>       putStrLn $ show result -- will be a Value iff fun_name is not null


Decode any string encoded as Z-encoded string and print it

> putZDecStrLn = putStrLn . zDecodeString

> showVdefg :: Vdefg -> String
> showVdefg (Rec vdefs) = "Rec " ++ concatMap showVdef vdefs
> showVdefg (Nonrec vdef) = "Nonrec " ++ showVdef vdef

> showVdef :: Vdef -> String
> showVdef (Vdef ((mname,var),ty,exp)) = var ++ " :: " ++ showType ty