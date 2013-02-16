
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

> main :: IO () 
> main = do
>   args <- getArgs
>   case args of
>     [f] -> do
>       putStrLn $ "Reading " ++ f ++ " .."
>       contents <- readFile f
>       let 
>         module'@(Module mdlname tdefs vdefgs)  = case parse contents 0 of
>           OkP modl -> modl
>           FailP msg -> error msg
>       putStrLn $ "Parsed module name:\n\t" ++ (show mdlname)
>       --putStrLn $ "Parsed tdefs:" 
>       --mapM (\p -> putZDecStrLn $ "\t " ++ show p) tdefs
>       putStrLn $ "Heap\n----------------------------------------\n" 
>       new_heap <- H.new
>       heap <- execStateT (evalModule module') new_heap
>       H.mapM_ (\(id,val) -> putStrLn $ id ++ "... \t " ++ show val ++ "\n") heap
>     _ -> putStrLn "Wrong usage"

Decode any string encoded as Z-encoded string and print it

> putZDecStrLn = putStrLn . zDecodeString

> showVdefg :: Vdefg -> String
> showVdefg (Rec vdefs) = "Rec " ++ concatMap showVdef vdefs
> showVdefg (Nonrec vdef) = "Nonrec " ++ showVdef vdef

> showVdef :: Vdef -> String
> showVdef (Vdef ((mname,var),ty,exp)) = var ++ " :: " ++ showType ty