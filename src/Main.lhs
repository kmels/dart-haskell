This program reads a program in Haskell External Core (EC) syntax and evaluates its main function. The module name is Main

> module Main where

We'll need to parse EC input. For that we use the extcore package.

> import Language.Core.Parser
> import Language.Core.ParseGlue
> import Language.Core.Core
> import Language.Core.Core

> import System.Environment

> import Language.Core.Util(showExtCoreType)

To move

> import Text.Encoding.Z
> import DART.MkRandomValue
> import Data.Maybe
> import Language.Core.ValueDefinition
> import DART.FunctionFeeder
> import Language.Core.Interpreter

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
>       putStrLn $ "Parsed tdefs:" 
>       mapM (\p -> putZDecStrLn $ "\t " ++ show p) tdefs
>       putStrLn $ "Value definitions1:\n----------------------------------------\n" 
>       mapM_ (\p -> putZDecStrLn $ "... \t " ++ showVdefg p ++ "\n") vdefgs
>       --putStrLn $ "Functions founded:\n----------------------------------------\n" 
>       --mapM_ (\p -> putZDecStrLn $ "... \t " ++ show p ++ "\n") $ (mapMaybe vdefgToMaybeTapp vdefgs)
>       putStrLn $ "Interpreted functions (with no arguments):\n----------------------------------------\n" 
>       mapM_ (\p -> putStrLn $ "... \t " ++ show p ++ "\n") $ map evalVdefg vdefgs
>     _ -> putStrLn "Wrong usage"

Decode any string encoded as Z-encoded string and print it

> putZDecStrLn = putStrLn . zDecodeString

> showVdefg :: Vdefg -> String
> showVdefg (Rec vdefs) = "Rec " ++ concatMap showVdef vdefs
> showVdefg (Nonrec vdef) = "Nonrec " ++ showVdef vdef

> showVdef :: Vdef -> String
> showVdef (Vdef ((mname,var),ty,exp)) = var ++ " :: " ++ showExtCoreType ty