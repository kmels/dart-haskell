This program reads a program in Haskell External Core (EC) syntax and evaluates its main function. The module name is Main

> module Main where

We'll need to parse EC input. For that we use the extcore package.

> import Language.Core.Parser
> import Language.Core.ParseGlue
> import Language.Core.Core

and functions to interact with the user

> import System.Environment

> main :: IO () 
> main = do
>   args <- getArgs
>   case args of
>     [f] -> do
>       putStrLn $ "Reading " ++ f ++ " .."
>       contents <- readFile f
>       let 
>         module'@(Module name tdefs vdefgs)  = case parse contents 0 of
>           OkP modl -> modl
>           FailP msg -> error msg
>       putStrLn $ "Parsed module name " ++ (show name)
>     _ -> putStrLn "Wrong usage"

