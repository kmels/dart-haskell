This program reads a program in Haskell External Core syntax and evaluates its main function. The module name is Main

> module Main where

We'll need some data structures from GHC

> import HsSyn

and the provided lexer and parser for External Core

> import LexCore (lexer)
> import ParserCore (parseCore)
> import ParserCoreUtils

> import Module ( Module (.. ), ModuleName (..), moduleNameString )

and functions to interact with the user

> import System.Environment

Let's parse the input file and convert it to External Core

> import Util

> main :: IO () 
> main = do
>   args <- getArgs
>   case args of
>     [f] -> do
>       putStrLn $ "Reading " ++ f ++ " .."
>       contents <- readFile f
>       let
>         token = case lexer returnP contents 0 of
>           OkP a -> a
>           FailP msg -> error msg
>         (module',typeDeclarations,ifaceBindings) = case parseCore contents 0 of
>           OkP (HsExtCore m t i) -> (m,t,i)
>           FailP msg -> error msg
>       putStrLn $ "Parsed module name " ++ (moduleNameString . moduleName) module'
>     _ -> putStrLn "Wrong usage"

