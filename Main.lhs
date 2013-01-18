This program reads a program in Haskell External Core syntax and evaluates its main function.

> module Main where

> import System.Environment

Let's parse the input file and convert it to External Core

> main :: IO () 
> main = do
>   args <- getArgs
>   case args of
>     [f] -> do
>       putStrLn $ "Reading " ++ f ++ " .."
>       contents <- readFile f
>     _ -> putStrLn "Wrong usage"

