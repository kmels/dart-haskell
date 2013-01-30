This program reads a program in Haskell External Core (EC) syntax and evaluates its main function. The module name is Main

> module Main where

We'll need to parse EC input. For that we use the extcore package.

> import Language.Core.Parser
> import Language.Core.ParseGlue
> import Language.Core.Core
> import Language.Core.Core

and functions to interact with the user

> import System.Environment

To move

> import Text.Encoding.Z
> import DART.MkRandomValue

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
>       rint <- rndInt 
>       putStrLn . show $ rint
>       putStrLn $ "Parsed vdefgs:" 
>       mapM_ (\p -> putZDecStrLn $ "... \t " ++ showVdef p ++ "\n") vdefgs
>     _ -> putStrLn "Wrong usage"

helper show function that says which constructor is used

> showVdef :: Vdefg -> String
> showVdef (Rec vdefs) = concatMap (\p -> "Rec -> " ++ show p) vdefs
> showVdef (Nonrec (Vdef ((mname,var),ty,exp) )) = "Nonrec\n\t\t..qual_mname: " ++ show mname ++ "\n\t\t..var: " ++ show var ++ "\n\t\t..ty: " ++ show ty ++ "\n\t\t..exp: " ++ show exp
> --showVdef (Nonrec vdef) = "Nonrec " ++ show vdef

Decode any string encoded as Z-encoded string and print it

> putZDecStrLn = putStrLn . zDecodeString
