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
>       putStrLn $ "Value definitions1:\n----------------------------------------\n" 
>       mapM_ (\p -> putZDecStrLn $ "... \t " ++ showVdefg p ++ "\n") vdefgs
>       putStrLn $ "Functions founded:\n----------------------------------------\n" 
>       --mapM_ (\p -> putZDecStrLn $ "... \t " ++ showFApp p ++ "\n") $ (mapMaybe vdefgTapp vdefgs)
>       putStrLn $ "Feeded functions:\n----------------------------------------\n" 
>       mapM_ (\p -> putZDecStrLn $ "... \t " ++ show p ++ "\n") $ map feedFunction $ mapMaybe vdefgToMaybeTapp vdefgs
>     _ -> putStrLn "Wrong usage"

helper show function that says which constructor is used

> {-showFApp :: FunctionApplication -> String
> showFApp (FunApp (Tvar t1) t2 exp) = "\t\t function FROM (Tvar) " ++ (show t1) ++ " TO " ++ show t2 ++ "\n\t\t..exp: " 
> showFApp (FunApp (Tcon t1) t2 exp) = "\t\t function FROM (Tcon) " ++ show t1 ++ " TO " ++ show t2 ++ "\n\t\t..exp: " 
> showFApp (FunApp (Tapp t1 t1') t2 exp) = "\t\t function FROM (Tapp) " ++ show t1 ++ " .. " ++ show t1' ++ " TO " ++ show t2 ++ "\n\t\t..exp: " 
> showFApp (FunApp (Tforall t1@(tvar,kind) t1') t2 exp) = "\t\t function FROM (Tforall) " ++ show t1 ++ " TO " ++ show t2 ++ "\n\t\t..exp: " 

> showFApp (FunApp t1 t2 exp) = "\t\t function (Any) FROM " ++ show t1 ++ " TO " ++ show t2 ++ "\n\t\t..exp: " -}

Decode any string encoded as Z-encoded string and print it

> putZDecStrLn = putStrLn . zDecodeString

> showVdefg :: Vdefg -> String
> showVdefg (Rec vdefs) = concatMap (\p -> "Rec -> " ++ show p) vdefs
> --showVdefg (Nonrec (Vdef ((mname,var),ty,exp) )) = "Nonrec\n\t\t..qual_mname: " ++ show mname ++ "\n\t\t..var: " ++ show var ++ "\n\t\t..ty: " ++ show ty ++ "\n\t\t..exp: " 
> showVdefg (Nonrec (Vdef ((mname,var),ty,exp) )) = showExtCoreType ty