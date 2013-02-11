----------------------------------------------------------------------------
-- |
-- Module      :  Language.Core.Interpreter
-- Copyright   :  (c) Carlos López-Camey, University of Freiburg
-- License     :  BSD-3
--
-- Maintainer  :  c.lopez@kmels.net
-- Stability   :  stable
--
--
-- An interpreter for external core expressions
-----------------------------------------------------------------------------

> {-# LANGUAGE FlexibleInstances #-}

> module Language.Core.Interpreter where

> import Language.Core.Core
> import Language.Core.Util(showType,showExtCoreType,showExp,showMname)
> import Language.Core.TypeExtractor(extractType)
> import Language.Core.TypeExtractor.DataTypes

The list of value definitions represents the environment

> evalVdefg :: Vdefg -> [Vdefg] -> String
> evalVdefg (Rec vdefs) vdefgs = "" -- "Recursive eval not yet implemented\n\t" ++ concatMap (\(Vdef ((mname,var),ty,exp)) -> var ++ " :: " ++ showType ty ++ "\n\t"++ showExp exp) vdefs
> evalVdefg (Nonrec (Vdef ((mname,var), ty, exp))) vdefgs = 
>   let 
>      extractedType = extractType ty 
>      environment = vdefgsAsEnvironment vdefgs
>   in case extractedType of
>     Nothing -> "Could not parse type " ++ showExtCoreType ty ++ "; therefore I did not interpret"
>     Just (CType (PType pt)) -> "Will evaluate " ++ var ++ "::" ++ show pt ++ "\n\tExp: " ++ showExp exp ++ "\n\tResult: " ++ showIM (evalExp exp [])
>     Just ty -> "" -- var ++ "; I still don't know how to evaluate values of type " ++ show ty ++ "\n\tExp: " ++ showExp exp ++ "\n\tResult: " ++ showIM (evalExp exp [])

> vdefgsAsEnvironment :: [Vdefg] -> Environment
> vdefgsAsEnvironment [] = []
> vdefgsAsEnvironment ((Rec vdefs):vds) = map vdefAsInEnvironment vdefs ++ vdefgsAsEnvironment vds
> vdefgsAsEnvironment ((Nonrec vdef):vds) = vdefAsInEnvironment vdef : vdefgsAsEnvironment vds

> vdefAsInEnvironment :: Vdef -> (Id,IM Value)
> vdefAsInEnvironment (Vdef (qvar, ty, exp)) = (qualifiedVar qvar, IM . ExtCoreExp $ exp)

AThe interpreter monad

> qualifiedVar :: Qual Var -> String
> qualifiedVar (mname,var) = showMname mname ++ "." ++ var

> data IM a = IM a deriving Show

> instance Monad IM where
>   return a = IM a
>   (IM a) >>= f = f a

> showIM :: IM Value -> String
> showIM = show 

> instance Show (IM Value -> IM Value) where
>          show f = "FUN.."

> instance Show Exp where
>          show = showExp 

> data Value = Wrong String
>            | ExtCoreExp Exp
>            | Num Integer
>            | Fun (IM Value -> IM Value) deriving Show

> type Environment = [(Id,IM Value)]

> evalExp :: Exp -> Environment -> IM Value

> evalExp (App -- Integer sum
>          (Appt
>           (Var ((Just (M (P ("base"),["GHC"],"Num"))),"zp")) -- +
>           _
>          )
>          (Var ((Just (M (P ("base"),["GHC"],"Num"))),"zdfNumInt")) -- $fNumInt
>         ) env = return . Fun $ \arg1 -> return . Fun $ \arg2 -> addValues arg1 arg2

> evalExp (App -- Integer construction
>              (Dcon ((Just (M (P ("ghczmprim"),["GHC"],"Types"))),"Izh"))
>              (Lit lit) 
>         ) env = evalLit lit

> evalExp (App exp1 exp2) env = evalExp exp1 env >>= (\f -> apply f (evalExp exp2 env))

Variables 

> evalExp (Var (mname,var)) env = let var' = showMname mname ++ "." ++ var in lookupVar var' env

Otherwise

> evalExp otherExp env = return . Wrong $ " TODO: " ++ showExp otherExp

> evalLit :: Lit -> IM Value
> evalLit (Literal (Lint i) ty) = if (showExtCoreType ty == "ghczmprim:GHC.Prim.Intzh")
>                                 then return . Num $ i 
>                                 else return . Wrong $ showExtCoreType ty ++ " .. expected " ++ "ghczmprim:GHCziPrim.Intzh"

> lookupVar :: Id -> Environment -> IM Value
> lookupVar x [] = return . Wrong $ "Could not find " ++ x ++ " in the environment"
> lookupVar x ((y,v):env') = if x == y then v else lookupVar x env'

> apply :: Value -> IM Value -> IM Value
> apply (Fun f) v = f v
> apply f m = return . Wrong $ "Applying something that is not a function, namely " ++ show f

Functions on Nums

> --evalExp (Appt (("base",["GHC"],"Num.zp") ty) env =  -- sum

> getInteger :: IM Value -> IM Value
> getInteger mv = do
>   n <- mv -- first arg
>   case n of
>     (Num i) -> mv
>     other_ty -> return . Wrong $ "Expected Integer, found " ++ show other_ty

> addValues :: IM Value -> IM Value -> IM Value
> addValues (IM (Num i)) (IM (Num j)) = IM . Num $ i + j 
> addValues (IM a) (IM b) = return . Wrong $ "Trying to add values " ++ show a ++ " and " ++ show b