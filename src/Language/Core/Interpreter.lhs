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
> import Language.Core.Util(showType,showExtCoreType,showExp)
> import Language.Core.TypeExtractor(extractType)
> import Language.Core.TypeExtractor.DataTypes

> evalVdefg :: Vdefg -> String
> evalVdefg (Rec vdefs) = "Recursive eval not yet implemented\n\t" ++ concatMap (\(Vdef ((mname,var),ty,exp)) -> var ++ " :: " ++ showType ty ++ "\n\t"++ showExp exp) vdefs
> evalVdefg (Nonrec (Vdef ((mname,var), ty, exp))) = 
>   let 
>      extractedType = extractType ty 
>   in case extractedType of
>     Nothing -> "Could not parse type " ++ showExtCoreType ty ++ "; therefore I did not interpret"
>     Just (CType (PType pt)) -> "Will evaluate " ++ var ++ "::" ++ show pt ++ "\n\tExp: " ++ showExp exp ++ "\n\tResult: " ++ showIM (evalExp exp [])
>     Just ty -> var ++ "; I still don't know how to evaluate values of type " ++ show ty ++ "\n\tExp: " ++ showExp exp ++ "\n\tResult: " ++ showIM (evalExp exp [])

AThe interpreter monad

> data IM a = IM a deriving Show

> instance Monad IM where
>   return a = IM a
>   (IM a) >>= f = f a

> showIM :: IM Value -> String
> showIM = show 

> instance Show (IM Value -> IM Value) where
>          show f = "FUN.."

> data Value = Wrong String
>            | Num Integer
>            | Fun (IM Value -> IM Value) deriving Show

> type Environment = [(Id,IM Value)]

> evalExp :: Exp -> Environment -> IM Value

> evalExp (App -- Integer sum
>             (Appt
>                (Var ((Just (M (P ("base"),["GHC"],"Num"))),"zp")) -- +
>                _
>             )
>             (Var ((Just (M (P ("base"),["GHC"],"Num"))),"zdfNumInt")) -- $fNumInt
> --         ) env = return . Fun $ (\Num n -> (\Num m -> Num $ n + m )))
>         ) env = return . Fun $ (
>                              \mv -> case mv of
>                                 (IM (Num i)) -> return . Fun $ \mv' -> case mv' of
>                                                             (IM (Num j)) -> return . Num $ i + j
>                                                             other_mv -> return . Wrong $ "Expected Num(1), found " ++ show other_mv
>                                 other_mv -> return . Wrong $ "Expected Num(2), found " ++ show other_mv
>                                                        )

> evalExp (App -- Integer construction
>              (Dcon ((Just (M (P ("ghczmprim"),["GHC"],"Types"))),"Izh"))
>              (Lit lit) 
>         ) env = evalLit lit

> evalExp (App exp1 exp2) env = evalExp exp1 env >>= (\f -> apply f (evalExp exp2 env))

> evalExp otherExp env = return . Wrong $ " TODO: " ++ showExp otherExp

> evalLit :: Lit -> IM Value
> evalLit (Literal (Lint i) ty) = if (showExtCoreType ty == "ghczmprim:GHC.Prim.Intzh")
>                                 then return . Num $ i 
>                                 else return . Wrong $ showExtCoreType ty ++ " .. expected " ++ "ghczmprim:GHCziPrim.Intzh"


> apply :: Value -> IM Value -> IM Value
> apply (Fun f) v = f v
> apply f m = return . Wrong $ "Applying something that is not a function, namely " ++ show f

Functions on Nums

> --evalExp (Appt (("base",["GHC"],"Num.zp") ty) env =  -- sum

