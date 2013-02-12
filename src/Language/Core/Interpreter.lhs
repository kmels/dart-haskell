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

> module Language.Core.Interpreter where

> import Language.Core.Interpreter.Structures
> import Language.Core.Core
> import Language.Core.ValueDefinition(vdefgName)
> import Language.Core.Util(qualifiedVar,showVdefg,showType,showExtCoreType,showExp,showMname)
> import Language.Core.TypeExtractor(extractType)
> import Language.Core.TypeExtractor.DataTypes

> import qualified Data.HashTable.IO as H

> import Control.Monad.State.Lazy

Given a module which contains a list of value definitions, *vd*, evaluate every *vd* and return a heap with their interpreted values.

Value definition to mapped values
-----------------------------------------------------
| Value definition type              | mapped to    |
| Concrete type e.g. Int             | IM (Num val) |
| Concrete type e.g. Int -> Int      | ?            |
-----------------------------------------------------

> evalModule :: Module -> IM Heap
> evalModule m@(Module name _ vdefgs) = do
>   eval_head <- evalVdefg (head vdefgs)
>   liftIO $ putStrLn $ "Result of head: " ++ show eval_head
>   mapM_ (\vdefg -> do 
>             h <- get
>             res <- evalVdefg vdefg
>             let id = vdefgName vdefg
>             liftIO $ putStrLn $ "Doing  .. " ++ (vdefgName vdefg)
>             liftIO $ H.insert h id res
>             liftIO $ putStrLn $ "Result: " ++ show res
>         ) vdefgs
>     {- vdefg <- lift vdefgs -- :: ListT (IO ..)
>     
>     env <- lift H.toList heap
>     let 
>       id = vdefgName vdefg
>       result = (evalVdefg vdefg env)
>     liftIO $ putStrLn $ "Result  .. " ++ showIM result
>     liftIO $ H.insert heap id result
>     return (id,result)-}
>   h <- get
>   return h

where 
     compute :: Vdefg -> IO (Id, IM Value)
     compute vdefg = do 
       env <- H.toList heap
       return $ evalVdefg vdefg

-- sequence :: Monad m => [m a] -> m [a]
a = (k,v)
m = IO
[m a] = IO (k,v) <- results

The list of value definitions represents the environment

> evalVdefg :: Vdefg -> IM Value
> evalVdefg (Rec vdefs) = return . Wrong $ "TODO: Recursive vdefg" -- "Recursive eval not yet implemented\n\t" ++ concatMap (\(Vdef ((mname,var),ty,exp)) -> var ++ " :: " ++ showType ty ++ "\n\t"++ showExp exp) vdefs
> evalVdefg (Nonrec (Vdef (qvar, ty, exp))) = 
>   let 
>      extractedType = extractType ty 
>   in case extractedType of
>     Nothing -> return . Wrong $ "Could not parse type " ++ showExtCoreType ty ++ "; therefore I did not interpret"
>     Just (CType (PType pt)) -> do
>       res <- evalExp exp -- result
>       heap <- get 
>       liftIO $ H.insert heap (qualifiedVar qvar) res
>       return $ res
>     Just ty -> return . Wrong $ "I still don't know how to evaluate values of type " -- ++ show ty ++ "\n\tExp: " ++ showExp exp ++ "\n\tResult: " ++ showIM (evalExp exp [])


> evalExp :: Exp -> IM Value

This one is a function application which has the type accompanied. We won't care about the type now, as I'm not sure how it can be used now.

Appt is always (?) applied to App together with a var that represents the function call of the Appt. In the case of integer summation, this is base:GHC.Num.f#NumInt. That is why we have to ignore the first parameter when applied.

> evalExp (Appt function_exp _) = return $ Fun (\_ -> evalExp function_exp)

This is the sum function

> evalExp (Var ((Just (M (P ("base"),["GHC"],"Num"))),"zp")) = return $ Fun (\arg1 -> return . Fun $ \arg2 -> addValues arg1 arg2)
> evalExp (Var ((Just (M (P ("base"),["GHC"],"Num"))),"zt")) = return $ Fun (\arg1 -> return . Fun $ \arg2 -> multiplyValues arg1 arg2)

> evalExp (App -- Integer construction
>              (Dcon ((Just (M (P ("ghczmprim"),["GHC"],"Types"))),"Izh"))
>              (Lit lit) 
>         ) = evalLit lit

> evalExp (App function_exp argument_exp) = do 
>   f <- evalExp function_exp 
>   x <- evalExp argument_exp
>   apply f x

Variables 

> evalExp (Var (mname,var)) = let var' = showMname mname ++ "." ++ var in lookupVar var'

Otherwise

> evalExp otherExp = return . Wrong $ " TODO: " ++ showExp otherExp

> evalLit :: Lit -> IM Value
> evalLit (Literal (Lint i) ty) = if (showExtCoreType ty == "ghczmprim:GHC.Prim.Intzh")
>                                 then return . Num $ i 
>                                 else return . Wrong $ showExtCoreType ty ++ " .. expected " ++ "ghczmprim:GHCziPrim.Intzh"

> lookupVar :: Id -> IM Value
> lookupVar x = do
>   env <- get
>   maybeV <- liftIO $ H.lookup env x
>   case maybeV of
>     Nothing -> return . Wrong $ "Could not find " ++ x ++ " in the environment"
>     Just v -> return v

> apply :: Value -> Value -> IM Value
> apply (Fun f) v = f $ v
> apply w@(Wrong _) _ = return w
> apply f m = return . Wrong $ "Applying something that is not a function, namely " ++ show f

Functions on Nums

> --evalExp (Appt (("base",["GHC"],"Num.zp") ty) env =  -- sum

> getInteger :: IM Value -> IM Value
> getInteger mv = do
>   n <- mv -- first arg
>   case n of
>     (Num i) -> mv
>     other_ty -> return . Wrong $ "Expected Integer, found " ++ show other_ty

> addValues :: Value -> Value -> IM Value
> addValues (Num i) (Num j) = return . Num $ i + j 
> addValues a b = return . Wrong $ "Trying to add values " ++ show a ++ " and " ++ show b

> multiplyValues :: Value -> Value -> IM Value
> multiplyValues (Num i) (Num j) = return . Num $ i * j 
> multiplyValues a b = return . Wrong $ "Trying to multiply values " ++ show a ++ " and " ++ show b