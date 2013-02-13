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
> import Language.Core.Util(qualifiedVar,showVdefg,showType,showExtCoreType,showExp,showMname,bindId,showBind)
> import Language.Core.TypeExtractor(extractType)
> import Language.Core.TypeExtractor.DataTypes

> import qualified Data.HashTable.IO as H

> import Control.Monad.State.Lazy

> import Data.Time.Clock(getCurrentTime,diffUTCTime)

Given a module which contains a list of value definitions, *vd*, evaluate every *vd* and return a heap with their interpreted values.

Value definition to mapped values
-----------------------------------------------------
| Value definition type              | mapped to    |
| Concrete type e.g. Int             | Num val      |
| Concrete type e.g. Int -> Int      | ?            |
-----------------------------------------------------

> evalModule :: Module -> IM Heap
> evalModule m@(Module name _ vdefgs) = do
>   eval_head <- evalVdefg (head vdefgs)
>   liftIO $ putStrLn $ "Result of head: " ++ show eval_head
>   mapM_ (\vdefg -> do 
>             before <- liftIO getCurrentTime
>             h <- get
>             liftIO $ putStr $ "Evaluating " ++ (vdefgName vdefg)
>             res <- evalVdefg vdefg             
>             after <- liftIO getCurrentTime
>             let 
>               id = vdefgName vdefg
>               time = after `diffUTCTime` before
>             liftIO $ putStrLn $ " ... done in " ++ show time ++ " secs. "
>             liftIO $ putStrLn $ "\tResult: " ++ show res
>             liftIO $ H.insert h id res
>         ) vdefgs
>   h <- get
>   return h

The list of value definitions represents the environment

> evalVdefg :: Vdefg -> IM Value
> evalVdefg (Rec vdefs) = return . Wrong $ "TODO: Recursive vdefg" -- "Recursive eval not yet implemented\n\t" ++ concatMap (\(Vdef ((mname,var),ty,exp)) -> var ++ " :: " ++ showType ty ++ "\n\t"++ showExp exp) vdefs
> evalVdefg (Nonrec (Vdef (qvar, ty, exp))) = 
>   let 
>      extractedType = extractType ty 
>   in case extractedType of
>     Nothing -> return . Wrong $ "Could not parse type " ++ showExtCoreType ty ++ "; therefore I did not interpret"
>     Just (CType (PType pt)) -> do
>       liftIO $ putStrLn $ "\n\n Value exp: " ++ showExp exp ++ " \n\n"
>       res <- evalExp exp -- result
>       heap <- get 
>       liftIO $ H.insert heap (qualifiedVar qvar) res
>       return $ res
>     Just (Lambda (LambdaAbstraction ct gt)) -> do
>       liftIO $ putStrLn $ "\n\n Function exp: " ++ showExp exp ++ " \n\n"
>       res <- evalExp exp -- result
>       heap <- get 
>       liftIO $ H.insert heap (qualifiedVar qvar) res
>       return $ res
>     Just ty -> return . Wrong $ "I still don't know how to evaluate values of type " ++ show ty -- ++ "\n\tExp: " ++ showExp exp ++ "\n\tResult: " ++ showIM (evalExp exp [])


> evalExp :: Exp -> IM Value

This one is a function application which has the type accompanied. We won't care about the type now, as I'm not sure how it can be used now.

Appt is always (?) applied to App together with a var that represents the function call of the Appt. In the case of integer summation, this is base:GHC.Num.f#NumInt. That is why we have to ignore the first parameter when applied.

If Appt is being applied to another appt, then we ignore another level of parameters. This is the case of function application, namely ($) :: (a -> b) -> a -> b

> evalExp (Appt function_exp ty) = do
>   heap <- get
>   g <- liftIO $ evalStateT (evalExp function_exp) heap
>   return $ Fun (\f -> apply f g) $ " Appt :: " ++ showType ty

This is the sum function

> evalExp (Var ((Just (M (P ("base"),["GHC"],"Num"))),"zp")) = let
>   add n = Fun (\m -> addValues n m) "GHC.Num.+ :: Int -> Fun (Int -> Int)"
>  in return $ Fun (\n -> return (add n)) "GHC.Num.+ :: Fun (Int -> Fun (Int -> Int))"

> evalExp (Var ((Just (M (P ("base"),["GHC"],"Num"))),"zt")) = let
>   mul n = Fun (\m -> multiplyValues n m) "GHC.Num.* :: Int -> Fun (Int -> Int)"
>  in return $ Fun (\n -> return (mul n)) "GHC.Num.* :: Fun (Int -> Fun (Int -> Int))"

> evalExp (Var ((Just (M (P ("base"),["GHC"],"Base"))),"zd")) = let
>   ap f = Fun (\x -> apply f x) "GHC.Base.$ :: Fun (a -> b) -> Fun (a -> b)"
>  in return $ Fun (\f -> return (ap f)) "GHC.Base.$ :: Fun ( Fun(a -> b) -> a -> b)"

> evalExp (Var ((Just (M (P ("base"),["GHC"],"Num"))),"zdfNumInt")) = let
>   ap f = Fun (\x -> apply f x) "GHC.Base.$fNumInt :: Fun (a -> Int) -> Fun (a -> Int)"
>  in return $ Fun (\f -> return (ap f)) "GHC.Base.$fNumInt :: Fun ( Fun(a -> Int) -> a -> Int)"

> evalExp (Lam binded_var exp) = let
>   name = bindId binded_var
>   bindAndEval binded_value = do 
>     liftIO $ putStrLn $ "Binding " ++ show binded_value ++ " to " ++ showBind binded_var
>     heap <- get
>     --TODO, this should be inserted in an environment instead and then be deleted, (gotta change the IM type). It is now added and deleted in the heap. This assumes External Core source code doesn't have any variables shadowed
>     liftIO $ H.insert heap name binded_value
>     res <- evalExp exp
>     liftIO $ H.delete heap name 
>     return res
>  in return $ Fun bindAndEval "Anonymous function"

> evalExp (App -- Integer construction
>              (Dcon ((Just (M (P ("ghczmprim"),["GHC"],"Types"))),"Izh"))
>              (Lit lit) 
>         ) = evalLit lit

> evalExp (App function_exp argument_exp) = do 
>   f <- evalExp function_exp
>   x <- evalExp argument_exp
>   liftIO . putStrLn $ "Applying " ++ show f ++ " to " ++ show x
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
> apply fun@(Fun f _) v = f v
> apply w@(Wrong _) _ = return w
> apply f m = return . Wrong $ "Applying something that is not a function, namely " ++ show f

> curry' :: Value -> IM Value -> IM Value
> curry' fun@(Fun f s) g' = do
>   h <- get
>   g <- liftIO $ evalStateT g' h
>   apply fun g
 

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

