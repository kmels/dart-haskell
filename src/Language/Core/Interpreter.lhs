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

> import Language.Core.Interpreter.Apply
> import Language.Core.Interpreter.Structures

> import qualified Language.Core.Interpreter.GHC.Num as GHC.Num

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

> evalExp e@(Appt function_exp ty) = do
>   heap <- get
>   liftIO . putStrLn $ "Evaluating subexpression " ++ showExp function_exp
>   g <- liftIO $ evalStateT (evalExp function_exp) heap
>   return $ Fun (\f -> apply f g) $ "\\"++"f :: (a -> " ++ showType ty ++ ") -> apply f " ++ show g

> evalExp (Var ((Just (M (P ("base"),["GHC"],"Base"))),"zd")) = let
>   ap f = Fun (\x -> apply f x) "GHC.Base.$ :: Fun(a -> b)"
>  in return $ Fun (\f -> return (ap f)) "$" -- GHC.Base.$ :: Fun(a -> b) -> Fun(a -> b)

> evalExp (Lam binded_var exp) = let
>   name = bindId binded_var
>   bindAndEval binded_value = do 
>     liftIO $ putStrLn $ "\t getting the heap"
>     heap <- get
>     --TODO, this should be inserted in an environment instead and then be deleted, (gotta change the IM type). It is now added and deleted in the heap. This assumes External Core source code doesn't have any variables shadowed
>     liftIO $ putStrLn $ "\t binding " ++ name ++ " to " ++ show binded_value ++ " in the heap"
>     liftIO $ H.insert heap name binded_value
>     liftIO $ putStrLn $ "\t evaluating lambda body"
>     res <- evalExp exp
>     liftIO $ putStrLn $ "\t deleting binded value for " ++ bindId binded_var ++ " in the heap"
>     --liftIO $ H.delete heap name 
>     return res
>  in return $ Fun bindAndEval $ "\\" ++ bindId binded_var ++ " -> exp" 

> evalExp (App -- Integer construction
>              (Dcon ((Just (M (P ("ghczmprim"),["GHC"],"Types"))),"Izh"))
>              (Lit lit) 
>         ) = evalLit lit

> evalExp e@(App function_exp argument_exp) = do 
>   f <- evalExp function_exp
>   x <- evalExp argument_exp
>   liftIO . putStrLn $ "Evaluating subexpression " ++ showExp e
>   liftIO . putStrLn $ "Applying " ++ show f ++ " to " ++ show x
>   apply f x

Variables 

> evalExp e@(Var qvar) = 
>   let
>     -- Interpreter modules that might know what to do with `qvar`
>     -- this list is of type [Qual Var -> Maybe Value]
>     lib_vars = [GHC.Num.evalVar]
>     lib_val = callEvalVar lib_vars qvar -- Maybe Value
>   in
>    -- Space for improvement: maybe we should lookupVar before finding lib functions?
>    -- It might be more common
>    case lib_val of  
>      Just val -> return val
>      _ -> lookupVar $ qualifiedVar qvar

Otherwise

> evalExp otherExp = return . Wrong $ " TODO: " ++ showExp otherExp

> evalLit :: Lit -> IM Value
> evalLit (Literal (Lint i) ty) = if (showExtCoreType ty == "ghczmprim:GHC.Prim.Intzh")
>                                 then return . Num $ i 
>                                 else return . Wrong $ showExtCoreType ty ++ " .. expected " ++ "ghczmprim:GHCziPrim.Intzh"

> lookupVar :: Id -> IM Value
> lookupVar x = do
>   liftIO . putStrLn $ "Looking up var " ++ x
>   env <- get
>   maybeV <- liftIO $ H.lookup env x
>   case maybeV of
>     Nothing -> return . Wrong $ "Could not find " ++ x ++ " in the environment"
>     Just v -> return v

> getInteger :: IM Value -> IM Value
> getInteger mv = do
>   n <- mv -- first arg
>   case n of
>     (Num i) -> mv
>     other_ty -> return . Wrong $ "Expected Integer, found " ++ show other_ty


-- Some sort of findMaybe and ($) i.e. it returns only one maybe, the first Just found by mapping the functions to qv, or Nothing.

> callEvalVar :: [(Qual Var -> Maybe Value)] -> Qual Var -> Maybe Value
> callEvalVar [] qv = Nothing
> callEvalVar (eqv:eqvs) qv = 
>   case eqv qv of
>     v@(Just value) -> v
>     _ -> callEvalVar eqvs qv
