
> {-# LANGUAGE ImplicitParams #-}

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
> import qualified Language.Core.Interpreter.GHC.Classes as GHC.Classes

> import Language.Core.Core
> import Language.Core.ValueDefinition (vdefgId,vdefgName)
> import Language.Core.Util(qualifiedVar,showVdefg,showType,showExtCoreType,showExp,showMname,bindId,showBind)
> import Language.Core.TypeExtractor(extractType)
> import Language.Core.TypeExtractor.DataTypes

> import qualified Data.HashTable.IO as H

> import Control.Monad.State.Lazy

> import Data.Time.Clock(getCurrentTime,diffUTCTime)
> import Data.List(find)
> import DART.CmdLine
> import Text.Encoding.Z(zDecodeString)

Given a module which contains a list of value definitions, *vd*, evaluate every *vd* and return a heap with their interpreted values.

Value definition to mapped values
-----------------------------------------------------
| Value definition type              | mapped to    |
| Concrete type e.g. Int             | Num val      |
| Concrete type e.g. Int -> Int      | ?            |
-----------------------------------------------------

> evalModule :: (?debug :: Bool, ?show_expressions :: Bool) => Module -> IM Heap
> evalModule m@(Module name tdefs vdefgs) = do
>   mapM_ (\vdefg -> do 
>             before <- liftIO getCurrentTime
>             h <- get
>             io . dodebugNoLine $ "Evaluating " ++ (vdefgId vdefg) ++ " to ... "
>             res <- evalVdefg vdefg             
>             after <- liftIO getCurrentTime
>             let 
>               id = vdefgId vdefg
>               time = after `diffUTCTime` before
>             io . dodebugNoLine $ show res
>             io . dodebug $ "; done in " ++ show time ++ " secs. "
>             liftIO $ H.insert h id res
>         ) vdefgs
>   h <- get
>   return h

Given a module and a function name, we evaluate the function in that module and return the heap. 

> evalModuleFunction :: (?debug :: Bool, ?show_expressions :: Bool) => Module -> String -> IM Value
> modl@(Module mname _ vdefgs) `evalModuleFunction` fname = 
>   if null fname then 
>     error $ "evalModuleFunction: function name is empty" 
>   else case maybeVdefg of
>     Nothing -> return . Wrong $  "Could not find function " ++ fname ++ " in " ++ showMname (Just mname)
>     Just vdefg -> do
>       io . dodebug $ "Found function name " ++ fname
>       heap <- get -- should be empty
>       evalVdefg vdefg
>   where
>     fnames = map vdefgName vdefgs -- [String]
>     fnames_vdefgs = zip fnames vdefgs 
>     maybeVdefg = find ((==) fname . fst) fnames_vdefgs >>= return . snd 

The list of value definitions represents the environment

> evalVdefg :: (?debug :: Bool, ?show_expressions :: Bool) => Vdefg -> IM Value

> evalVdefg (Rec (v@(Vdef _):[]) ) = evalVdefg $ Nonrec $ v

More than one vdef? I haven't found a test case (TODO)

> evalVdefg (Rec vdefs) = return . Wrong $ "TODO: Recursive eval not yet implemented\n\t" ++ concatMap (\(Vdef ((mname,var),ty,exp)) -> " VDEF; " ++ var ++ " :: " ++ showType ty ++ "\n\t"++ showExp exp) vdefs

> evalVdefg (Nonrec (Vdef (qvar, ty, exp))) = do
>   whenFlag (?show_expressions) $ do
>     io . dodebug $ "\n\n Value exp: " ++ showExp exp ++ " \n\n"
>   res <- evalExp exp -- result
>   heap <- get 
>   liftIO $ H.insert heap (qualifiedVar qvar) res
>   return res

> evalExp :: Exp -> IM Value

This one is a function application which has the type accompanied. We won't care about the type now, as I'm not sure how it can be used now.

Appt is always (?) applied to App together with a var that represents the function call of the Appt. In the case of integer summation, this is base:GHC.Num.f#NumInt. That is why we have to ignore the first parameter when applied.

If Appt is being applied to another appt, then we ignore another level of parameters. This is the case of function application, namely ($) :: (a -> b) -> a -> b

> evalExp e@(Appt function_exp ty) = do
>   heap <- get
>   --liftIO . putStrLn $ "Evaluating subexpression " ++ showExp function_exp
>   f <- liftIO $ evalStateT (evalExp function_exp) heap
>   return $ Fun (\g -> apply f g) $ "\\"++"g -> apply " ++ show f ++ " g"

> evalExp (Var ((Just (M (P ("base"),["GHC"],"Base"))),"zd")) = let
>   ap f = Fun (\x -> apply f x) "GHC.Base.$ :: Fun(a -> b)"
>  in return $ Fun (\f -> return (ap f)) "$" -- GHC.Base.$ :: Fun(a -> b) -> Fun(a -> b)

> evalExp (Lam binded_var exp) = let
>   name = bindId binded_var
>   bindAndEval binded_value = do 
>     --liftIO $ putStrLn $ "\t getting the heap"
>     heap <- get
>     --TODO, this should be inserted in an environment instead and then be deleted, (gotta change the IM type). It is now added and deleted in the heap. This assumes External Core source code doesn't have any variables shadowed
>     --liftIO $ putStrLn $ "\t binding " ++ name ++ " to " ++ show binded_value ++ " in the heap"
>     liftIO $ H.insert heap name binded_value
>     --liftIO $ putStrLn $ "\t evaluating lambda body"
>     res <- evalExp exp
>     --liftIO $ putStrLn $ "\t deleting binded value for " ++ bindId binded_var ++ " in the heap"
>     --liftIO $ H.delete heap name 
>     return res
>  in return $ Fun bindAndEval $ "\\" ++ bindId binded_var ++ " -> exp" 

> evalExp (App -- Integer construction
>          (Dcon ((Just (M (P ("ghczmprim"),["GHC"],"Types"))),"Izh"))
>          (Lit lit) 
>         ) = evalLit lit

> evalExp e@(App function_exp argument_exp) = do 
>   --liftIO . putStrLn $ "Evaluating subexpression " ++ showExp e
>   f <- evalExp function_exp
>   --liftIO . putStrLn $ " f: " ++ showExp function_exp ++ " => " ++ show f
>   x <- evalExp argument_exp
>   --liftIO . putStrLn $ " x: " ++ showExp argument_exp ++ " => " ++ show x
>   res <- apply f x
>   --liftIO . putStrLn $ "\t Applying f x  = " ++ show res
> --   liftIO . putStrLn $ "\t Applying " ++ show f ++ " to " ++ show x
>   --liftIO . putStrLn $ "Evaluating subexpression " ++ showExp e ++ " => " ++ show res
>   return res

Variables 

> evalExp e@(Var qvar) = 
>   let
>     -- Interpreter modules that might know what to do with `qvar`
>     -- this list is of type [Qual Var -> Maybe Value]
>     lib_vars = [GHC.Num.evalVar,GHC.Classes.evalVar]
>     lib_val = callEvalVar lib_vars qvar -- Maybe Value
>   in
>    -- Space for improvement: maybe we should lookupVar before finding lib functions?
>    -- It might be more common
>    case lib_val of  
>      Just val -> return val
>      _ -> lookupVar $ qualifiedVar qvar

Case of

> evalExp (Case exp (var,_) _ alts) = do
>   var_val <- lookupVar var
>   exp <- return $ find (matches var_val) alts >>= Just . altExp
>   case exp of
>     Just e -> evalExp e
>     _ -> return . Wrong $ "Unexhaustive pattern matching of " ++ var

Literals 

> evalExp (Lit lit) = evalLit lit

Data constructors

> evalExp (Dcon qcon) = lookupVar . qualifiedVar $ qcon

Otherwise

> evalExp otherExp = return . Wrong $ " TODO: " ++ showExp otherExp

> matches :: Value -> Alt -> Bool
> val `matches` (Acon qual_dcon tbs vbs idx_exp) = False --TODO
> val `matches` (Alit lit exp) = False --TODO
> val `matches` (Adefault _) = True -- this is the default case, i.e. "_ -> " 

> altExp :: Alt -> Exp
> altExp (Acon _ _ _ exp) = exp
> altExp (Alit _ exp) = exp
> altExp (Adefault exp) = exp

> evalLit :: Lit -> IM Value
> evalLit (Literal (Lint i) ty) = case showExtCoreType ty of
>   "ghczmprim:GHC.Prim.Intzh" -> return . Num $ i 
>   "integerzmgmp:[\"GHC\",\"Integer\"].Type.Integer" -> return . Num $ i 
>   _ -> return . Wrong $ showExtCoreType ty ++ " .. expected " ++ "ghczmprim:GHCziPrim.Intzh"

> evalLit (Literal (Lrational r) ty) = case showExtCoreType ty of
>   "Rational" -> return . Rat $ r
>   _ -> return . Wrong $ showExtCoreType ty ++ " .. expected " ++ "Rational"

> evalLit (Literal (Lchar c) ty) = case showExtCoreType ty of
>   "ghczmprim:CHAR" -> return . Char $ c 
>   _ -> return . Wrong $ showExtCoreType ty ++ " .. expected " ++ "Char"

> evalLit (Literal (Lstring s) ty) = case showExtCoreType ty of
>   "ghczmprim:GHC.Prim.STRING" -> return . String $ s
>   _ -> return . Wrong $ showExtCoreType ty ++ " .. expected " ++ "String"

> lookupVar :: Id -> IM Value
> lookupVar x = do
>   --liftIO . putStrLn $ "Looking up var " ++ x
>   env <- get
>   maybeV <- liftIO $ H.lookup env x
>   case maybeV of
>     Nothing -> 
>       let
>         zDecoded = zDecodeString x
>       in if (zDecoded /= x) then 
>            return . Wrong $ "Could not find " ++ zDecoded ++ " in the libraries"
>          else 
>            return . Wrong $ "Could not find " ++ x ++ " in the environment"
>     Just v -> return v

A sort of findMaybe and ($) i.e. it returns only one maybe, the first Just found by mapping the functions to qv, or Nothing.

> callEvalVar :: [(Qual Var -> Maybe Value)] -> Qual Var -> Maybe Value
> callEvalVar [] qv = Nothing
> callEvalVar (eqv:eqvs) qv = 
>   case eqv qv of
>     v@(Just value) -> v
>     _ -> callEvalVar eqvs qv

