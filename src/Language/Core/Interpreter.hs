{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE DoAndIfThenElse #-}

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
-- Interprets External Core expressions non-strictly
-----------------------------------------------------------------------------

module Language.Core.Interpreter where

import           Language.Core.Interpreter.Apply
import           Language.Core.Interpreter.Acknowledge(acknowledgeTypes, 
                                                       acknowledgeVdefgs,
                                                       acknowledgeVdefg)
import           Language.Core.Interpreter.Util(return')
import           Language.Core.Interpreter.Structures
import           Language.Core.Interpreter.Evaluable(eval)
  
import           Control.Applicative((<|>))
import qualified Data.HashTable.IO as H
import           Data.Maybe
import           Language.Core.Core
import           Language.Core.TypeExtractor(extractType)
import           Language.Core.TypeExtractor.DataTypes
import           Language.Core.Util(qualifiedVar,showVdefg,showType,showExtCoreType,showExp,bindVarName,showBind)
import           Language.Core.Vdefg (isTmp,vdefgId,vdefgName)

import           Control.Monad.State.Lazy
import           DART.CmdLine
import           DART.FileIO
import           DART.InterpreterSettings
import           Data.List(find)
import           Data.Time.Clock(getCurrentTime,diffUTCTime)
import           Text.Encoding.Z(zDecodeString)
-- data & control
import           Data.List(intersectBy)
{-Given a module which contains a list of value definitions, *vd*, evaluate every *vd* and return a heap with their interpreted values.

Value definition to mapped values
-----------------------------------------------------
| Value definition type              | mapped to    |
| Concrete type e.g. Int             | Num val      |
| Concrete type e.g. Int - Int      | ?            |
-----------------------------------------------------
-}

doEvalVdefg :: Vdefg -> Env -> IM HeapReference
doEvalVdefg vdefg env = do
  before <- liftIO getCurrentTime
  h <- gets heap
  sttgs <- gets settings
  debugM $ "Evaluating " ++ (vdefgId vdefg)
  (heapRef,res) <- evalVdefg vdefg env
  after <- liftIO getCurrentTime
  let 
    id = vdefgId vdefg
    time = after `diffUTCTime` before    
    should_print = debug sttgs && show_tmp_variables sttgs
                   || debug sttgs && show_tmp_variables sttgs && (not $ isTmp vdefg)
  (when should_print) $ do
    debugM $ "Evaluation of " ++ (vdefgId vdefg)
    debugM $ "\t.. done in " ++ show time ++ "\n\t.. and resulted in " ++ show res
  
  return heapRef

evalModule :: (?settings :: InterpreterSettings) => Module -> Env -> IM [(Id, Value)]
evalModule m@(Module name tdefs vdefgs) libs_env = do
  -- recognize type and value definitions
  tycons_env <- acknowledgeTypes m
  vdefs_env <- acknowledgeVdefgs m --libs_env
  
  -- time to evaluate, set an environment and evaluate only those defs that are not temp
  let 
    env = tycons_env ++ vdefs_env ++ libs_env
    exposed_vdefgs = filter (not . isTmp) vdefgs    
  heap_refs <- mapM (flip doEvalVdefg env) exposed_vdefgs
  
  -- lookup values in memory
  vals <- mapM (flip (evalHeapAddress . snd) []) heap_refs --fun_heap_refs
  return $ zip (map fst heap_refs) vals

-- | Given a module and a function name, we evaluate the function in that module and return the heap. 

evalModuleFunction :: (?settings :: InterpreterSettings) => Module -> String -> Env -> IM Value
evalModuleFunction m@(Module mname tdefs vdefgs) fname libs = 
   if null fname then 
     error $ "evalModuleFunction: function name is empty" 
   else case maybeVdefg of
     Nothing -> return . Wrong $  "Could not find function " ++ fname ++ " in " ++ show mname
     Just vdefg -> do
       debugM $ "Found definition of " ++ fname
       tycons_env <- acknowledgeTypes m 
       vdefs_env <- acknowledgeVdefgs m --(libs ++ tycons_env)
       let env = (tycons_env ++ vdefs_env ++ libs)
       heap_ref@(_,address) <- doEvalVdefg vdefg env  -- ++ libs)
       evalHeapAddress address env
   where
     fnames = map vdefgName vdefgs -- [String]
     fnames_vdefgs = zip fnames vdefgs 
     maybeVdefg = find ((==) fname . fst) fnames_vdefgs >>= return . snd -- :: Maybe Vdefg


-- | Evaluate a value definition, which can be either recursive or not recursive

evalVdefg :: Vdefg -> Env -> IM (HeapReference,Value)
evalVdefg (Rec (v@(Vdef _):[]) ) env = do
  evalVdefg (Nonrec v) env
-- More than one vdef? I haven't found a test case (TODO)
evalVdefg (Rec vdefs) env = return $ (,) ("",0) (Wrong "TODO: Recursive eval not yet implemented\n\t" )

evalVdefg (Nonrec (Vdef (qvar, ty, exp))) env = do
  whenFlag show_expressions $ do    
    debugMStep $ "Evaluating value definition"
    indentExp exp >>= debugM . (++) "Expression: " 
  increaseIndentation
  res <- evalExp exp env  -- result
  decreaseIndentation
  
  h <- gets heap
  
  heap_ref <- memorize (mkVal res) (qualifiedVar qvar)
  return $ (heap_ref,res)

evalExp :: Exp -> Env -> IM Value

evalExp (App -- Integer,Char construction
          (Dcon ((Just (M (P ("ghczmprim"),["GHC"],"Types"))),constr))
          (Lit lit) 
        ) env | constr == "Izh" = eval lit []
              | constr == "Czh" = eval lit []
              | otherwise = return . Wrong $ " Constructor " ++ constr ++ " is not yet implemented. Please submit a bug report"

evalExp e@(App function_exp argument_exp) env = do
  ti <- gets tab_indentation
  let ?tab_indentation = ti
      
  f <- evalExpI function_exp env "Evaluating function application"
  
  -- if the argument is a variable that is already in the env, don't make a new reference  
  case argument_exp of
    (Var qvar) -> do
      qvar_val <- qualifiedVar qvar `lookupId` env
      case qvar_val of
        Right (Wrong _) -> mkRefAndApply f argument_exp -- not found, 
        whnf -> apply f (qualifiedVar qvar) env --don't create thunk for variables in scope
    _ -> mkRefAndApply f argument_exp
  where
    mkRefAndApply :: Value -> Exp -> IM Value
    mkRefAndApply f arg_exp = do
      heap_reference@(id,arg_address) <- mkHeapReference arg_exp env
      --decreaseIndentation
      debugM "" >> debugM ("Argument to function has reference: " ++ show heap_reference)
  
      -- apply f arg_address (heap_reference:env) -- Note1: the address is paassed 
      res <- apply f id (heap_reference:env) -- Note1: the address is paassed 
      return res

-- | A function application which has the type annotation which we will essentially ignore.
evalExp e@(Appt exp ty) env = evalExp exp env

evalExp (Var ((Just (M (P ("base"),["GHC"],"Base"))),"zd")) env = let
  applyFun :: Value -> IM Value  
  applyFun (Fun f dsc) = return $ Fun f ("($) " ++ dsc)
  applyFun _ = return $ Wrong "($), Applying something that is not a function"
  
  -- takes a function `f` and returns a function `g` that applies `f` to its argument 
  ap id e = evalId id e >>= applyFun  
  in return $ Fun ap "($) :: (a -> b) -> a -> b"

-- lambda abstraction over types variables
-- returns a Fun value
evalExp e@(Lam (Vb (var_name,ty)) exp) env = do
  --whenFlag show_subexpressions $ indentExp e >>= \e -> debugM $ "Evaluating subexpression " ++ e
  debugM $ "Making function from lambda abstraction \\" ++ var_name ++ " :: " ++ showType ty ++ " -> exp "
  return $ Fun applyFun $ "\\" ++ var_name ++ " -> exp" 
  where     
    applyFun :: Id -> Env -> IM Value
    applyFun id env' = do
      --make alias (var_name should point to where id is)
      --so that the body of exp can find var_name
      heap_ref <- lookupId id env' >>= flip memorize var_name
      evalExpI exp (heap_ref:env) "Evaluating Lambda body (exp)"

-- lambda abstraction over variables
-- ignores the type argument, evaluating the expression
evalExp e@(Lam (Tb (var_name,_)) exp) env = do
  whenFlag show_subexpressions $ indentExp e >>= \e -> debugM $ "Evaluating subexpression " ++ e
  debugM "Ignoring type parameter"
  evalExpI exp env "Evaluating lambda body (exp)"
      
-- Qualified variables that should be in the environment
evalExp e@(Var qvar) env = mkPointer (qualifiedVar qvar) env >>= flip evalPointer env
evalExp (Dcon qcon) env = mkPointer (qualifiedVar qcon) env >>= flip evalPointer env

-- Case of

evalExp (Case exp vbind@(vbind_var,_) _ alts) env = do
  --let qvar = qualifiedVar var      
    
  increaseIndentation
  heap_reference@(id,address) <- memorize (mkThunk exp env) vbind_var
  exp_value <- evalHeapAddress address (heap_reference:env)

  watchReductionM $ "\tDoing case analysis for " ++ show vbind_var
  maybeAlt <- findMatch exp_value alts
  let exp = maybeAlt >>= Just . altExp -- Maybe Exp
  
  --debugM $ "EXP: "++ show exp
  case exp of
    Just e -> do -- a matched alternative was found, in case it's a constructor, bind its arguments
      let alt = fromJust $ maybeAlt
      
      debugM "Making altEnv"
      alt_env <- mkAltEnv exp_value alt
      debugM $ "This is the alt env: " ++ show alt_env
      debugM "End making altEnv"
      -- TODO IN ENVIRONMENT when(isAcon alt) $ var_val `bindAltVars` alt
      res <- evalExp e (heap_reference:env ++ alt_env)
      debugM "End making altEnv"
      -- when(isAcon alt) $ deleteAltBindedVars alt
      return res
    _ -> return . Wrong $ "Unexhaustive pattern matching of " ++ vbind_var

evalExp (Lit lit) _ = eval lit []

evalExp (Let vdefg exp) env = do
  env' <- acknowledgeVdefg vdefg env
  evalExp exp (env' ++ env)
  
-- Otherwise

evalExp otherExp _ = do
  expStr <- indentExp otherExp
  return . Wrong $ " TODO: {" ++ expStr ++ "}\nPlease submit a bug report"

-- | Given an alternative and a value that matches the alternative,
-- binds the free variables in memory and returns a list of references (an environment)
mkAltEnv :: Value -> Alt -> IM Env
mkAltEnv (TyConApp (AlgTyCon _ _) vals) (Acon _ _ vbinds _) = debugM ("Vals: " ++ show vals) >> debugM ("Vbinds :: " ++ show vbinds) >> 
  if (length vals /= length vbinds)
  then error "length vals /= length vbinds"
  else debugM "CAFE" >> mapM (uncurry memorize) (vals `zip` (map fst vbinds))
mkAltEnv _ _ = return []
  
--bindAltVars val@(Num n) (Acon _ _ [(var,_)] _) = var `bindTo` val
--bindAltVars t v = io . putStrLn $ " Don't know how to bind values " ++ show t ++ " to " ++ show v



isAcon :: Alt -> Bool
isAcon (Acon _ _ _ _) = True
isAcon _ = False


-- | Tries to find an alternative that matches a value. It returns the first match, if any.
findMatch :: Value -> [Alt] -> IM (Maybe Alt)
findMatch val [] = return Nothing
findMatch val (a:alts) = do  
  matches <- val `matches` a
  
  if (not matches)
  then val `findMatch` alts   
  else do
    debugM $ "Found case match for" ++ show val
    return . Just $ a

matches :: Value -> Alt -> IM Bool
(TyConApp (AlgTyCon n _) vals) `matches` (Acon qual_dcon tbs vbs idx_exp) = do

  let tyconId = qualifiedVar qual_dcon
      matches' = tyconId == n
      
  watchReductionM $ 
    "Trying to match value with type constructor " ++ tyconId ++ 
    "\t.. " ++ if matches' then " matches" else " does not match"
        
  return $ matches'
    
val `matches` (Alit lit exp) = return False --TODO
val `matches` (Adefault _) = return True -- this is the default case, i.e. "_ - " 
(Num n) `matches` (Acon qdcon _ _ _) = do
  watchReductionM $ "Trying to match a Num value (" ++ show n ++ ") with the type constructed by " ++ qualifiedVar qdcon    
  let matches' = qualifiedVar qdcon == "ghc-prim:GHC.Types.I#" 
  
  watchReductionM $ "\t.. " ++ if matches' then " matches" else " does not match"

  return matches'
  
(Boolean False) `matches` (Acon qdcon _ _ _) = return $ qualifiedVar qdcon == "ghc-prim:GHC.Types.False"
(Boolean True) `matches` (Acon qdcon _ _ _) = return $ qualifiedVar qdcon == "ghc-prim:GHC.Types.True"
val `matches` alt = do
  io . putStrLn $ "??? Matching " ++ show val ++ " with " ++ show alt
  return False
  
altExp :: Alt -> Exp
altExp (Acon _ _ _ exp) = exp
altExp (Alit _ exp) = exp
altExp (Adefault exp) = exp

-- | Loads nothing ATM, but it'll be useful
loadLibrary :: (?settings :: InterpreterSettings) => [(Id, Either Thunk Value)] -> IM Env
loadLibrary funs = mapM (uncurry $ flip memorize) funs

evalFails :: String -> IM (Either Thunk Value)
evalFails = return . Right . Wrong
  
-- | Does the same as evalExp but indents and deindents for debugging output purposes
evalExpI :: Exp -> Env -> String -> IM Value
evalExpI exp env desc = do 
  debugMStep $ desc ++ " {"
  increaseIndentation
  debugSubexpression exp 
  debugM "" -- new line
  res <- evalExp exp env
  decreaseIndentation
  debugMStepEnd
  return res

evalThunk :: Thunk -> Env -> IM Value
evalThunk (VdefgThunk exp) env = evalExp exp env
evalThunk (Thunk exp env) _ = do
  ti <- gets tab_indentation
  let ?tab_indentation = ti
  debugM $ "Evaluating thunk: " ++ showExp exp
  evalExp exp env

    
-- | Given a Pointer HeapAddress, eval a Thunk if necessary to return a Value represented
-- by the address 
evalPointer :: Value -> Env -> IM Value
evalPointer (Pointer address) env = evalHeapAddress address env
evalPointer e@(Wrong s) _ = return e
evalPointer _ _ = return . Wrong $ "evalPointer: The impossible happened"

-- | Given an environment, looks for the address in the heap, evals a thunk using the given environment if necessary to return a value
evalHeapAddress :: HeapAddress -> Env -> IM Value
evalHeapAddress address env = do
  eTnkVal <- lookupMem address
  -- if it is a thunk, eval and memorize in heap
  val <- either (flip evalThunk env) return eTnkVal  
  -- re-save
  h <- gets heap
  io $ H.insert h address (Right val)
  return val
  
-- | Looks for the address in the heap, evals a thunk if necessary to return a value
evalId :: Id -> Env -> IM Value
--evalId i e = lookupId i e >>= either (evalThunk e) return
evalId i e = do
  ptr <- mkPointer i e 
  case ptr of
    e@(Wrong s) -> return e -- i was not found in env
    Pointer heap_address -> do -- we know something about i in env
      eTnkVal <- lookupMem heap_address
      debugM $ "lookupId " ++ i ++ " = " ++ show eTnkVal
      whnf <- case eTnkVal of  -- evaluate to weak head normal form
        Left thunk -> do
          val <- evalThunk thunk e
          h <- gets heap
          io $ H.insert h heap_address (Right val)
          return val
        Right val -> return val  -- it is already in weak head normal form
      debugM (show i ++ " ~> " ++ show whnf)
      return whnf
  
-- | Function application
apply :: Value -> Id -> Env -> IM Value
apply (Fun f d) id env = do
  debugM $ "applying " ++ d ++ " to " ++ id 
  res <- f id env  
  debugM $ "apply " ++ d ++ " to " ++ id ++ " => " ++ show res   
  return res
    
-- Applies a (possibly applied) type constructor that expects appliedValue of type ty.
-- The type constructor that we are applying has |vals| applied values
apply (TyConApp tycon vals) id env =  do 
    val <- evalId id env    
    return $ TyConApp tycon (vals ++ [Right val])
    
apply w@(Wrong _) _ _ = return w
apply f x _ = return . Wrong $ "Applying " ++ show f ++ " with argument " ++ show x

increaseIndentation :: IM ()
increaseIndentation = get >>= put . increase_indentation
decreaseIndentation :: IM ()
decreaseIndentation = get >>= put . decrease_indentation

increase_indentation :: DARTState -> DARTState
increase_indentation s = s { tab_indentation  = tab_indentation s + 1 }
decrease_indentation :: DARTState -> DARTState
decrease_indentation s = s { tab_indentation  = tab_indentation s - 1 }
