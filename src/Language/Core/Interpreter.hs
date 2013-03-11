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
import           Language.Core.Interpreter.Acknowledge(acknowledgeTypes,acknowledgeVdefgs)
import           Language.Core.Interpreter.Structures
  
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
import           Data.List(find)
import           Data.Time.Clock(getCurrentTime,diffUTCTime)
import           DART.InterpreterSettings
import           Text.Encoding.Z(zDecodeString)

-- data & control
import Data.List(intersectBy)
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
  vdefs_env <- acknowledgeVdefgs m
  
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
       vdefs_env <- acknowledgeVdefgs m
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
        ) env | constr == "Izh" = evalLit lit
              | constr == "Czh" = evalLit lit
              | otherwise = return . Wrong $ " Constructor " ++ constr ++ " is not yet implemented. Please submit a bug report"

evalExp e@(App function_exp argument_exp) env = do
  debugMStep ("Evaluating function application {")
  ti <- gets tab_indentation
  let ?tab_indentation = ti
      
  f <- evalExpI function_exp env
  heap_reference@(id,arg_address) <- mkHeapReference argument_exp
  --decreaseIndentation
  debugM "" >> debugM ("Argument to function has reference: " ++ show heap_reference)
  
  -- apply f arg_address (heap_reference:env) -- Note1: the address is paassed 
  res <- apply f id (heap_reference:env) -- Note1: the address is paassed 
  return res

-- | A function application which has the type annotation which we will essentially ignore.
evalExp e@(Appt exp ty) env = do
  h <- gets heap
  debugMStep $ "Evaluating typed function application {"
  f <- evalExpI exp env
  return $ Fun (return' f) $ "\\"++"g -> apply " ++ show f ++ " g"

evalExp (Var ((Just (M (P ("base"),["GHC"],"Base"))),"zd")) env = let
  applyFun :: Value -> IM Value  
  applyFun (Fun f dsc) = return $ Fun f ("($) " ++ dsc)
  applyFun _ = return $ Wrong "($), Applying something that is not a function"
  
  -- takes a function `f` and returns a function `g` that applies `f` to its argument 
  ap id e = lookupId id e >>= either (evalThunk e) return >>= applyFun  
  in return $ Fun ap "($) :: (a -> b) -> a -> b"

evalExp e@(Lam binded_var exp) env = do
  whenFlag show_subexpressions $ indentExp e >>= \e -> debugM $ "Evaluating subexpression " ++ e
  return $ Fun applyFun $ "\\" ++ var_name ++ " -> exp" 
  where     
    var_name = bindVarName binded_var
    applyFun :: Id -> Env -> IM Value
    applyFun id env' = watchReductionM "\t evaluating lambda body (exp)" >>
                       evalExpI exp (env' ++ env)

-- Qualified variables that should be in the environment
evalExp e@(Var qvar) env = mkPointer (qualifiedVar qvar) env >>= flip evalPointer env
evalExp (Dcon qcon) env = mkPointer (qualifiedVar qcon) env >>= flip evalPointer env

-- Case of

evalExp (Case exp vbind@(vbind_var,_) _ alts) env = do
  --let qvar = qualifiedVar var      
    
  increaseIndentation
  heap_reference@(id,address) <- memorize (mkThunk exp) vbind_var
  exp_value <- evalHeapAddress address env

  watchReductionM $ "\tDoing case analysis for " ++ show vbind_var
  maybeAlt <- findMatch exp_value alts
  let exp = maybeAlt >>= Just . altExp -- Maybe Exp
  
  case exp of
    Just e -> do -- a matched alternative was found, in case it's a constructor, bind its arguments
      let alt = fromJust $ maybeAlt
      -- TODO IN ENVIRONMENT when(isAcon alt) $ var_val `bindAltVars` alt
      res <- evalExp e (heap_reference:env)
      -- when(isAcon alt) $ deleteAltBindedVars alt
      return res
    _ -> return . Wrong $ "Unexhaustive pattern matching of " ++ vbind_var

evalExp (Lit lit) _ = evalLit lit

-- Otherwise

evalExp otherExp _ = do
  expStr <- indentExp otherExp
  return . Wrong $ " TODO: {" ++ expStr ++ "}\nPlease submit a bug report"

-- -- | Binds variables to values in a case expression
-- bindAltVars :: Value -> Alt -> IM ()
-- bindAltVars (TyConApp (AlgTyCon _ _) vals) (Acon _ _ vbinds _) = 
--   if (length vals /= length vbinds) 
--   then error "length vals /= length vbinds"
--   else mapM_ (uncurry memorize) ((map fst vbinds) `zip` vals)
-- bindAltVars val@(Num n) (Acon _ _ [(var,_)] _) = var `bindTo` val
-- bindAltVars t v = io . putStrLn $ " Don't know how to bind values " ++ show t ++ " to " ++ show v



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
  else return . Just $ a 

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

evalLit :: Lit -> IM Value
evalLit (Literal (Lint i) ty) = case showExtCoreType ty of
   "ghc-prim:GHC.Prim.Int#" -> return . Num $ i 
   "integer-gmp:GHC.Integer.Type.Integer" -> return . Num $ i 
   _ -> return . Wrong $ showExtCoreType ty ++ " .. expected " ++ "ghc-prim:GHCziPrim.Int#"

evalLit (Literal (Lrational r) ty) = case showExtCoreType ty of
  "Rational" -> return . Rat $ r
  _ -> return . Wrong $ showExtCoreType ty ++ " .. expected " ++ "Rational"

evalLit (Literal (Lchar c) ty) = case showExtCoreType ty of
  "ghc-prim:GHC.Prim.Char#" -> return . Char $ c 
  _ -> return . Wrong $ showExtCoreType ty ++ "ghc-prim:GHC.Prim.Char#" ++ "Char"

evalLit (Literal (Lstring s) ty) = case showExtCoreType ty of
   "ghc-prim:GHC.Prim.Addr#" -> return . String $ s
   _ -> return . Wrong $ showExtCoreType ty ++ " .. expected " ++ "ghc-prim:GHC.Prim.Addr#"

-- | Loads nothing ATM, but it'll be useful
loadLibrary :: (?settings :: InterpreterSettings) => [(Id, Either Thunk Value)] -> IM Env
loadLibrary funs = mapM (uncurry $ flip memorize) funs

evalFails :: String -> IM (Either Thunk Value)
evalFails = return . Right . Wrong
  
-- | Does the same as evalExp but indents and deindents for debugging output purposes
evalExpI :: Exp -> Env -> IM Value
evalExpI exp env = do  
  increaseIndentation
  debugSubexpression exp 
  debugM "" -- new line
  res <- evalExp exp env
  decreaseIndentation
  debugMStepEnd
  return res

evalThunk :: Env -> Thunk -> IM Value
evalThunk env (Thunk exp) = evalExp exp env
    
-- | Given a Pointer HeapAddress, eval a Thunk if necessary to return a Value represented
-- by the address 
evalPointer :: Value -> Env -> IM Value
evalPointer (Pointer address) env = evalHeapAddress address env
evalPointer e@(Wrong s) _ = return e
evalPointer _ _ = return . Wrong $ "evalPointer: The impossible happened"

-- | Given an environment, looks for the address in the heap, evals a thunk using the given environment if necessary to return a value
evalHeapAddress :: HeapAddress -> Env -> IM Value
evalHeapAddress address env = lookupMem address >>= either (evalThunk env) return

-- | Looks for the address in the heap, evals a thunk if necessary to return a value
evalId :: Id -> Env -> IM Value
evalId i e = debugM ("Evaluating id " ++ show i) >> lookupId i e >>= either (evalThunk e) return

-- | Function application
apply :: Value -> Id -> Env -> IM Value
apply (Fun f d) id env = do
  debugM $ "Applying id " ++ id ++ " to function " ++ d
  res <- f id env
  debugM $ "Res: " ++ show res
  return res
    
apply (TyConApp (AlgTyCon name (ty:tys)) vals) id env = 
  -- Applies a (possibly applied) type constructor that expects appliedValue of type ty.
  -- The type constructor that we are applying has |vals| applied values
  -- Returns a new type constructor that will take |tys| more values
  do 
    val <- lookupId id env >>= either (evalThunk env) return
    return $ TyConApp newTyCon (mkAppValues val)
  where
    newTyCon :: TyCon
    newTyCon = AlgTyCon name tys -- expects one value less
    mkAppValues :: Value -> [Either Thunk Value]
    mkAppValues v = vals ++ [Right v] -- :: records the (just) applied value *as a pointer*
  
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

printENV env = debugM $ "Env: " ++ concatMap (\c -> show (fst c) ++ ",") env

-- | A function that ignores its parameters and returns a value
-- the parenthesis in the signature have no effects and are only here to understand better 
return' :: Value -> (Id -> Env -> IM Value)
return' v = \_ -> \_ -> return v
