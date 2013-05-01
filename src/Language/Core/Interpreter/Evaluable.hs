----------------------------------------------------------------------------
-- |
-- Module      :  Language.Core.Interpreter.Evaluable
-- Copyright   :  (c) Carlos López-Camey, University of Freiburg
-- License     :  BSD-3
--
-- Maintainer  :  c.lopez@kmels.net
-- Stability   :  stable
--
--
-- Contains a type class that describes data types that can be evaluated to a value
-----------------------------------------------------------------------------

{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE DoAndIfThenElse #-}

module Language.Core.Interpreter.Evaluable where

import           DART.CmdLine
import           DART.Compiler.JIT(jitCompile)
import           DART.InterpreterSettings
import qualified Data.HashTable.IO as H
import           Data.List(find,inits)
import           Data.Time.Clock
import           Data.Time.Clock(getCurrentTime,diffUTCTime)
import           Language.Core.Interpreter.Acknowledge
import           Language.Core.Interpreter.Apply
import           Language.Core.Interpreter.Structures
import           Language.Core.Vdefg (findVdefByName,vdefgNames,vdefName)
import           Language.Core.Module
class Evaluable a where
  eval :: a -> Env -> IM Value
  
instance Evaluable Lit where
  eval l@(Literal coreLit ty) _ = case showExtCoreType ty of
    "ghc-prim:GHC.Prim.Int#" -> let (Lint i) = coreLit in return . Num $ i 
    "integer-gmp:GHC.Integer.Type.Integer" -> let (Lint i) = coreLit in return . Num $ i 
    "ghc-prim:GHC.Prim.Char#" -> case coreLit of
      (Lchar c) -> return . Char $ c  
      (Lint i) -> return . Num $ i 
    "ghc-prim:GHC.Prim.Addr#" -> let (Lstring s) = coreLit in return . String $ s
    --"Rational" -> return . Rat $ r
    _ -> return . Wrong $ "Could not evaluate literal of type " ++ showExtCoreType ty

-- | If the user provides an expression to evaluate, it can be either a haskell expression
-- that we can compile just in time, or a function name that is defined in the module
instance Evaluable HaskellExpression where
  eval hs@(HaskellExpression expression_string m@(Module mname tdefs vdefgs)) env = 
    -- | Is it a function defined within the module?
    case (m `moduleFindVdefByName` expression_string) of
      Just vdef -> do
        debugM $ "Found a definition of " ++ expression_string
        --eval (ModuleFunction vdef m) env
        eval (Nonrec vdef) env
      Nothing -> do
        debugM $ "Did not found any function named " ++ expression_string
        let 
          fnames = concatMap vdefgNames vdefgs -- [String]
          fnames_vdefgs = zip fnames vdefgs  -- [(String,Vdefg)]
          --maybeVdefg = find (\t -> fst t == id) fnames_vdefgs >>= return . snd -- :: Maybe Vdefg

        debugM $ show $ fnames
        debugM $ show $ length fnames
        debugM $ show $ length vdefgs
        debugM $ show $ fnames_vdefgs
        debugM $ show $ (map fst fnames_vdefgs)
        --debugM $ show $ maybeVdefg
        
        jitMaybeVdef <- jitCompile hs
        
        case jitMaybeVdef of
          Just vdefg -> eval vdefg env
          Nothing -> return . Wrong $  "Could not evaluate " ++ expression_string ++ " in " ++ show mname
      
instance Evaluable Vdefg where
  eval vdefg env = do
    refs <- evalVdefg vdefg env
    case refs of
      [] -> return . Wrong $ "The impossible happened"
      [single_ref@(_,address)] -> eval address env
      _ -> do
        vals <- mapM (\(_,address) -> eval address env) refs
        let mkList x = [x]
        return . MkListOfValues $ zip (map mkList ['a'..'z']) vals 
  
--instance Evaluable ModuleFunction where
  --eval (ModuleFunction vdef m@(Module mname tdefs vdefgs)) env = evalVdef
  -- eval (ModuleFunction vdefg m@(Module mname tdefs vdefgs)) env = 
  --   case vdefg of
  --     one_def@(Nonrec vdef) -> do
  --       [hr@(_,address)] <- evalVdefg one_def env -- this pattern match should always be error-free
  --       eval address env
        
  --     -- If the definition is recursive, fetch all the heap references
  --     -- and then look for the given function (variable) name 
  --     rdefs@(Rec defs) -> do
  --       debugM $ "Found recursive definition "
  --       --debugM $ "Vdefg: " ++ show vdefg
  --       heap_refs <- evalVdefg defs env 
  --       vals <- mapM (\(_,address) -> eval address env) heap_refs
  --      return . MkListOfValues $ zip (map vdefName defs) vals                    

-- | Given an environment, looks for the address in the heap, evals a thunk using the given environment if necessary to return a value
instance Evaluable HeapAddress where
  eval address env = do
    beVerboseM $ "Evaluable HeapAddress: " ++ show address
    eTnkVal <- lookupMem address
    -- if it is a thunk, eval and memorize in heap
    either (evalThnk address) return eTnkVal
    where
      -- evaluates a thunk and updates its value in the heap
      evalThnk :: HeapAddress -> Thunk -> IM Value 
      evalThnk address thunk = do
        val <- eval thunk env
        h <- gets heap
        watchReductionM $ "Storing value " ++ show val ++ " in " ++ show address
        io $ H.insert h address (Right val)
        return val
    
instance Evaluable Thunk where
  --eval :: Thunk -> Env -> IM Value
  eval (Thunk exp env) e = do -- TODO. To comment: Why we don't care about the second env?
    ti <- gets tab_indentation
    let ?tab_indentation = ti
    debugM $ "Evaluating thunk: " ++ showExp exp
    eval exp (env ++ e) -- (!) passing (e ++ env) instead produces an infinite loop

instance Evaluable Value where
--  eval :: Value -> Env -> IM Value
  eval e@(Wrong _) _ = return e
  eval (Pointer ptr) env = eval ptr env
  eval v env = return $ Wrong $ "Wrong Evaluable Value: " ++ show v
  
instance Evaluable Pointer where
--  eval :: Pointer -> Env -> IM Value
  eval (MkPointer address) env = do
    debugM "Evaluable Pointer"
    eval address env
  
-- | Given a Pointer HeapAddress, eval a Thunk if necessary to return a Value represented
-- by the address 
-- evalPointer :: Value -> Env -> IM Value
-- evalPointer (Pointer address) env = evalHeapAddress address env
-- evalPointer e@(Wrong s) _ = return e
-- evalPointer _ _ = return . Wrong $ "evalPointer: The impossible happened"

instance Evaluable Exp where
  -- Integer,Char construction
  eval (App (Dcon ((Just (M (P ("ghczmprim"),["GHC"],"Types"))),constr)) (Lit lit)) env = case constr of
    "Izh" -> eval lit []
    "Czh" -> eval lit []
    otherwise -> return . Wrong $ " Constructor " ++ constr ++ " is not yet implemented. Please submit a bug report"

  eval e@(App function_exp argument_exp) env = do
    ti <- gets tab_indentation
    let ?tab_indentation = ti
            
    f <- evalExpI function_exp env ("Evaluating function_exp " ++ showExp function_exp)
  
    -- if the argument is a variable that is already in the env, don't make a new reference  
    case argument_exp of
      (Var qvar) -> do
        qvar_val <- qualifiedVar qvar `lookupId` env
        case qvar_val of
          Right (Wrong _) -> mkRefAndApply f argument_exp -- not found, 
          whnf -> do
            debugM $ "NOT Skipping the making of reference, " ++ (qualifiedVar qvar) ++ " is already in env"
            mkRefAndApply f argument_exp -- not found, 
            --apply f (qualifiedVar qvar) env --don't create thunk for variables in scope
      _ -> mkRefAndApply f argument_exp
    where
    mkRefAndApply :: Value -> Exp -> IM Value
    mkRefAndApply f arg_exp = do
      debugM "Making a reference from/to the function argument "
      heap_reference@(id,arg_address) <- mkHeapReference arg_exp env
      debugM $ "Argument to function has reference: " ++ show heap_reference
  
      res <- apply f id (heap_reference:env) -- Note1: the address is paassed 
      return res

  -- | A function application which has the type annotation which we will essentially ignore.
  eval e@(Appt exp ty) env = do 
    ti <- gets tab_indentation  
    let ?tab_indentation = ti
    debugM $ showExp exp
    debugM $ showType ty
    case exp of
      (Var qvar) -> evalExpI exp env "Typed Var application "
      (Dcon qvar) -> evalExpI exp env $ "Typed Dcon application  " ++ qualifiedVar qvar
      _ -> evalExpI exp env "Typed application "

  eval (Var ((Just (M (P ("base"),["GHC"],"Base"))),"zd")) env = 
    let
      applyFun :: Value -> IM Value  
      applyFun (Fun f dsc) = return $ Fun f ("($) " ++ dsc)
      applyFun x = return . Wrong $ "($), Applying something that is not a function, namely: " ++ show x
      -- takes a function `f` and returns a function `g` that applies `f` to its argument 
      ap id e = evalId id e >>= applyFun  
    in return $ Fun ap "($) :: (a -> b) -> a -> b"

  -- lambda abstraction over types variables
  -- returns a Fun value
  eval e@(Lam (Vb (var_name,ty)) exp) env = do
    --whenFlag show_subexpressions $ indentExp e >>= \e -> debugM $ "Evaluating subexpression " ++ e
    debugM $ "Making function from lambda abstraction \\" ++ var_name ++ " :: " ++ showType ty ++ " -> exp "
  
    -- If we abstract over a variable with a free type variable  
    -- e.g. \zddNumTmp :: Num a 
    -- and we are applying it, we should assume some value before reducing its value
    
    case exp of
      App _ (Var x) -> if (qualifiedVar x == showType ty)
                       then debugM ("HOLA" ++ showType ty ++" . " ++ qualifiedVar x) >> return (Fun mkFun $ "\\" ++ var_name ++ " -> exp")
                       else debugM ("HOLA" ++ showType ty ++" . " ++ qualifiedVar x) >> return (Fun mkFun $ "\\" ++ var_name ++ " -> exp")
      exp' -> do
        ti <- gets tab_indentation
        let ?tab_indentation = ti
        return $ Fun mkFun $ "\\" ++ var_name ++ " -> exp"
    where  
    -- a function that receives an identifier that points to some address
    -- makes a pointer from the variable we are abstracting over to found the address
    -- and computes a value in a new constructed environment
      mkFun :: Id -> Env -> IM Value
      mkFun id env' = do      
        ptr <- getPointer id env'
        case ptr of
          Pointer ptr -> evalExpI exp ((var_name,ptr_address ptr):env) "Evaluating Lambda body (exp)"
          w@(Wrong _)  -> return w
        
  -- lambda abstraction over variables
  -- ignores the type argument, evaluating the expression
  eval e@(Lam (Tb (var_name,_)) exp) env = do
    whenFlag show_subexpressions $ indentExp e >>= \e -> debugM $ "Evaluating subexpression " ++ e
    debugM $ "Saving type "   ++ var_name ++ " as a free type var"
    free_type_ref <- memorize (Right . FreeTypeVariable $ var_name) var_name
    evalExpI exp (free_type_ref:env) "Evaluating lambda body (exp)"
      
  -- Qualified variables that should be in the environment
  eval e@(Var qvar) env = do
    maybePtr <- mkPointer (qualifiedVar qvar) env
    case maybePtr of
      Just ptr ->   eval ptr env
      Nothing -> return $ Wrong $ "Could not find var in env " ++ qualifiedVar qvar  
  
  eval (Dcon qcon) env = getPointer (qualifiedVar qcon) env >>= flip eval env

  -- Case of
  eval (Case exp vbind@(vbind_var,ty) gen_ty alts) env = do
    increaseIndentation
    heap_reference@(id,address) <- memorize (mkThunk exp env) vbind_var
    exp_value <- eval address (heap_reference:env)

    watchReductionM $ "\tDoing case analysis for " ++ show vbind_var
    maybeAlt <- findMatch exp_value alts
    let exp = maybeAlt >>= Just . altExp -- Maybe Exp
  
    --debugM $ "EXP: "++ show exp
    case maybeAlt of
      Just alt -> do -- a matched alternative was found, in case it's a constructor, bind its arguments
        let exp = altExp alt
      
        debugM "Making altEnv"
        alt_env <- mkAltEnv exp_value alt
        debugM $ "This is the alt env: " ++ show alt_env
        debugM "End making altEnv"
        -- TODO IN ENVIRONMENT when(isAcon alt) $ var_val `bindAltVars` alt
        ti <- gets tab_indentation
        let ?tab_indentation = ti
        debugM $ "Evaluating case exp = " ++ showExp exp
        res <- eval exp (alt_env ++ heap_reference:env)
        -- when(isAcon alt) $ deleteAltBindedVars alt
        return res
      _ -> return . Wrong $ "Unexhaustive pattern matching of " ++ vbind_var

  eval (Lit lit) _ = eval lit []

  eval (Let vdefg exp) env = do
    env' <- acknowledgeVdefg vdefg env
    eval exp (env' ++ env)
  
  -- | Otherwise
  eval otherExp _ = indentExp otherExp >>= \expStr -> return . Wrong $ " TODO: {" ++ expStr ++ "}\nPlease submit a bug report"

-- | Given an alternative and a value that matches the alternative,
-- binds the free variables in memory and returns a list of references (an environment)
mkAltEnv :: Value -> Alt -> IM Env
mkAltEnv v@(Num _) (Acon (_,"Izh") _ [(vbind_var,vbind_ty)] _) = 
  do
    -- bind a single number
    beVerboseM $ "Binding " ++ vbind_var ++ " to val " ++ show v
    heap_ref <- memorize (Right v) vbind_var
    return [heap_ref]
  
mkAltEnv (Num n) (Acon q tbinds vbinds exp) = (debugM  $ " !!! WTF" ++ show q) >> return []
  
mkAltEnv (TyConApp (MkDataCon _ _) pointers) (Acon _ _ vbinds _) = do
  debugM ("pointers: " ++ show pointers)
  debugM ("Vbinds :: " ++ show vbinds)
  if (length pointers /= length vbinds)
  then error "length vals /= length vbinds"
  else do
     debugM "CAFE" 
     -- get vals and memorize that
     let ids = (map fst vbinds)
     return $ ids `zip` (map ptr_address pointers)  
--     mapM (uncurry memorize) (addresses `zip` (map fst vbinds))
mkAltEnv val alt = do
  debugM $ " (!!!) Alert, mkAltEnv, val= " ++ show val ++ "\nalt= " ++ show alt
  debugM $ "Returning empty env (didn't bind anything)"
  return []


-- | Tries to find an alternative that matches a value. It returns the first match, if any.
-- According to [GHC](http://hackage.haskell.org/trac/ghc/wiki/Commentary/Compiler/CoreSynType)
-- the DEFAULT alternative must come first, then constructors, then lits
findMatch :: Value -> [Alt] -> IM (Maybe Alt)
findMatch val [] = return Nothing -- no alt
findMatch val (def_alt@(Adefault exp):alts) = 
  do 
    -- a default alt, if none of the remaining alternatives match, match always with this alternative
    otherMatch <- findMatch val alts
    case otherMatch of
      found@(Just alt) -> return found
      _ -> return . Just $ def_alt
      
findMatch val (a:alts) = do
  ti <- gets tab_indentation
  let ?tab_indentation = ti
  debugM $ "Comparing " ++ show val ++ " and " ++ showAlt a
  matches <- val `matches` a
  
  if (not matches)
  then val `findMatch` alts   
  else do
    debugM $ "Found case match for" ++ show val
    return . Just $ a

matches :: Value -> Alt -> IM Bool

-- data
matches (TyConApp (MkDataCon n _) _) (Acon qual_dcon _ _ _) = return $ qualifiedVar qual_dcon == n
--matches val (Alit lit exp) = return False --TODO

matches val (Adefault _) = return True -- this is the default case, i.e. "_ -> exp " 

-- primitives
matches (Num n) (Acon qdcon _ _ _) = do
  watchReductionM $ "Trying to match a Num value (" ++ show n ++ ") with the type constructed by " ++ qualifiedVar qdcon    
  let matches' = qualifiedVar qdcon == "ghc-prim:GHC.Types.I#"   
  watchReductionM $ "\t.. " ++ if matches' then " matches" else " does not match"
  return matches'  

-- lits
matches (Rat n) (Alit (Literal (Lrational r) _) exp) | n == r = return True  
matches (Char c) (Alit (Literal (Lchar c2) _) exp) | c == c2 = return True
matches (String s) (Alit (Literal (Lstring s2) _) _) | s == s2 = return True

matches (Num n) (Alit (Literal (Lint i) _) exp) | n == i = return True
matches (Num n) (Alit (Literal (Lint i) _) exp) | otherwise = return False

matches (Boolean False) (Acon qdcon _ _ _) = return $ qualifiedVar qdcon == "ghc-prim:GHC.Types.False"
matches (Boolean True) (Acon qdcon _ _ _) = return $ qualifiedVar qdcon == "ghc-prim:GHC.Types.True"

-- We keep a String value as a separate data type, but in Haskell it is a list of chars
matches (String _) (Acon (Just (M (P ("ghczmprim"),["GHC"],"Types")),"ZMZN") _ _ _) = return True  
matches (String _) (Acon (Just (M (P ("ghczmprim"),["GHC"],"Types")),"ZC") tbinds vbinds exp) = return True

matches e@(Wrong s) _ = return False

--match against list cons

val `matches` alt = do
  ti <- gets tab_indentation
  let ?tab_indentation = ti
  io . putStrLn $ "??? Matching " ++ show val ++ " with " ++ show alt
--  io . putStrLn $ 
  return False
  
altExp :: Alt -> Exp
altExp (Acon _ _ _ exp) = exp
altExp (Alit _ exp) = exp
altExp (Adefault exp) = exp

evalFails :: String -> IM (Either Thunk Value)
evalFails = return . Right . Wrong
  
-- | Does the same as evalExp but indents and deindents for debugging output purposes
evalExpI :: Exp -> Env -> String -> IM Value
evalExpI exp env desc = do 
  ti <- gets tab_indentation
  let ?tab_indentation = ti
  
  debugMStep $ desc ++ " {"
  debugSubexpression exp 
  increaseIndentation  
  
  res <- eval exp env
  debugM $ "evalExpI#res: " ++ show res
  decreaseIndentation
  debugM $ "}" -- debugMStepEnd
  
  return res
  
-- | Looks for the address in the heap, evals a thunk if necessary to return a value
evalId :: Id -> Env -> IM Value
--evalId i e = lookupId i e >>= either (evalThunk e) return
evalId i e = do
  debugM $ "Evaluating variable " ++ i
  ptr <- getPointer i e 
  case ptr of
    e@(Wrong s) -> return e -- i was not found in env
    Pointer (MkPointer heap_address) -> do -- we know something about i in env
      eTnkVal <- lookupMem heap_address
      --debugM $ "lookupId " ++ i ++ " = " ++ show eTnkVal
      whnf <- case eTnkVal of  -- evaluate to weak head normal form
        Left thunk -> do
          val <- eval thunk e
          h <- gets heap
          io $ H.insert h heap_address (Right val)
          return val
        Right val -> return val  -- it is already in weak head normal form
      debugM (show i ++ " ~> " ++ show whnf)
      return whnf
  
-- | Function application
apply :: Value -> Id -> Env -> IM Value
apply (Fun f d) id env = do
  watchReductionM $ "applying " ++ d ++ " to " ++ id
  res <- f id env
  watchReductionM $ "apply " ++ d ++ " to " ++ id ++ " => " ++ show res   
  return res
    
-- Applies a (possibly applied) type constructor that expects appliedValue of type ty.
-- The type constructor that we are applying has |vals| applied values
apply (TyConApp tycon addresses) id env =  do 
    addr <- getPointer id env
    case addr of
      Pointer p -> return $ TyConApp tycon (addresses ++ [p])
      e@(Wrong s) -> return e
      
apply w@(Wrong _) _ _ = return w
apply f x _ = return . Wrong $ "Applying " ++ show f ++ " with argument " ++ show x

-- evalVdefgBenchmark :: Vdefg -> Env -> IM [HeapReference]
-- evalVdefgBenchmark vdefg env = do
--   debugM $ "doEvalVdefg; env.size == " ++ (show . length $ env)
--   beforeTime <- liftIO getCurrentTime
--   h <- gets heap  
      
--   heap_refs <- evalVdefg vdefg env
  
--   mapM_ (benchmark beforeTime) heap_refs  
--   debugM $ "doEvalVdefg.heap_refs: " ++ show heap_refs
--   debugM $ "doEvalVdefg.heap_refs.size: " ++ show (length heap_refs)
--   return heap_refs
  
--   where
--     benchmark :: UTCTime -> HeapReference -> IM ()
--     benchmark before heapRef@(id,heap_address) = do      
--       sttgs <- gets settings
--       res <- lookupMem heap_address
--       afterTime <- liftIO getCurrentTime
--       let 
--         time = afterTime `diffUTCTime` before
      
--         -- TODO: replace with new flag --benchmark
--         should_print = debug sttgs && show_tmp_variables sttgs
--                        || debug sttgs && show_tmp_variables sttgs
                     
--       (when should_print) $ do
--         debugM $ "Evaluation of " ++ id
--         debugM $ "\t.. done in " ++ show time ++ "\n\t.. and resulted in " ++ show res      


-- | Evaluate a value definition, which can be either recursive or not recursive

-- | Returns either one heap reference when the definition is non recursive
-- or a list of these when it is recursive
evalVdefg :: Vdefg -> Env -> IM [HeapReference]
evalVdefg (Rec vdefs) env = mapM (flip evalVdefg env . Nonrec) vdefs >>= return . concat
  
evalVdefg (Nonrec (Vdef (qvar, ty, exp))) env = do
  debugMStep $ "Evaluating value definition: " ++ qualifiedVar qvar
  debugM $ "doEvalVdefg; env.size == " ++ (show . length $ env)
  whenFlag show_expressions $ indentExp exp >>= debugM . (++) "Expression: "
  
  increaseIndentation
  res <- eval exp env  -- result **** 
  decreaseIndentation

  heap_ref <- memorize (mkVal res) (qualifiedVar qvar) -- ***
  return [heap_ref]
 
