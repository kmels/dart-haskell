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

--------------------------------------------------------------------------------
-- DART
import           DART.CaseAnalysis.PredicateBranch
--------------------------------------------------------------------------------
-- System
import           Data.Time.Clock
import           Data.Time.Clock(getCurrentTime,diffUTCTime)

import           DART.CmdLine
import           DART.Compiler.JIT(jitCompile)
import           DART.Util.StringUtils(separateWithNewLines)
import qualified Data.HashTable.IO as H
import           Data.List(find,inits)

--------------------------------------------------------------------------------
-- Language Core
import           Language.Core.Interpreter.Acknowledge
import           Language.Core.Interpreter.Apply
import           Language.Core.Interpreter.CaseAnalysis
import           Language.Core.Interpreter.Structures
import           Language.Core.Module
import           Language.Core.Vdefg (findVdefByName,vdefgNames,vdefName)

class Evaluable a where
  eval :: a -> Env -> IM Value
  
instance Evaluable Lit where
  eval l@(Literal coreLit ty) _ = case showExtCoreType ty of
    "Tcon(ghc-prim:GHC.Prim.Int#)" -> let (Lint i) = coreLit in return . Num $ i 
    "Tcon(integer-gmp:GHC.Integer.Type.Integer)" -> let (Lint i) = coreLit in return . Num $ i 
    "Tcon(ghc-prim:GHC.Prim.Char#)" -> case coreLit of
      (Lchar c) -> return . Char $ c  
      (Lint i) -> return . Num $ i 
    "Tcon(ghc-prim:GHC.Prim.Addr#)" -> let (Lstring s) = coreLit in return . String $ s
    --"Rational" -> return . Rat $ r
    _ -> return . Wrong $ "Could not evaluate literal of type " ++ showExtCoreType ty

-- | If the user provides an expression to evaluate, it can be either a haskell expression
-- that we can compile just in time, or a function name that is defined in the module
instance Evaluable HaskellExpression where
  eval hs@(HaskellExpression expression_string m@(Module mname tdefs vdefgs)) env = 
    -- | Is it a function defined within the module?
    case (m `moduleFindVdefByName` expression_string) of
      Just vdef -> do
        watchReductionM $ "Found a definition of " ++ expression_string
        eval (Nonrec vdef) env
      Nothing -> do
        debugM $ "Did not found any function named " ++ expression_string
        let 
          fnames = concatMap vdefgNames vdefgs -- [String]
          fnames_vdefgs = zip fnames vdefgs  -- [(String,Vdefg)]
                  
        jitMaybeVdef <- jitCompile hs
        
        case jitMaybeVdef of
          Just vdefg -> eval vdefg env
          Nothing -> return . Wrong $  "Could not evaluate " ++ expression_string ++ " in " ++ show mname
      
instance Evaluable Id where
  eval id env = evalId id env
  
-- | Looks for the address in the heap, evals a thunk if necessary to return a value
evalId :: Id -> Env -> IM Value
--evalId i e = lookupId i e >>= either (evalThunk e) return
evalId i e = do
  watchReductionM $ "Evaluating variable " ++ i
  ptr <- getPointer i e 
  case ptr of
    e@(Wrong s) -> return e -- i was not found in env
    Pointer (MkPointer heap_address) -> do -- we know something about i in env
      eTnkVal <- lookupMem heap_address
      whnf <- case eTnkVal of  -- evaluate to weak head normal form
        Left thunk -> do
          val <- eval thunk e
          h <- gets heap
          io $ H.insert h heap_address (Right val)
          return val
        Right val -> return val  -- it is already in weak head normal form
      watchReductionM (show i ++ " ~> " ++ show whnf)
      return whnf
      
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

-- | Given an environment, looks for the address in the heap, evals a thunk using the given environment if necessary to return a value
instance Evaluable HeapAddress where
  eval address env = do
    beVerboseM $ "accessing address " ++ show address
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
        
    watchReductionM $ "Evaluating thunk: " ++ showExp exp
    
    reached_timeout <- isTimeout
        
    case reached_timeout of
      False -> eval exp (env ++ e) -- (!) passing (e ++ env) instead produces an infinite loop
      True -> do
        max_secs <- gets settings >>= return . show . do_timeout_after_seconds
        clearTimeout
        return . Wrong $ "Reached timeout of " ++ max_secs ++ " seconds"

instance Evaluable Value where
--  eval :: Value -> Env -> IM Value
  eval e@(Wrong _) _ = return e
  eval (Pointer ptr) env = eval ptr env
  eval v env = return $ Wrong $ "Wrong Evaluable Value: " ++ show v
  
instance Evaluable Pointer where
  eval (MkPointer address) env = eval address env

instance Evaluable Exp where
  -- Integer,Char construction -- match against Qual for the benefit of speed
  eval (App (Dcon ((Just (M (P ("ghczmprim"),["GHC"],"Types"))),constr)) (Lit lit)) env = case constr of
    "Izh" -> eval lit []
    "Czh" -> eval lit []
    otherwise -> return . Wrong $ " Constructor " ++ constr ++ " is not yet implemented. Please submit a bug report"

  -- f :: a -> b
  -- x :: a
  -- => applied_types = [a,b]
  eval e@(App f_exp arg_exp) env = do
    ti <- gets tab_indentation  
    let ?tab_indentation = ti
    
    f_val <- evalExpI f_exp env ("Reducing lambda " ++ showExp f_exp)
  
    -- if the argument is already in env, don't make another reference
    case arg_exp of
      (Var qvar) -> do
        arg_val <- zDecodeQualified qvar `lookupId` env
        case arg_val of
          Right (Wrong _) -> mkRefAndApply f_val arg_exp -- not found,           
          whnf -> do 
            beVerboseM $ "not making a new reference, " ++ (zDecodeQualified qvar) ++ " is already in env"
            mkRefAndApply f_val arg_exp
            --apply f (qualifiedVar qvar) env --don't create thunk for variables in scope
      _ -> mkRefAndApply f_val arg_exp
    where
    mkRefAndApply :: Value -> Exp -> IM Value
    mkRefAndApply f arg_exp = do
      beVerboseM "Making a reference from/to the function argument "
      heap_reference@(id,_) <- mkHeapReference arg_exp env
      watchReductionM $ "Argument saved as " ++ id
      
      res <- apply f id (heap_reference:env) -- the address is passed 
      return res

  -- | A typed function (or data) application
  eval e@(Appt exp ty) env = do 
    ti <- gets tab_indentation  
    let ?tab_indentation = ti
    case exp of
      (Var qvar) -> evalExpI exp env "Typed Var application "
      (Dcon qvar) -> do
      
        v <- evalExpI exp env $ "Application of typed data constructor \"" ++ zDecodeQualified qvar ++ "\" with type = " ++ showType ty
        
        case v of
          -- if data constructor expects a polymorphic type, instance the type variable.
          (TyCon 
            tycon@(MkDataCon id 
                   datacon_sig@(Tvar(poly_var):ts) 
                   applied_types')
            tyname) -> do
            
            watchReductionM $ "Instancing type \"" ++ poly_var ++ "\" to " ++ (showType ty)
            let
              -- convert `Tvar(poly_var)` to `ty`
              mapSigTy :: Ty -> Ty
              mapSigTy poly_ty@(Tvar poly_var')  | poly_var' == poly_var = ty
                                                 | otherwise = poly_ty
              mapSigTy sigty = sigty
              
            return $ TyCon (tycon { signature = map mapSigTy datacon_sig,
                                    applied_types = (ty:applied_types')})
                     tyname
            
          tyconv@(TyCon tycon@(MkDataCon id (t:ts) applied_types') tyname) -> do
            watchReductionM $ "Applied types: " ++ (show applied_types')
            watchReductionM $ "Creating annotated type constructor from " ++ (show t) ++ " to " ++ (tyname)
            return $ TyCon (tycon { signature = ts, applied_types = (ty:applied_types')}) tyname
          
          --tyconapp@(TyConApp _ _) -> return tyconapp
          
          otherwise -> return $ Wrong $ "The impossible happened: Typed application was expecting a type constructor or an applied type constructor, but got: " ++ show otherwise
      _ -> evalExpI exp env "Typed application "

  -- ($) :: (a -> b) -> a -> b
  eval (Var ((Just (M (P ("base"),["GHC"],"Base"))),"zd")) env = 
    let
      applyFun :: Value -> IM Value  
      applyFun (Fun f dsc) = do return $ Fun f ("($) " ++ dsc)
      applyFun x = return . Wrong $ "($), Applying something that is not a function, namely: " ++ show x
      -- takes a function `f` and returns a function `g` that applies `f` to its argument 
      ap id e = evalId id e >>= applyFun  
    in return $ Fun ap "($) :: (a -> b) -> a -> b"

  -- lambda abstraction over types variables
  -- returns a Fun value
  eval e@(Lam (Vb (var_name,ty)) exp) env = do
    --whenFlag show_subexpressions $ indentExp e >>= \e -> debugM $ "Evaluating subexpression " ++ e
    watchReductionM $ "Making function from lambda abstraction \\" ++ var_name ++ " :: " ++ showType ty ++ " -> exp "
  
    -- If we abstract over a variable with a free type variable  
    -- e.g. \zddNumTmp :: Num a 
    -- and we are applying it, we should assume some value before reducing its value
    
    case exp of
      App _ (Var x) -> if (zDecodeQualified x == showType ty)
                       then return (Fun mkFun $ "\\" ++ var_name ++ " -> exp")
                       else return (Fun mkFun $ "\\" ++ var_name ++ " -> exp")
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
        
  -- lambda abstraction over a type variable
  eval e@(Lam (Tb (var_name,_)) exp) env = do
    whenFlag show_subexpressions $ indentExp e >>= \e -> debugM $ "Evaluating subexpression " ++ e
    watchReductionM $ "Saving type "   ++ var_name ++ " as a free type var"
    free_type_ref <- memorize (Right . FreeTypeVariable $ var_name) var_name
    evalExpI exp (free_type_ref:env) "Evaluating lambda body (exp)"
      
  -- Qualified variables that should be in the environment
  eval e@(Var qvar) env = do
    maybePtr <- mkPointer (zDecodeQualified qvar) env
    case maybePtr of
      Just ptr ->   eval ptr env
      Nothing -> return $ Wrong $ "Could not find var in env " ++ zDecodeQualified qvar ++ ".."
  
  eval (Dcon qcon) env = getPointer (zDecodeQualified qcon) env >>= flip eval env

  -- Case of
  eval (Case case_exp vbind@(vbind_var,ty) gen_ty alts) env = analyzeCase case_exp vbind env alts >>= evalAnalysis
    where
      evalAnalysis :: CaseAnalysis -> IM Value
      evalAnalysis (CaseAnalysisResult exp matched_alt exp_ref exp_value) = do
        case matched_alt of
          -- If a matched alternative was not found, check if an error has happened, and propagate that. If there is no error and there is no matched alternative, there is an unexhaustive pattern matching error
          Nothing -> return $ case exp_value of 
              err@(Wrong msg) -> err
              _ -> Wrong $ "Unexhaustive pattern matching of " ++ vbind_var              
          -- if a pattern matched against the expression value, we should evaluate the expression body to which that pattern corresponds
          Just matched_pattern -> do
            recordBranch case_exp exp_value env
            
            -- bind those free variables contained in the matched alternative pattern
            watchReductionM $ "Binding free variables contained in the matched pattern"
            free_vars_env <- mkAltEnv exp_value matched_pattern
            eval (patternExpression matched_pattern) (exp_ref:(free_vars_env ++ env))    

  eval (Lit lit) _ = eval lit []

  eval (Let vdefg exp) env = do
    env' <- acknowledgeVdefg vdefg env
    eval exp (env' ++ env)
  
  -- | Otherwise
  eval otherExp _ = indentExp otherExp >>= \expStr -> return . Wrong $ " TODO: {" ++ expStr ++ "}\nPlease submit a bug report"

-- | Given a case analysis expression, build an analysis data type that contains
-- about the execution of pattern matching
analyzeCase :: Exp -> Vbind -> Env -> [Alt] -> IM CaseAnalysis
analyzeCase case_exp (var_to_bind,_) env alts = do
  watchSMT $ "\tDoing case analysis for " ++ show var_to_bind
  
  -- bind the exp to the var_to_bind in the heap, it might be used within the scope of the alternative's expression body
  heap_reference@(id,address) <- memorize (mkThunk case_exp env) var_to_bind
  exp_value <- eval address (heap_reference:env)
  
  matched_alt <- findMatch exp_value alts
  
  return $ CaseAnalysisResult {
    analysis_expression = case_exp,
    matched_alternative = matched_alt,
    expression_ref = heap_reference,
    expression_value = exp_value
    }
    
-- | Given an alternative and a value that matches the alternative,
-- binds the free variables in memory and returns them in an environment
mkAltEnv :: Value -> Alt -> IM Env
mkAltEnv v@(Num _) (Acon (_,"Izh") _ [(vbind_var,vbind_ty)] _) = 
  do -- bind a single number    
    beVerboseM $ "Binding " ++ vbind_var ++ " to val " ++ show v
    heap_ref <- memorize (Right v) vbind_var
    return [heap_ref]
  
mkAltEnv (Num n) (Acon q tbinds vbinds exp) = (debugM  $ "what is this number @mkAltEnv???" ++ show q) >> return []
  
mkAltEnv (TyConApp (MkDataCon id _ _) pointers) (Acon _ _ vbinds _) = do
  if (length pointers < length vbinds)
  then do
   let
     evalPtr = flip eval []
     vbindss = show vbinds
     showLength = show . length
   vals <- mapM evalPtr pointers
   error $ "The impossible happened @mkAltEnv, length vals (" ++ (show . length $ pointers) ++ ") /= length vbinds (" ++ (show . length $ vbinds) ++ ") " ++ ", for type constructor" ++ id ++ "\n\tvals: " ++ (show vals) ++ "\n\tvbinds: " ++ vbindss
  else do
     -- get vals and memorize that
     let ids = (map fst vbinds)
     return $ ids `zip` (map ptr_address pointers)  
--     mapM (uncurry memorize) (addresses `zip` (map fst vbinds))
mkAltEnv val alt = do
  watchReductionM $ "Returning empty env (didn't bind anything)"
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
  watchReductionM $ "Comparing " ++ show val ++ " and " ++ showAlt a
  matches <- val `matches` a
  
  if (not matches)
  then val `findMatch` alts   
  else do
    watchReductionM $ "Found case match for" ++ show val
    return . Just $ a

matches :: Value -> Alt -> IM Bool

-- data
matches (TyConApp (MkDataCon n _ _) _) (Acon qual_dcon _ _ _) = return $ zDecodeQualified qual_dcon == n
matches (TyCon (MkDataCon n _ _) _) (Acon qual_dcon _ _ _) = return $ zDecodeQualified qual_dcon == n
--matches val (Alit lit exp) = return False --TODO

matches val (Adefault _) = return True -- this is the default case, i.e. "_ -> exp " 

-- primitives
matches (Num n) (Acon qdcon _ _ _) = do
  watchReductionM $ "Trying to match a Num value (" ++ show n ++ ") with the type constructed by " ++ zDecodeQualified qdcon    
  let matches' = zDecodeQualified qdcon == "ghc-prim:GHC.Types.I#"   
  watchReductionM $ "\t.. " ++ if matches' then " matches" else " does not match"
  return matches'  

-- lits
matches (Rat n) (Alit (Literal (Lrational r) _) exp) | n == r = return True  
matches (Char c) (Alit (Literal (Lchar c2) _) exp) | c == c2 = return True
matches (String s) (Alit (Literal (Lstring s2) _) _) | s == s2 = return True

matches (Num n) (Alit (Literal (Lint i) _) exp) | n == i = return True
matches (Num n) (Alit (Literal (Lint i) _) exp) | otherwise = return False

matches (Boolean False) (Acon qdcon _ _ _) = return $ zDecodeQualified qdcon == "ghc-prim:GHC.Types.False"
matches (Boolean True) (Acon qdcon _ _ _) = return $ zDecodeQualified qdcon == "ghc-prim:GHC.Types.True"

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
  
patternExpression :: Alt -> Exp
patternExpression (Acon _ _ _ exp) = exp
patternExpression (Alit _ exp) = exp
patternExpression (Adefault exp) = exp

evalFails :: String -> IM (Either Thunk Value)
evalFails = return . Right . Wrong
  
-- | Does the same as evalExp but indents and deindents for debugging output purposes
evalExpI :: Exp -> Env -> String -> IM Value
evalExpI exp env desc = do 
  ti <- gets tab_indentation
  let ?tab_indentation = ti
  
  watchReductionM $ desc ++ " => {"
  debugSubexpression exp 
  increaseIndentation  
  
  res <- eval exp env
  watchReductionM $ "=> " ++ show res
  decreaseIndentation
  watchReductionM $ "}" -- debugMStepEnd
  
  return res
  
-- | Function application
apply :: Value -> Id -> Env -> IM Value
apply (Fun f d) id env = do
  watchReductionM $ "applying function " ++ d ++ " to " ++ id
  res <- f id env
  watchReductionM $ "apply " ++ d ++ " to " ++ id ++ " => " ++ show res   
  return res
    
-- Applies a (possibly applied) type constructor that expects appliedValue of type ty.
-- The type constructor that we are applying has |vals| applied values
apply (TyConApp tycon@(MkDataCon datacon_name' signature' applied_types') addresses ) id env = do 
    addr <- getPointer id env
    case addr of
      Pointer p -> case signature' of
        (sig_head:sig_tail) -> do
          watchReductionM $ "Reducing type constructor by " ++ show (sig_head)
          return $ TyConApp tycon { signature = sig_tail } (addresses ++ [p])
        [] -> do
          watchReductionM $ "Type constructor's signature has no types left that require reduction." 
          return $ TyConApp tycon { signature = [] } (addresses ++ [p])
      e@(Wrong s) -> return e

--apply tca@(TyConApp tycon@(MkDataCon _ ts) addresses) id env = return . Wrong $ "Applying " ++ (show . length) ts ++ " with argument " ++ show id
            
apply (TyCon tycon@(MkDataCon _ (t:ts) applied_types') _) id env = do
  addr <- getPointer id env
  case addr of
    Pointer p -> return $ TyConApp (tycon { signature = ts }) ([p])
    e@(Wrong s) -> return e
      
apply w@(Wrong _) _ env = return w

apply f x env = do
  xstr <- evalId x env
  return . Wrong $ "Applying " ++ show f ++ " with argument " ++ (show xstr)

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
  debugMStep $ "Evaluating value definition: " ++ zDecodeQualified qvar  
  whenFlag show_expressions $ indentExp exp >>= debugM . (++) "Expression: "
  
  increaseIndentation
  io . putStrLn $ "Callng benchmarkIM"
  res <- benchmarkIM (zDecodeQualified qvar) $ eval exp env  -- result **** 
  decreaseIndentation

  heap_ref <- memorize (mkVal res) (zDecodeQualified qvar) -- ***
  return [heap_ref]
 
-- | If the flag --benchmark was provided, the given computation is
-- then benchmarked
benchmarkIM :: Id -> IM a -> IM a
benchmarkIM id computation = do
  before <- liftIO getCurrentTime
  computed <- computation
  after <- liftIO getCurrentTime
  
  -- should we add the benchmark?
  whenFlag benchmark $ modify $ addBenchmark id (after `diffUTCTime` before)  
    
  return computed
  where
    addBenchmark :: Id -> NominalDiffTime -> DARTState -> DARTState
    addBenchmark id difftime st = st {
      benchmarks = (id,difftime):(benchmarks st)
    }
    

  
