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
-- An interpreter for external core expressions
-----------------------------------------------------------------------------

module Language.Core.Interpreter where

import           Language.Core.Interpreter.Apply
import           Language.Core.Interpreter.Structures
  
import           Control.Applicative((<|>))
import qualified Data.HashTable.IO as H
import           Data.Maybe
import           Language.Core.Core
import qualified Language.Core.Interpreter.GHC.CString as GHC.CString
import qualified Language.Core.Interpreter.GHC.Classes as GHC.Classes
import qualified Language.Core.Interpreter.GHC.Num as GHC.Num
import qualified Language.Core.Interpreter.GHC.Tuple as GHC.Tuple
import qualified Language.Core.Interpreter.GHC.Types as GHC.Types
import           Language.Core.TypeExtractor(extractType)
import           Language.Core.TypeExtractor.DataTypes
import           Language.Core.Util(qualifiedVar,showVdefg,showType,showExtCoreType,showExp,bindId,showBind)
import           Language.Core.Vdefg (isTmp,vdefgId,vdefgName)

import           Control.Monad.State.Lazy
import           DART.CmdLine
import           DART.FileIO
import           Data.List(find)
import           Data.Time.Clock(getCurrentTime,diffUTCTime)
import           DART.InterpreterSettings
import           Text.Encoding.Z(zDecodeString)
{-Given a module which contains a list of value definitions, *vd*, evaluate every *vd* and return a heap with their interpreted values.

Value definition to mapped values
-----------------------------------------------------
| Value definition type              | mapped to    |
| Concrete type e.g. Int             | Num val      |
| Concrete type e.g. Int - Int      | ?            |
-----------------------------------------------------
-}

doEvalVdefg :: (?settings :: InterpreterSettings) => Vdefg -> IM Value
doEvalVdefg vdefg = do
  before <- liftIO getCurrentTime
  h <- get
  io . dodebug $ "Evaluating " ++ (vdefgId vdefg)
  res <- evalVdefg vdefg             
  after <- liftIO getCurrentTime
  let 
    id = vdefgId vdefg
    time = after `diffUTCTime` before    
    should_print = debug ?settings && show_tmp_variables ?settings
                   || debug ?settings && (not . show_tmp_variables $ ?settings) && (not $ isTmp vdefg)
  (when should_print) $ do
    io . dodebug $ "Evaluation of " ++ (vdefgId vdefg)
    io . dodebug $ "\t.. was done in " ++ show time ++ "\n\t.. and resulted in " ++ show res
  res `saveResultAs` id

evalModule :: (?settings :: InterpreterSettings) => Module -> IM Heap
evalModule m@(Module name tdefs vdefgs) = do
  acknowledgeTypes m
  mapM_ doEvalVdefg vdefgs
  h <- get
  return h

-- | Given a module and a function name, we evaluate the function in that module and return the heap. 

evalModuleFunction :: (?settings :: InterpreterSettings) => Module -> String -> IM Value
evalModuleFunction m@(Module mname tdefs vdefgs) fname = 
   if null fname then 
     error $ "evalModuleFunction: function name is empty" 
   else case maybeVdefg of
     Nothing -> return . Wrong $  "Could not find function " ++ fname ++ " in " ++ show mname
     Just vdefg -> do
       io . dodebug $ "Found definition of " ++ fname
       acknowledgeTypes m
       acknowledgeVdefgs m
       --let show_exps = ?show_expressions 
       --_ <- let ?show_expressions = False in evalModule m
       --let ?show_expressions = show_exps in doEvalVdefg vdefg
       doEvalVdefg vdefg
   where
     fnames = map vdefgName vdefgs -- [String]
     fnames_vdefgs = zip fnames vdefgs 
     maybeVdefg = find ((==) fname . fst) fnames_vdefgs >>= return . snd -- :: Maybe Vdefg

-- | Given a module, recognize type constructors and put them in the heap so that we can build values for custom types afterwards. 
     
acknowledgeTypes :: (?settings :: InterpreterSettings) => Module -> IM ()
acknowledgeTypes modl@(Module _ tdefs _) = mapM_ acknowledgeType tdefs
  
acknowledgeType :: (?settings :: InterpreterSettings) => Tdef -> IM ()
acknowledgeType tdef@(Data qdname@(_,dname) tbinds cdefs)  = do
  io . dodebug $ "Acknowledging type " ++ qualifiedVar qdname
  mapM_ insertTyCon cdefs where
    insertTyCon :: Cdef -> IM ()
    insertTyCon tcon@(Constr qcname tbinds' types) = do
      h <- get 
      let 
        tyConName = qualifiedVar qcname
        tyCons = TyCon $ AlgTyCon tyConName types
      tyCons `saveResultAs` tyConName
      return ()
    
-- | Given a module, recognize all of its value definitions, functions, and put them in the heap so that we can evaluate them when required. 
acknowledgeVdefgs :: (?settings :: InterpreterSettings) => Module -> IM ()
acknowledgeVdefgs m@(Module _ _ vdefgs) = mapM_ acknowledgeVdefg vdefgs

-- | Acknowledges value definitions
acknowledgeVdefg  :: Vdefg -> IM ()
acknowledgeVdefg (Nonrec vdef) = acknowledgeVdef vdef
acknowledgeVdefg (Rec vdefs) = mapM_ acknowledgeVdef vdefs

acknowledgeVdef :: Vdef -> IM ()  
acknowledgeVdef (Vdef (qvar, ty, exp)) = exp `saveThunkAs` (qualifiedVar qvar) >>= \_ -> return ()

{-evalTypeCon :: (?debug :: Bool, watch_reduction ?settings :: Bool) => String -> Cdef -> IM Value
evalTypeCon finalType (Constr qcname@(_,cname) typarams types) = do
  h <- get -- get the heap
  io . dodebug $ "Building constructor for " ++ cname
  constructor <- return . evalTypeCon' $ types
  io . dodebug $ "Built constructor value " ++ show constructor
  return constructor where 
    mkConDesc :: [Ty] -> String
    mkConDesc (ty:[]) = cname ++ " :: " ++ (showType ty) ++ " ->" ++ finalType
    mkConDesc (ty:tys)= cname ++ " :: " ++ (showType ty) ++ " ->" ++ show (Constr (Nothing,"") [] tys) ++ " -> " ++ finalType
    
    evalTypeCon' :: [Ty] -> Value -- TyCon-}
    
        
                                             
--The list of value definitions represents the environment
evalVdefg :: (?settings :: InterpreterSettings) => Vdefg -> IM Value
evalVdefg (Rec (v@(Vdef _):[]) ) = evalVdefg $ Nonrec $ v
-- More than one vdef? I haven't found a test case (TODO)
evalVdefg (Rec vdefs) = return . Wrong $ "TODO: Recursive eval not yet implemented\n\t" --  ++ concatMap (\(Vdef ((mname,var),ty,exp)) -> " VDEF; " ++ var ++ " :: " ++ showType ty ++ "\n\t"++ showExp exp) vdefs
--evalVdefg (Rec vdefs) = return . Wrong $ "TODO: Recursive eval not yet implemented for value definitions :\n" ++ concatMap (\(Vdef ((mname,var),ty,exp)) -> "\t" ++ var ++ " :: " ++ showType ty++ "\n\t\tExpression:" ++ showExp exp) vdefs

evalVdefg (Nonrec (Vdef (qvar, ty, exp))) = do
  whenFlag (show_expressions ?settings) $ do
    io . dodebug $ "\t.. expression: " ++ showExp exp ++ "\n"
  res <- evalExp exp -- result
  heap <- get 
  res `saveResultAs` (qualifiedVar qvar)

evalExp :: (?settings :: InterpreterSettings) => Exp -> IM Value

-- | This one is a function application which has the type accompanied. We won't care about the type now, as I'm not sure how it can be used now.
-- Appt is always (?) applied to App together with a var that represents the function call of the Appt. In the case of integer summation, this is base:GHC.Num.f#NumInt. That is why we have to ignore the first parameter when applied.
-- If Appt is being applied to another appt, then we ignore another level of parameters. This is the case of function application, namely ($) :: (a - b) - a - b

evalExp e@(Appt dc@(Dcon dcon) ty) = do
  heap <- get
  debugSubexpression dc
  f <- liftIO $ evalStateT (evalExp dc) heap
  -- if the type constructor has type parameters but has no type arguments
  -- e.g. as in Nil :: [a], we shall ignore the type parameter
  return $ case f of
    TyCon tc@(AlgTyCon id []) -> TyCon tc -- we don't expect any further arguments
    TyCon tc -> Fun (\g -> return $ TyConApp tc [g]) (show tc ++ showType ty)
    _ -> Fun (\g -> apply f g) $ "\\"++"g -> apply " ++ show f ++ " g"

evalExp e@(Appt fun ty) = do
  heap <- get
  debugSubexpression fun
  f <- liftIO $ evalStateT (evalExp fun) heap
  return $ Fun (\g -> apply f g) $ "\\"++"g -> apply " ++ show f ++ " g"
  

evalExp (Var ((Just (M (P ("base"),["GHC"],"Base"))),"zd")) = let
  ap f = Fun (\x -> apply f x) "GHC.Base.$ :: Fun(a - b)"
  in return $ Fun (\f -> return (ap f)) "$" -- GHC.Base.$ :: Fun(a - b) - Fun(a - b)

evalExp e@(Lam binded_var exp) = let  
   name = bindId binded_var             
   bindAndEval binded_value = do 
     --liftIO $ putStrLn $ "\t getting the heap"
     heap <- get
     -- TODO, this should be inserted in an environment instead and then be deleted, (gotta change the IM type). It is now added and deleted in the heap. This assumes External Core source code doesn't have any variables shadowed
     
     when(watch_reduction ?settings) $ 
       io . dodebug $ "\t binding " ++ name ++ " to " ++ show binded_value ++ " in the heap"
       
     liftIO $ H.insert heap name (Right binded_value)
     
     when(watch_reduction ?settings) $ 
       io . dodebug $ "\t evaluating lambda body (exp)"
       
     let ?settings = increase_tab_level ?settings
     res <- evalExp exp     
     let ?settings = decrease_tab_level ?settings
     
     when(watch_reduction ?settings) $ 
       io . dodebug $ "\t deleting binded value for " ++ bindId binded_var ++ " in the heap"
       
     liftIO $ H.delete heap name 
     return res
  in do
     when (show_subexpressions ?settings) $ 
       io . dodebug $ "Evaluating subexpression " ++ showExp e
     return $ Fun bindAndEval $ "\\" ++ bindId binded_var ++ " -> exp"

evalExp (App -- Integer,Char construction
          (Dcon ((Just (M (P ("ghczmprim"),["GHC"],"Types"))),constr))
          (Lit lit) 
         ) | constr == "Izh" = evalLit lit
           | constr == "Czh" = evalLit lit
           | otherwise = return . Wrong $ " Constructor " ++ constr ++ " is not yet implemented. Please submit a bug report"

evalExp e@(App function_exp argument_exp) = do 
  debugSubexpression e
  f <- evalExp function_exp
   --liftIO . putStrLn $ " f: " ++ showExp function_exp ++ " = " ++ show f
  x <- evalExp argument_exp
   --liftIO . putStrLn $ " x: " ++ showExp argument_exp ++ " = " ++ show x
  when(watch_reduction ?settings) $ 
    io . dodebug $ "\t Applying val " ++ show x ++ " to function " ++ show f
  res <- apply f x
   --liftIO . putStrLn $ "\t Applying f x  = " ++ show res
   
   --liftIO . putStrLn $ "Evaluating subexpression " ++ showExp e ++ " = " ++ show res
  return res

-- Variables 

evalExp e@(Var qvar) = debugSubexpression e >>= \_ -> evalVar . qualifiedVar $ qvar

-- Case of

evalExp (Case exp@(Var var) vbind@(vbind_var,_) _ alts) = do
  let qvar = qualifiedVar var
    
  when (watch_reduction ?settings) $ do
    io . dodebug $ "\tDoing case analysis for " ++ qvar
  
  let ?settings = increase_tab_level ?settings
  
  -- (too much) debugSubexpression exp 
  var_val <- evalVar qvar
  watchReduction $ "\t Trying  " ++ show var_val     
  vbind_var `bindTo` var_val
        
  watchReduction $ "\t Trying to match " ++ show var_val     
   
  maybeAlt <- findMatch var_val alts     
  let exp = maybeAlt >>= Just . altExp -- Maybe Exp
  
  case exp of
    Just e -> do -- a matched alternative was found, in case it's a constructor, bind its arguments
      let alt = fromJust $ maybeAlt
      when(isAcon alt) $ var_val `bindAltVars` alt
      res <- evalExp e
      when(isAcon alt) $ deleteAltBindedVars alt
      return res
    _ -> return . Wrong $ "Unexhaustive pattern matching of " ++ qvar

evalExp (Lit lit) = evalLit lit

-- Data constructors

evalExp (Dcon qcon) = evalVar . qualifiedVar $ qcon
evalExp otherExp = return . Wrong $ " TODO: " ++ showExp otherExp

-- | Binds variables to values in a case expression
bindAltVars :: (?settings :: InterpreterSettings) => Value -> Alt -> IM ()
bindAltVars (TyConApp (AlgTyCon _ _) vals) (Acon _ _ vbinds _) = 
  if (length vals /= length vbinds) 
  then error "length vals /= length vbinds"
  else mapM_ (uncurry bindTo) ((map fst vbinds) `zip` vals)
bindAltVars val@(Num n) (Acon _ _ [(var,_)] _) = var `bindTo` val
bindAltVars t v = io . putStrLn $ " Don't know how to bind values " ++ show t ++ " to " ++ show v
    
bindTo :: (?settings :: InterpreterSettings) => Id -> Value -> IM ()
bindTo i v = do
  when (watch_reduction ?settings) $ io . dodebug $ "Binding " ++ show i ++ " to " ++ show v              
  heap <- get
  io $ H.insert heap i (Right v)
              
deleteAltBindedVars :: Alt -> IM ()
deleteAltBindedVars (Acon _ _ vbinds _) = do 
  heap <- get
  mapM_ (io . H.delete heap . fst) vbinds
-- Otherwise

isAcon :: Alt -> Bool
isAcon (Acon _ _ _ _) = True
isAcon _ = False


-- | Tries to find an alternative that matches a value. It returns the first match, if any.
findMatch :: (?settings :: InterpreterSettings) => Value -> [Alt] -> IM (Maybe Alt)
findMatch val [] = return Nothing
findMatch val (a:alts) = do
  matches <- val `matches` a
  
  if (not matches)
  then val `findMatch` alts   
  else return . Just $ a 

matches :: (?settings :: InterpreterSettings) => Value -> Alt -> IM Bool
(TyConApp (AlgTyCon n _) vals) `matches` (Acon qual_dcon tbs vbs idx_exp) = do

  let tyconId = qualifiedVar qual_dcon
      matches' = tyconId == n
      
  when(watch_reduction ?settings) $ do
    io . dodebug $ "Trying to match value with type constructor " ++ tyconId
    io . dodebug $ "\t.. " ++ if matches' then " matches" else " does not match"
        
  return $ matches'
    
val `matches` (Alit lit exp) = return False --TODO
val `matches` (Adefault _) = return True -- this is the default case, i.e. "_ - " 
(Num n) `matches` (Acon qdcon _ _ _) = do
  when (watch_reduction ?settings) $ 
    io . dodebug $ "Trying to match a Num value (" ++ show n ++ ") with the type constructed by " ++ qualifiedVar qdcon
    
  let matches' = qualifiedVar qdcon == "ghc-prim:GHC.Types.I#" 
  
  when (watch_reduction ?settings) $ 
    io . dodebug $ "\t.. " ++ if matches' then " matches" else " does not match"

  return matches'
  
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

evalVar :: (?settings :: InterpreterSettings) => Id -> IM Value
evalVar id = lookupVar id >>= \v -> case v of
    Left (Thunk e) -> do      
      debugM $ "Variable " ++ id ++ " is a Thunk, evaluating"
      debugSubexpression e
      let ?settings = increase_tab_level ?settings
      r <- evalExp e
      let ?settings = decrease_tab_level ?settings
      return r
    Right v -> return $ case v of
      (TyCon tycon@(AlgTyCon n (a:args))) -> v -- TyCon $ AlgTyCon n [] -- take type arguments off
      _ -> v
    
lookupVar :: (?settings :: InterpreterSettings) => Id -> IM (Either Thunk Value)
lookupVar x = do
   debugM $ "Looking up var " ++ x
   env <- get
   val <- liftIO $ H.lookup env x -- looks for a variable in the heap
   lib_val <- liftIO $ H.lookup env xDecoded -- looks for a GHC lib variable in the heap    

   -- if val is Just a, return a. 
   -- Otherwise if lib_val is Just a, return a
   -- Otherwise, if lib_val is Nothing, val <|> lib_val is also Nothing, return fail
   -- 
   maybe fail return (val <|> lib_val) where
   
     fail :: IM (Either Thunk Value)
     fail = if (xDecoded /= x) 
            then -- it is z-decoded
              evalFails $ "Could not find " ++ xDecoded ++ " in the libraries"
            else 
              evalFails $ "Could not find " ++ x ++ " in the environment"
              
     xDecoded :: String
     xDecoded = zDecodeString x

-- | A sort of findMaybe and ($) i.e. it returns only one maybe, the first Just found by mapping the functions to qv, or Nothing.

callEvalVar :: [(Qual Var -> Maybe Value)] -> Qual Var -> Maybe Value
callEvalVar [] qv = Nothing
callEvalVar (eqv:eqvs) qv = 
   case eqv qv of
     v@(Just value) -> v
     _ -> callEvalVar eqvs qv

evalFails :: String -> IM (Either Thunk Value)
evalFails = return . Right . Wrong

saveResultAs :: Value -> Id -> IM Value
val `saveResultAs` qvar = do
  heap <- get
  io $ H.insert heap qvar (Right val)
  return val

saveThunkAs :: Exp -> Id -> IM Thunk
exp `saveThunkAs` qvar = do
  heap <- get
  io $ H.insert heap qvar (Left thunk)
  return thunk where
    thunk = Thunk exp

apply :: Value -> Value -> IM Value
apply fun@(Fun f _) v = f v
--apply tc@(AlgTyCon name []) v = return . Wrong $ "[], Applying " ++ show tc ++ " with argument " ++ show v
apply (TyCon tc@(AlgTyCon name (t:types))) v = return $ TyConApp tc' [v] where
  tc' :: TyCon
  tc' = AlgTyCon name types
apply (TyConApp tc@(AlgTyCon name (t:types)) vals) v = return $ TyConApp tc' (vals ++ [v]) where          
  tc' :: TyCon
  tc' = AlgTyCon name types
apply w@(Wrong _) _ = return w
apply f m = return . Wrong $ "Applying " ++ show f ++ " with argument " ++ show m

-- | List of library functions
libraries :: [(Id,Either Thunk Value)]
libraries = concat $ [--GHC.Num.all,
  --GHC.Classes.all,
  GHC.CString.all,
  GHC.Types.all
  , GHC.Tuple.all
  ]

-- | Loads nothing ATM, but it'll be useful
loadLibraries :: (?settings :: InterpreterSettings) => IM ()
loadLibraries = do
  -- loadLib "lib/GHC/Tuple.hs" -- fails due to builtin syntax but good idea
  h <- get
  mapM_ loadBinding libraries
  return ()
  where
    loadLib :: FilePath -> IM ()
    loadLib f = do
      io . dodebug $ "Loading " ++ f
      m <- io . readModule $ f
      acknowledgeTypes m
      
    loadBinding :: (Id,Either Thunk Value) -> IM ()
    loadBinding (id,val) = do
      h <- get 
      io . dodebug $ "Loading " ++ id
      io $ H.insert h id val
  
