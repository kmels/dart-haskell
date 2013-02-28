{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

----------------------------------------------------------------------------
-- |
-- Module      :  Language.Core.Util
-- Copyright   :  (c) Carlos López-Camey, University of Freiburg
-- License     :  BSD-3
--
-- Maintainer  :  c.lopez@kmels.net
-- Stability   :  stable
--
--
-- Helper function on External Core
----------------------------------------------------------------------------- 

module Language.Core.Util where

import Language.Core.Core

-- | Pretty print a Ty (from extcore) where a function from [Int] to Int is printed as:
-- `ghc-prim:GHC.Prim.(->)ghc-prim:GHC.Types.[]ghc-prim:GHC.Types.Intghc-prim:GHC.Types.Int`
-- This representation is used to reify types thorugh Language.Core.TypeExtractor

showExtCoreType :: Ty -> String
showExtCoreType (Tvar t) = t
showExtCoreType (Tcon (mname,t2)) = showMname mname ++ "." ++ t2
showExtCoreType (Tapp t1 t2) = showExtCoreType t1 ++ showExtCoreType t2
showExtCoreType _ = "UNKNOWN"

-- | Given a type string representation from ext core e.g. ghc-prim:GHC.Prim.(->) ghc-prim:GHC.Types.[] ghc-prim:GHC.Types.Int ghc-prim:GHC.Types.Int we transform it in a more readable string form e.g. [Int] -> Int

showType :: Ty -> String
showType (Tvar t) = t
-- constructors
showType (Tcon (Just (M ((P "ghczmprim"), ["GHC"], "Tuple")),"Z2T")) = "(,)"
showType (Tcon (Just (M ((P "ghczmprim"), ["GHC"], "Types")),"ZMZN")) = "[]"
-- a primitive type from GHC
showType (Tcon (Just (M ((P "ghczmprim"), ["GHC"], "Types")),primitiveType)) = primitiveType 
showType (Tcon (Just (M ((P "integerzmgmp"), ["GHC","Integer"], "Type")),"Integer")) = "Integer"
showType (Tcon (mname,t2)) = wrapName "Tcon" $ showMname mname ++ "." ++ t2
-- a type constructor applied to a type parameter e.g. a list
showType (Tapp tc@(Tcon _) innerType') = 
  let 
    constructor = showType tc
    innerType = showType innerType'
  in case constructor of
    "[]" -> if (innerType == "Char") then "String" else "[" ++ innerType ++ "]"
    _ -> constructor ++ " " ++ innerType
-- a type constructor applied to two type parameters e.g. a Tuple
showType (Tapp ta@(Tapp tc@(Tcon _) ty2) ty1) = let
  constructor = showType tc
  in case constructor of
    "(,)" -> "(" ++ showType ty1 ++ "," ++ showType ty2 ++ ")"
    _ -> constructor ++ " " ++ showType ty1 ++ " " ++ showType ty2
    
showType (Tapp t1 t2) = wrapName "Tapp" $ showType t1 ++ " " ++ showType t2
showType _ = "showType, TODO"

showMname :: Maybe AnMname -> String
showMname Nothing = ""
showMname (Just (M ((P packageName),[],s2))) = packageName ++ ":" ++ s2
showMname (Just (M ((P packageName),[s1],s2))) = packageName ++ ":" ++ s1 ++ "." ++ s2
showMname (Just (M ((P packageName),ss,s2))) = packageName ++ ":" ++ show ss ++ "." ++ s2

wrapName s r = s ++ "(" ++ r ++ ")"

showExp :: Exp -> String
showExp (Var (mname,variable)) = wrapName "var" $ showMname mname ++ "." ++ variable
showExp (Dcon (mname,dcon)) = wrapName "dataConstructor" $ showMname mname ++ "." ++ dcon
showExp (Lit lit) = showLit lit
showExp (App exp1 exp2) = wrapName "app" $ showExp exp1 ++ "," ++ showExp exp2
showExp (Appt exp typ) = wrapName "appt" $ showExp exp ++ "," ++ showType typ --e.g. >= Int
showExp (Lam bind exp) = "\n\t\\" ++ showBind bind ++ " -> " ++ showExp exp
showExp (Let vdefg exp) = wrapName "let" $ showVdefg vdefg ++ showExp exp
showExp (Case exp (vbind_var,vbind_ty) ty alts) = "\n\t\tcase \n\t\t\t" ++ showExp exp ++ "\n\tof " ++vbind_var ++ "::" ++ showType vbind_ty ++ ".." ++ showType ty ++ concatMap (\alt -> "\n\t" ++ showAlt alt) alts
showExp (Cast exp ty) = wrapName "case" $ showExp exp ++ showType ty
showExp (Note msg exp) = wrapName "note" $ msg ++ showExp exp
showExp (External str ty) = wrapName "external" $ str ++ showType ty
--showExp _ = "NOT IMPLEMENTED YET"

showCoreLit :: CoreLit -> String
showCoreLit (Lint i) = show i
showCoreLit (Lrational r) = show r
showCoreLit (Lchar c) = show c
showCoreLit (Lstring s) = s

showBind :: Bind -> String
showBind (Vb (var,ty)) = wrapName "Bind" $ var ++ "::" ++ showType ty
showBind (Tb tb@(tvar,kind)) = showTbind tb

showVdefg :: Vdefg -> String
showVdefg (Rec vdefs) = wrapName "Rec" $ concatMap showVdef vdefs
--showVdefg (Nonrec (Vdef ((mname,var),ty,exp) )) = "Nonrec\n\t\t..qual_mname: " ++ show mname ++ "\n\t\t..var: " ++ show var ++ "\n\t\t..ty: " ++ show ty ++ "\n\t\t..exp: " 
showVdefg (Nonrec vdef) = wrapName "Nonrec" $ showVdef vdef

showVdef :: Vdef -> String
showVdef (Vdef ((mname,var),ty,exp)) = wrapName "Vdef" $ showMname mname ++ var ++ showType ty ++ showExp exp

showAlt :: Alt -> String
showAlt (Adefault exp) = wrapName "Adefault" $ showExp exp
showAlt (Alit lit exp) = wrapName "ALit" $ showLit lit ++ showExp exp
showAlt (Acon (mname,dcon) tbinds vbinds exp) = wrapName "Acon" $ showMname mname ++ ", " ++ dcon ++ ", " ++ concatMap (\tb -> showTbind tb ++ ",") tbinds ++ concatMap (\vb ->showVbind vb ++ ",") vbinds ++ showExp exp

showTbind :: (Tvar,Kind) -> String
showTbind (tvar,kind) = tvar ++ " :: " ++ showKind kind

showVbind :: Vbind -> String
showVbind (var,ty) = wrapName "Vbind" $ var ++ "::" ++ showType ty

showKind :: Kind -> String
showKind Klifted = "Klifted"
showKind Kunlifted = "Kunlifted"
showKind Kopen = "Kopen"
showKind (Karrow k k') = wrapName "Karrow" $ showKind k ++ " -> " ++ showKind k'
showKind (Keq ty ty') = wrapName "Keq" $ showType ty ++ " -> " ++ showType ty'

showLit :: Lit -> String
showLit (Literal coreLit ty) = 
 let
   showCoreLit :: CoreLit -> String
   showCoreLit (Lint i) = show i
   showCoreLit (Lrational r) = show r
   showCoreLit (Lchar c) = show c
   showCoreLit (Lstring s) = show s
 in wrapName "lit" $ showCoreLit coreLit ++ "::" ++ showType ty

qualifiedVar :: Qual Var -> String
qualifiedVar (Nothing,var) = var
qualifiedVar (mname,var) = showMname mname ++ "." ++ var

bindId :: Bind -> Id
bindId (Vb (var,ty)) = var
bindId (Tb (tvar,kind)) = tvar

instance Show Cdef where
  show (Constr (_,dcon) tbinds types) = 
    let 
      tbinds' = if (not . null $ tbinds) 
                then " :: forall(?) " ++ concatMap (\tb -> showTbind tb) tbinds
                else " " 
      showType' (Tvar t) = t
      showType' (Tapp (Tcon (_,tcon)) (Tvar t)) = tcon ++ " " ++ t
      showType' (Tapp dcon@(Tapp dt tv) ty) = showType' dcon ++ " " ++ showType' ty
      showType' x = showType x
      types' = if (not . null $ types)
               then concatMap (\t -> showType' t ++ " -> ") (init types) ++ (showType' . last $ types)
               else ""
    in dcon ++ tbinds' ++ types'

instance Show Ty where show = showExtCoreType
                       
instance Show Kind where show = showKind
                         
{-instance Show Tdef where 
  show (Data qtcon@(_,tcon) tbinds cdefs) = (show . qualifiedVar) qtcon ++ " ..\n\tType parameters:\n" ++ tbinds' ++ "\tType constructors:\n" ++ cdefs' where
     tbinds' = concatMap (\tb -> "\t\t" ++ showTbind tb ++ "\n") tbinds
     cdefs' = concatMap (\cd -> "\t\t" ++ show cd ++ " -> " ++ tcon ++ "\n") cdefs
-}

