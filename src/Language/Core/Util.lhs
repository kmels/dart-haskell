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

> module Language.Core.Util where

> import Language.Core.Core

> showExtCoreType :: Ty -> String
> showExtCoreType (Tvar t) = t
> showExtCoreType (Tcon (mname,t2)) = showMname mname ++ "." ++ t2
> showExtCoreType (Tapp t1 t2) = showExtCoreType t1 ++ showExtCoreType t2
> showExtCoreType _ = "UNKNOWN"

> showType :: Ty -> String
> showType (Tvar t) = wrapName "Tvar" t
> showType (Tcon (mname,t2)) = wrapName "Tcon" $ showMname mname ++ "." ++ t2

In order to pretty print types, we'll pattern match on the list type as a Ty

> showType (Tapp (Tcon (Just (M ((P "ghc-prim"), ["GHC"], "Types")),"[]")) (Tcon (Just (M ((P "ghc-prim"), ["GHC"], "Types")),"Char"))) = "[Char!!!!]"
> showType (Tapp (Tcon (Just (M ((P "ghc-prim"), ["GHC"], "Types")),"[]")) typ) = "[WTF???]" ++ showType typ
> showType (Tapp t1 t2) = wrapName "Tapp" $ showType t1 ++ "TIP2: " ++ showType t2

> showType _ = "NOT IMPLEMENTED YET!!!!!!"

> showMname :: Maybe AnMname -> String
> showMname Nothing = ""
> showMname (Just (M ((P "ghc-prim"),[s1],s2))) =  "WE FOUND" ++ s2
> showMname (Just (M ((P packageName),[s1],s2))) = if (packageName == "ghcprim") 
>                                                  then "GHC!!" ++ ":" ++ s1 ++ "." ++ s2
>                                                  else ">>>"++(show packageName) ++ "<<<:" ++ s1 ++ "." ++ s2

> wrapName s r = s ++ "(" ++ r ++ ")"

> showExp :: Exp -> String
> showExp (Var (mname,variable)) = wrapName "var" $ showMname mname ++ variable
> showExp (Dcon (mname,dcon)) = wrapName "dataConstructor" $ showMname mname ++ dcon ++")"
> showExp (Lit (Literal coreLit ty)) = wrapName "lit" $ showCoreLit coreLit ++ showType ty
> showExp (App exp1 exp2) = wrapName "app" $ showExp exp1 ++ showExp exp2
> showExp (Appt exp typ) = wrapName "appt" $ showExp exp ++ showType typ
> showExp (Lam bind exp) = wrapName "lam" $ showBind bind ++ showExp exp
> showExp (Let vdefg exp) = wrapName "let" $ showVdefg vdefg ++ showExp exp
> showExp (Case exp (vbind_var,vbind_ty) ty alts) = wrapName "case" $ showExp exp ++ vbind_var ++ showType vbind_ty ++ showType ty ++ concatMap showAlt alts
> showExp (Cast exp ty) = wrapName "case" $ showExp exp ++ showType ty
> showExp (Note msg exp) = wrapName "note" $ msg ++ showExp exp
> showExp (External str ty) = wrapName "external" $ str ++ showType ty
> --showExp _ = "NOT IMPLEMENTED YET"

> showCoreLit :: CoreLit -> String
> showCoreLit (Lint i) = show i
> showCoreLit (Lrational r) = show r
> showCoreLit (Lchar c) = [c]
> showCoreLit (Lstring s) = s

> showBind :: Bind -> String
> showBind (Vb (var,ty)) = wrapName "Bind" $ var ++ "::" ++ showType ty
> showBind (Tb (tvar,kind)) = "BIND NOT IMPLEMENTED YET"

> showVdefg :: Vdefg -> String
> showVdefg (Rec vdefs) = wrapName "Rec" $ concatMap showVdef vdefs
> --showVdefg (Nonrec (Vdef ((mname,var),ty,exp) )) = "Nonrec\n\t\t..qual_mname: " ++ show mname ++ "\n\t\t..var: " ++ show var ++ "\n\t\t..ty: " ++ show ty ++ "\n\t\t..exp: " 
> showVdefg (Nonrec vdef) = wrapName "Nonrec" $ showVdef vdef

> showVdef :: Vdef -> String
> showVdef (Vdef ((mname,var),ty,exp)) = wrapName "Vdef" $ showMname mname ++ var ++ showType ty ++ showExp exp

> showAlt :: Alt -> String
> showAlt alt = "showAlt NOT IMPLEMENTED"