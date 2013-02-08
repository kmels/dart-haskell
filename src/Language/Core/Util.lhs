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

Pretty print a Ty (from extcore) where a function from [Int] to Int is printed as:

`ghc-prim:GHC.Prim.(->)ghc-prim:GHC.Types.[]ghc-prim:GHC.Types.Intghc-prim:GHC.Types.Int`

This representation is used to reify types thorugh Language.Core.TypeExtractor

> showExtCoreType :: Ty -> String
> showExtCoreType (Tvar t) = t
> showExtCoreType (Tcon (mname,t2)) = showMname mname ++ "." ++ t2
> showExtCoreType (Tapp t1 t2) = showExtCoreType t1 ++ showExtCoreType t2
> showExtCoreType _ = "UNKNOWN"

> showType :: Ty -> String
> showType (Tvar t) = wrapName "Tvar" t

> showType (Tcon (Just (M ((P "ghczmprim"), ["GHC"], "Types")),primitiveType)) = primitiveType
> showType (Tcon (mname,t2)) = wrapName "Tcon" $ showMname mname ++ "." ++ t2

In order to pretty print types, we'll pattern match on the list type as a Ty

> showType (Tapp (Tcon (Just (M ((P "ghczmprim"), ["GHC"], "Types")),"ZMZN")) (Tcon (Just (M ((P "ghczmprim"), ["GHC"], "Types")),listType))) = "[" ++ listType ++ "]"
> showType (Tapp t1 t2) = wrapName "Tapp" $ showType t1 ++ "TIP2: " ++ showType t2

> showType _ = "NOT IMPLEMENTED YET!!!!!!"

> showMname :: Maybe AnMname -> String
> showMname Nothing = ""
> showMname (Just (M ((P packageName),[s1],s2))) = packageName ++ ":" ++ s1 ++ "." ++ s2

> wrapName s r = s ++ "(" ++ r ++ ")"

> showExp :: Exp -> String
> showExp (Var (mname,variable)) = wrapName "var" $ showMname mname ++ "." ++ variable
> showExp (Dcon (mname,dcon)) = wrapName "dataConstructor" $ showMname mname ++ dcon ++")"
> showExp (Lit (Literal coreLit ty)) = wrapName "lit" $ showCoreLit coreLit ++ showType ty
> showExp (App exp1 exp2) = wrapName "app" $ showExp exp1 ++ showExp exp2
> showExp (Appt exp typ) = wrapName "appt" $ showExp exp ++ showType typ --e.g. >= Int
> showExp (Lam bind exp) = "\n\t\\" ++ showBind bind ++ " -> " ++ showExp exp
> showExp (Let vdefg exp) = wrapName "let" $ showVdefg vdefg ++ showExp exp
> showExp (Case exp (vbind_var,vbind_ty) ty alts) = "\n\t\tcase \n\t\t\t" ++ showExp exp ++ "\n\tof " ++vbind_var ++ "TYPE2" ++ showType vbind_ty ++ "TYPE" ++ showType ty ++ concatMap showAlt alts
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