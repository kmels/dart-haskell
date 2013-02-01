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

> showType :: Ty -> String
> showType (Tvar t) = t
> showType (Tcon (mname,t2)) = showMname mname ++ "." ++ t2
> showType (Tapp t1 t2) = showType t1 ++ showType t2
> showType _ = "UNKNOWN"

> showMname :: Maybe AnMname -> String
> showMname Nothing = ""
> showMname (Just (M ((P packageName),[s1],s2))) = packageName ++ ":" ++ s1 ++ "." ++ s2

> showExp :: Exp -> String
> showExp (Var (mname,variable)) = showMname mname ++ variable
> showExp (Dcon (mname,dcon)) = showMname mname ++ dcon
> showExp (Lit (Literal coreLit ty)) = showCoreLit coreLit ++ showType ty
> showExp (App exp1 exp2) = showExp exp1 ++ showExp exp2
> showExp (Appt exp typ) = showExp exp ++ showType typ
> showExp (Lam bind exp) = showBind bind ++ showExp exp
> showExp ( bind exp) = showBind bind ++ showExp exp
> showExp _ = "NOT IMPLEMENTED YET"

> showCoreLit :: CoreLit -> String
> showCoreLit (Lint i) = show i
> showCoreLit (Lrational r) = show r
> showCoreLit (Lchar c) = [c]
> showCoreLit (Lstring s) = s

> showBind :: Bind -> String
> showBind (Vb (var,ty)) = var ++ showType ty
> showBind (Tb (tvar,kind)) = "BIND NOT IMPLEMENTED YET"
