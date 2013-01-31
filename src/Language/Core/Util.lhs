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
> showType (Tapp t1 t2) = "(" ++ showType t1 ++ " => " ++ showType t2 ++ ")"

> showMname :: Maybe AnMname -> String
> showMname Nothing = ""
> showMname (Just (M ((P packageName),[s1],s2))) = packageName ++ ":" ++ s1 ++ "." ++ s2