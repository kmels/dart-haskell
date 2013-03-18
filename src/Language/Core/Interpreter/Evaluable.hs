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

module Language.Core.Interpreter.Evaluable where

import Language.Core.Interpreter.Structures

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
