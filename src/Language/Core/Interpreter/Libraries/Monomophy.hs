----------------------------------------------------------------------------
-- |
-- Module      :  Language.Core.Interpreter.Libraries.Monomophy
-- Copyright   :  (c) Carlos López-Camey, University of Freiburg
-- License     :  BSD-3
--
-- Maintainer  :  c.lopez@kmels.net
-- Stability   :  stable
--
--
-- Exports functions that convert polymorphic functions e.g. (+) :: Num a -> a -> a -> a
-- to monomorphic functions e.g. (+) :: Int -> Int -> Int
-----------------------------------------------------------------------------

module Language.Core.Interpreter.Libraries.Monomophy(
  monomophy_2
  ) where

import Language.Core.Interpreter(evalId)
import Language.Core.Interpreter.Structures
-- | Converts a polymorphic binary function on values to a monomorphic one.
monomophy_2 :: (Value -> Value -> IM Value) -> Id -> Env -> IM Value
monomophy_2 f id env = do
  tyclass_val <- evalId id env -- e.g. Var("zdfNumInt") => Fun(f :: (Num a) -> Int) "Int"
  let
    -- before calling f, we must know if there's any errors in them.
    f' :: Value -> Value -> IM Value
    f' v1 v2 = case v1 of
      (Wrong _) -> return v1
      _ -> case v2 of
        (Wrong _) -> return v2
        _ -> f v1 v2
    
    tyclass_show = show tyclass_val  
    f_unary tcv x = return $ Fun (\i -> \e -> evalId i e >>= f' x) (show tcv ++ " Unary")
    f_binary = Fun (\i -> \e -> evalId i e >>= f' tyclass_val) (show tyclass_val ++ " Binary")
  return $ f_binary
