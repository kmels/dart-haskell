----------------------------------------------------------------------------
-- |
-- Module      :  Language.Core.Interpreter.Libraries.ApplyValFun
-- Copyright   :  (c) Carlos López-Camey, University of Freiburg
-- License     :  BSD-3
--
-- Maintainer  :  c.lopez@kmels.net
-- Stability   :  stable
--
--
-- Provides useful functions of some arity to evaluate functions on Values
-----------------------------------------------------------------------------

module Language.Core.Interpreter.Libraries.ApplyValFun(
  applyFun_2
  ) where

import Language.Core.Interpreter(evalId)
import Language.Core.Interpreter.Structures

-- | Given a binary value function, evaluate two ids and apply the given function
applyFun_2 :: String -> (Value -> Value -> IM Value) -> Value
applyFun_2 fun_desc fun = Fun (\i e -> evalId i e >>= return . unaryFun) $ "binary("++fun_desc++")"
  where
    unaryFun :: Value -> Value
    unaryFun x = Fun (\i e -> evalId i e >>= \y -> fun x y) $ "unary("++fun_desc++")"
