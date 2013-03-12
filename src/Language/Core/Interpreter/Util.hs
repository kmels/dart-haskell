----------------------------------------------------------------------------
-- |
-- Module      :  Language.Core.Interpreter.Util
-- Copyright   :  (c) Carlos López-Camey, University of Freiburg
-- License     :  BSD-3
--
-- Maintainer  :  c.lopez@kmels.net
-- Stability   :  stable
--
--
-- Useful combinators to use in Language.Core.Interpreter or even
-- Language.Core.Interpreter.Libraries.{GHC.*}
-----------------------------------------------------------------------------

module Language.Core.Interpreter.Util where

import Language.Core.Interpreter.Structures
-- | A function that ignores its parameters and returns a value
-- the parenthesis in the signature have no effects and are only here to understand better 
return' :: Value -> (Id -> Env -> IM Value)
return' v = \_ -> \_ -> return v
