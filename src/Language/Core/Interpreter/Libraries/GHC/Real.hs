----------------------------------------------------------------------------
-- |
-- Module      :  Language.Core.Interpreter.Libraries.GHC.Real
-- Copyright   :  (c) Carlos López-Camey, University of Freiburg
-- License     :  BSD-3
--
-- Maintainer  :  c.lopez@kmels.net
-- Stability   :  stable
--
--
-- Defines functions defined in GHC.real
-----------------------------------------------------------------------------

module Language.Core.Interpreter.Libraries.GHC.Real(all) where

import DART.CmdLine
import Language.Core.Core
import Language.Core.Interpreter(evalId)
import Language.Core.Interpreter.Apply
import Language.Core.Interpreter.Libraries.Monomophy(monomophy_2)
import Language.Core.Interpreter.Structures
import Prelude hiding (all)

all :: [(Id, Either Thunk Value)]
all = [ mod'
      ]

-- | The polymorphic modulo function
-- mod :: Integral a => a -> a -> a
mod' :: (Id, Either Thunk Value)
mod' = (id, Right $ Fun (monomophy_2 modulo) "polymorphic(+)") 
  where
    id = "base:GHC.Real.mod"
    modulo :: Value -> Value -> IM Value
    modulo (Num x) (Num y) = return . Num $ x `mod` y
    modulo x y = return . Wrong $ "Cannot calculate modulo between " ++ show x ++ " and " ++ show y
