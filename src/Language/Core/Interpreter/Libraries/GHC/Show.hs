----------------------------------------------------------------------------
-- |
-- Module      :  Language.Core.Interpreter.Libraries.GHC.Show
-- Copyright   :  (c) Carlos López-Camey, University of Freiburg
-- License     :  BSD-3
--
-- Maintainer  :  c.lopez@kmels.net
-- Stability   :  stable
--
--
-- Defines functions defined in GHC.Num
-----------------------------------------------------------------------------

module Language.Core.Interpreter.Libraries.GHC.Show(all) where

import DART.CmdLine
import Language.Core.Core
import Language.Core.Interpreter(evalId)
import Language.Core.Interpreter.Apply
import Language.Core.Interpreter.Util(return')
import Language.Core.Interpreter.Libraries.Monomophy(monomophy_1, monomophy_2,mkMonomophier)
import Language.Core.Interpreter.Structures
import Prelude hiding (all)

all :: [(Id, Either Thunk Value)]
all = [ show'
      , mkMonomophier "base:GHC.Show.$fShowInt"
      ]
      
-- | The function that converts a showable to a string      
show' :: (Id, Either Thunk Value)
show' = (id, Right $ Fun (monomophy_1 "(show)" (return . String . show)) "polymorphic(show)") 
  where
    id = "base:GHC.Show.show"

