----------------------------------------------------------------------------
-- |
-- Module      :  Language.Core.Interpreter.Libraries
-- Copyright   :  (c) Carlos López-Camey, University of Freiburg
-- License     :  BSD-3
--
-- Maintainer  :  c.lopez@kmels.net
-- Stability   :  stable
--
-- Exports an environment containing built-in functions and types 
-- like the integer sum funtion or the list type
-----------------------------------------------------------------------------

module Language.Core.Interpreter.Libraries(ghc_base) where

import qualified Language.Core.Interpreter.Libraries.GHC.CString as GHC.CString
import qualified Language.Core.Interpreter.Libraries.GHC.Classes as GHC.Classes
import qualified Language.Core.Interpreter.Libraries.GHC.Num as GHC.Num
import qualified Language.Core.Interpreter.Libraries.GHC.Tuple as GHC.Tuple
import qualified Language.Core.Interpreter.Libraries.GHC.Types as GHC.Types
import qualified Language.Core.Interpreter.Libraries.GHC.Real as GHC.Real
import qualified Language.Core.Interpreter.Libraries.GHC.Show as GHC.Show
import qualified Language.Core.Interpreter.Libraries.GHC.Err as GHC.Err
import           Language.Core.Core
import           Language.Core.Interpreter.Structures

-- | List of functions _indexed_ by id, the most of them being a Value and not Thunks.
ghc_base :: [(Id,Either Thunk Value)]
ghc_base = concat $ [
  GHC.Num.all
   , GHC.Classes.all
   , GHC.CString.all
   , GHC.Types.all
   , GHC.Tuple.all
   , GHC.Real.all
   , GHC.Show.all
   , GHC.Err.all
  ]
