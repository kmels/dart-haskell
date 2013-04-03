----------------------------------------------------------------------------
-- |
-- Module      :  Language.Core.Interpreter.Libraries.GHC.Err
-- Copyright   :  (c) Carlos López-Camey, University of Freiburg
-- License     :  BSD-3
--
-- Maintainer  :  c.lopez@kmels.net
-- Stability   :  stable
--
-- Contains function values equivalent to those in GHC.Err
-----------------------------------------------------------------------------

module Language.Core.Interpreter.Libraries.GHC.Err where

import Language.Core.Interpreter
import Language.Core.Interpreter.Libraries.Monomophy
import Language.Core.Interpreter.Structures

err :: (Id, Either Thunk Value)
err = ("base:GHC.Err.error", Right $ Fun mkError "error") 
      where
        mkError :: Id -> Env -> IM Value
        mkError id env = evalId id env >>= \v -> case v of
          (String s) -> return . Wrong $ s
          _ -> return . Wrong $ "Wrong argument to GHC.Err.error, namely: " ++ show v
        
all = [err]
