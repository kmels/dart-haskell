----------------------------------------------------------------------------
-- |
-- Module      :  Language.Core.Interpreter.Libraries.GHC.Prim
-- Copyright   :  (c) Carlos López-Camey, University of Freiburg
-- License     :  BSD-3
--
-- Maintainer  :  c.lopez@kmels.net
-- Stability   :  stable
--
--
-- Defines functions defined in GHC.Prim
-----------------------------------------------------------------------------

module Language.Core.Interpreter.Libraries.GHC.Prim(all) where

import DART.CmdLine
import Language.Core.Interpreter(evalId)
import Language.Core.Interpreter.Structures
import Prelude hiding (all)

all :: [(Id, Either Thunk Value)]
all = [ intSharpSum
      ]
      
-- | +#, sums two ints
intSharpSum :: (Id, Either Thunk Value)
intSharpSum = (id, Right $ Fun sum'_1 "unary +#") where
  id = "ghc-prim:GHC.Prim.+#"
  sum'_1 :: Id -> Env -> IM Value 
  sum'_1 id env = evalId id env >>= \x -> return (Fun (sum'_2 x) "binary +#")

  sum'_2 :: Value -> Id -> Env -> IM Value
  sum'_2 (Num x) id env = evalId id env >>= \val -> case val of 
    (Num y) -> return . Num $ x + y
    y -> return $ Wrong $ "binary I# expected an integer, got " ++ show y    
  sum'_2 val _ _ = return $ Wrong $ "unary I#, expected an integer, got " ++ show val
