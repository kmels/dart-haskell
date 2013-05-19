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

import Language.Core.Interpreter.Libraries.Monomophy(monomophy_2)
import DART.CmdLine
import Language.Core.Interpreter(evalId)
import Language.Core.Interpreter.Structures
import Prelude hiding (all)

all :: [(Id, Either Thunk Value)]
all = [ intSharpSum,
        geqSharp
      ]
      
-- | >=#, greater equals on ints
geqSharp :: (Id, Either Thunk Value)
geqSharp = (id, Right $ Fun (geq'_binary) "binary(>=#)")
  where 
    id = "ghc-prim:GHC.Prim.>=#"
    geq'_binary :: Id -> Env -> IM Value
    geq'_binary i e = evalId i e >>= \x -> return $ Fun (geq'_unary x) "unary(>=#)"
      
    geq'_unary :: Value -> Id -> Env -> IM Value
    geq'_unary (Num x) i e = evalId i e >>= \val -> case val of
      (Num y) -> return . Boolean $ x >= y
      v -> return . Wrong $ "unary >=#, expected Int, got " ++ show val
    geq'_unary v _ _  = return . Wrong $ "binary >=#, expected Int, got " ++ show v

-- | >=#, greater equals on ints
leqSharp :: (Id, Either Thunk Value)
leqSharp = (id, Right $ Fun (geq'_binary) "binary(<=#)")
  where 
    id = "ghc-prim:GHC.Prim.<=#"
    geq'_binary :: Id -> Env -> IM Value
    geq'_binary i e = evalId i e >>= \x -> return $ Fun (geq'_unary x) "unary(<=#)"
      
    geq'_unary :: Value -> Id -> Env -> IM Value
    geq'_unary (Num x) i e = evalId i e >>= \val -> case val of
      (Num y) -> return . Boolean $ x <= y
      v -> return . Wrong $ "unary <=#, expected Int, got " ++ show val
    geq'_unary v _ _  = return . Wrong $ "binary <=#, expected Int, got " ++ show v
        
-- | +#, sums two ints
intSharpSum :: (Id, Either Thunk Value)
intSharpSum = (id, Right $ Fun sum'_1 "unary +#") where
  id = "ghc-prim:GHC.Prim.+#"
  sum'_1 :: Id -> Env -> IM Value 
  sum'_1 id env = evalId id env >>= \x -> return (Fun (sum'_2 x) "binary +#")

  sum'_2 :: Value -> Id -> Env -> IM Value
  sum'_2 (Num x) id env = evalId id env >>= \val -> case val of 
    (Num y) -> return . Num $ x + y
    y -> return $ Wrong $ "unary I#: expected an integer, got " ++ show y    
  sum'_2 val _ _ = return $ Wrong $ "binary I#, expected an integer, got " ++ show val
