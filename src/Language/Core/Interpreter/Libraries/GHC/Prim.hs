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
import Language.Core.Interpreter.Libraries.ApplyValFun(applyFun_2)
import DART.CmdLine
import Language.Core.Interpreter(evalId)
import Language.Core.Interpreter.Structures
import Prelude hiding (all)

all :: [(Id, Either Thunk Value)]
all = [ intSharpSum
        , geqSharp -- >=
        , gThanSharp -- >#
        , value_equality -- ==#
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
gThanSharp :: (Id, Either Thunk Value)
gThanSharp = (id, Right $ applyFun_2 ">#" gThanVal)
  where 
    id = "ghc-prim:GHC.Prim.>#"
    
    gThanVal :: Value -> Value -> IM Value
    gThanVal (Num x) (Num y) = return . Boolean $ x < y 
    gThanVal v@(Wrong _) _ = return . Wrong $ "binary(>#) expected an Int, got " ++ show v
    gThanVal _ w@(Wrong _) = return . Wrong $ "unary(>#) expected an Int, got " ++ show w
    
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
  
-- | Value equality 
value_equality :: (Id, Either Thunk Value)
value_equality = (id, Right $ Fun eq' "unary ==#") where
  id = "ghc-prim:GHC.Prim.==#"
  eq' :: Id -> Env -> IM Value
  eq' id env = evalId id env >>= \x -> return (Fun (eq'_2 x) "binary ==#")
  
  eq'_2 :: Value -> Id -> Env -> IM Value
  eq'_2 x y_id env = evalId y_id env >>= return . Boolean . (==) x
