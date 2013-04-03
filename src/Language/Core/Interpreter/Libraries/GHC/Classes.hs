module Language.Core.Interpreter.Libraries.GHC.Classes where

import Language.Core.Interpreter.Structures
import Language.Core.Core
import Language.Core.Interpreter.Libraries.Monomophy(monomophy_2, mkMonomophier)
import Language.Core.Interpreter.Apply
import Language.Core.Interpreter(evalId)

all :: [(Id, Either Thunk Value)]
all = [ equals
        , lt, leq        
        , geq
        , mkMonomophier "ghc-prim:GHC.Classes.$p1Ord"
        , mkMonomophier "ghc-prim:GHC.Classes.$fOrdInt"
        ]

-- | (==)
equals :: (Id, Either Thunk Value)
equals = (id, Right $ Fun (monomophy_2 "(==)" valEq) "polymorphic(==)") where
   id = "ghc-prim:GHC.Classes.==" 
   valEq :: Value -> Value -> IM Value
   valEq v w = return . Boolean $ (==) v w

leq :: (Id, Either Thunk Value)
leq = (id, Right $ Fun (monomophy_2 "(<=)" leq') "(<=)") where
  id = "ghc-prim:GHC.Classes.<="
  leq' :: Value -> Value -> IM Value
  leq' (Num v) (Num w) = return . Boolean $ v <= w
  leq' v w = return . Wrong $ "lessEquals: " ++ show v ++ " and " ++ show w ++ " are not comparable"

lt :: (Id, Either Thunk Value)
lt = (id, Right $ Fun (monomophy_2 "(<)" lt') "(<)") where
  id = "ghc-prim:GHC.Classes.<"
  lt' :: Value -> Value -> IM Value
  lt' (Num v) (Num w) = return . Boolean $ v < w
  lt' v w = return . Wrong $ "lessThan: " ++ show v ++ " and " ++ show w ++ " are not comparable"
  
geq = (id, Right $ Fun (monomophy_2 "(>=)" geq') "(>=)")
  where 
    id = "ghc-prim:GHC.Classes.>="
    geq' :: Value -> Value -> IM Value
    geq' (Num v) (Num w) = return . Boolean $ v >= w
    geq' v w = return . Wrong $ "greaterEquals: " ++ show v ++ " and " ++ show w ++ " are not comparable"
