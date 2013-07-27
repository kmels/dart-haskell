module Language.Core.Interpreter.Libraries.GHC.Classes where

import Language.Core.Interpreter.Structures
import Language.Core.Interpreter.Evaluable
import Language.Core.Core
import Language.Core.Interpreter.Libraries.Monomophy(monomophy_2, mkMonomophier)
import Language.Core.Interpreter.Libraries.ApplyValFun(applyFun_2)
import Language.Core.Interpreter.Apply
import Language.Core.Interpreter(evalId)
import qualified Data.List as Data.List

all :: [(Id, Either Thunk Value)]
all = [ equals
        -- Bools
      , conjunction -- (&&) :: Bool -> Bool -> Bool
      , lt, leq        
        , gt, geq
        , mkMonomophier "ghc-prim:GHC.Classes.$p1Ord"
        , mkMonomophier "ghc-prim:GHC.Classes.$fOrdInt"
        , mkMonomophier "ghc-prim:GHC.Classes.$fEq[]"
        , mkMonomophier "ghc-prim:GHC.Classes.$fEqInt"
        
        , mkMonomophier "integer-gmp:GHC.Integer.Type.$fOrdInteger"
        ]

-- | (==)
equals :: (Id, Either Thunk Value)
equals = (id, Right $ Fun (monomophy_2 "(==)" valEq) "polymorphic(==)") where
   id = "ghc-prim:GHC.Classes.==" 
   valEq :: Value -> Value -> IM Value
   valEq v@(Wrong _) _ = return v
   valEq _ w@(Wrong _) = return w
   valEq (TyConApp dc1 ps) (TyConApp dc2 ps2) | dc1 == dc2 && length ps == length ps2 = do
     -- get the value of every pointer
     ps_vals <- mapM (flip eval []) ps
     ps2_vals <- mapM (flip eval []) ps2

     -- compare every corresponding pointer value, they must be all equal
     mapM (uncurry valEq) (ps_vals `zip` ps2_vals) >>= return . Boolean . Data.List.all ((==) $ Boolean $ True) 
                                              | otherwise = return . Boolean $ False
   valEq v w = return . Boolean $ (==) v w   

-- | (&&)
conjunction :: (Id, Either Thunk Value)
conjunction = (id, Right $ applyFun_2 "&&" valConjunction)
  where
    id = "ghc-prim:GHC.Classes.&&" 
    valConjunction :: Value -> Value -> IM Value
    valConjunction (Boolean b1) (Boolean b2) = return . Boolean $ (&&) b1 b2
    valConjunction v@(Wrong _) _ = return v
    valConjunction _ w@(Wrong _) = return w
   
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

-- | Greater than (>)
gt :: (Id, Either Thunk Value)
gt = (id, Right $ Fun (monomophy_2 "(>)" gt') "(>)") where
  id = "ghc-prim:GHC.Classes.>"
  gt' :: Value -> Value -> IM Value
  gt' (Num v) (Num w) = return . Boolean $ v > w
  gt' v w = return . Wrong $ "greaterThan: " ++ show v ++ " and " ++ show w ++ " are not comparable"
    
geq = (id, Right $ Fun (monomophy_2 "(>=)" geq') "(>=)")
  where 
    id = "ghc-prim:GHC.Classes.>="
    geq' :: Value -> Value -> IM Value
    geq' (Num v) (Num w) = return . Boolean $ v >= w
    geq' v w = return . Wrong $ "greaterEquals: " ++ show v ++ " and " ++ show w ++ " are not comparable"
