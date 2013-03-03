module Language.Core.Interpreter.GHC.Classes where

import Language.Core.Interpreter.Structures
import Language.Core.Core
import Language.Core.Interpreter.Apply

-- | (==)

equals :: (Id, Either Thunk Value)
equals = (id, Right val) where
  id = "ghc-prim:GHC.Classes.==" 
  eq_2 x = Fun (\y -> equality x y) "== :: a -> a -> Bool"
  monomophy tcf = Fun (\x -> return $ eq_2 x) "Monophy"
  val = Fun (return . monomophy) "(==), monomophied"

lessEquals :: (Id, Either Thunk Value)
lessEquals = (id, Right val) where
  id = "ghc-prim:GHC.Classes.<="
  eq_2 x = Fun (\y -> lessEquals' x y) "== :: a -> a -> Bool"
  monomophy tcf = Fun (\x -> return $ eq_2 x) "Monophy"
  val = Fun (return . monomophy) "(<=), monomophied"

lessEquals' :: Value -> Value -> IM Value
lessEquals' (Num v) (Num w) = return . Boolean $ v <= w
lessEquals' v w = return . Wrong $ "lessEquals: " ++ show v ++ " and " ++ show w ++ " are not comparable"

greaterEquals :: (Id, Either Thunk Value)
greaterEquals = (id, Right val) where
  id = "ghc-prim:GHC.Classes.>="
  eq_2 x = Fun (\y -> greaterEquals' x y) "== :: a -> a -> Bool"
  monomophy tcf = Fun (\x -> return $ eq_2 x) "Monophy"
  val = Fun (return . monomophy) "(<=), monomophied"
  
equality :: Value -> Value -> IM Value
equality v w = return . Boolean $ v == w

greaterEquals' :: Value -> Value -> IM Value
greaterEquals' (Num v) (Num w) = return . Boolean $ v >= w
greaterEquals' v w = return . Wrong $ "greaterEquals: " ++ show v ++ " and " ++ show w ++ " are not comparable"

all :: [(Id, Either Thunk Value)]
all = [ equals
        , lessEquals
        , greaterEquals
        ]
