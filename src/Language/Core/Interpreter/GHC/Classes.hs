module Language.Core.Interpreter.GHC.Classes where

import Language.Core.Interpreter.Structures
import Language.Core.Core
import Language.Core.Interpreter.Apply

-- | (==)

--monomophy fun _ = Fun (\x -> return $ eq_2 x) "(==) :: a -> a -> Bool"

equals :: (Id, Either Thunk Value)
equals = (id, Right val) where
  id = "ghc-prim:GHC.Classes.==" 
  eq_2 tcf x = Fun (\y -> equality x y) ("(==) :: " ++ show tcf ++ " -> a -> Bool")
  monomophy tcf = Fun (\x -> return $ eq_2 tcf x) ("(==) :: " ++ show tcf ++ " -> a -> Bool")
  val = Fun (return . monomophy) "polymorphic (==)"

lessEquals :: (Id, Either Thunk Value)
lessEquals = (id, Right val) where
  id = "ghc-prim:GHC.Classes.<="
  leq_2 tcf x = Fun (\y -> lessEquals' x y) ("(<=) :: " ++ show tcf ++ " -> Bool")
  monomophy tcf = Fun (return . leq_2 tcf) ("(<=) :: " ++ show tcf ++ " -> " ++ show tcf ++ " -> Bool")
  val = Fun (return . monomophy) "polymorphic (<=)"

lessEquals' :: Value -> Value -> IM Value
lessEquals' (Num v) (Num w) = return . Boolean $ v <= w
lessEquals' v w = return . Wrong $ "lessEquals: " ++ show v ++ " and " ++ show w ++ " are not comparable"

greaterEquals :: (Id, Either Thunk Value)
greaterEquals = (id, Right val) where
  id = "ghc-prim:GHC.Classes.>="
  geq_2 tcf x = Fun (\y -> greaterEquals' x y) ("(>=) :: " ++ show tcf ++ " -> Bool")
  monomophy tcf = Fun (return . geq_2 tcf) ("(>=) :: " ++ show tcf ++ " -> " ++ show tcf ++ " -> Bool")
  val = Fun (return . monomophy) "polymorphic (>=)"
  
equality :: Value -> Value -> IM Value
equality v w = return . Boolean $ v == w

greaterEquals' :: Value -> Value -> IM Value
greaterEquals' (Num v) (Num w) = return . Boolean $ v >= w
greaterEquals' v w = return . Wrong $ "greaterEquals: " ++ show v ++ " and " ++ show w ++ " are not comparable"

fOrdInt :: (Id, Either Thunk Value)
fOrdInt = (id, Right val) where
  id = "ghc-prim:GHC.Classes.$fOrdInt"
  val = Fun return (idName id)

all :: [(Id, Either Thunk Value)]
all = [ equals
        , lessEquals
        , greaterEquals
        , fOrdInt
        ]
