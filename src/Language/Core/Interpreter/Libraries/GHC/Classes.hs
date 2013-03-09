module Language.Core.Interpreter.Libraries.GHC.Classes where

import Language.Core.Interpreter.Structures
import Language.Core.Core
import Language.Core.Interpreter.Apply
import Language.Core.Interpreter(evalAddr)

-- | (==)

monomophy_2 :: HeapAddress -> (Value -> Value -> IM Value) -> IM Value
monomophy_2 tyclass_adr f_callback = do
  tyclass_val <- evalAddr tyclass_adr -- e.g. Var("zdfNumInt") => Fun(f :: (Num a) -> Int) "Int"
  let 
    tyclass_show = show tyclass_val  
    f_unary tcv x = return $ Fun (\y -> evalAddr y >>= f_callback x) (show tcv ++ " Unary")
    f_binary = Fun (\x -> evalAddr x >>= f_unary tyclass_val) (show tyclass_val ++ " Binary")
  return $ f_binary

equals :: (Id, Either Thunk Value)
equals = (id, Right val) where
   id = "ghc-prim:GHC.Classes.==" 
--   eq_2 tcf x = Fun (\y -> evalAddr y >>= valEq x) ("(==) :: " ++ show tcf ++ " -> a -> Bool")
--   monomophy tcf = Fun (\x -> return $ eq_2 tcf x) ("(==) :: " ++ show tcf ++ " -> a -> Bool")
   val = Fun (\tcf -> monomophy_2 tcf valEq) "polymorphic (==)"

valEq :: Value -> Value -> IM Value
valEq v w = return . Boolean $ (==) v w

-- lessEquals :: (Id, Either Thunk Value)
-- lessEquals = (id, Right val) where
--   id = "ghc-prim:GHC.Classes.<="
--   leq_2 tcf x = Fun (\y -> lessEquals' x y) ("(<=) :: " ++ show tcf ++ " -> Bool")
--   monomophy tcf = Fun (return . leq_2 tcf) ("(<=) :: " ++ show tcf ++ " -> " ++ show tcf ++ " -> Bool")
--   val = Fun (return . monomophy) "polymorphic (<=)"

-- lessEquals' :: Value -> Value -> IM Value
-- lessEquals' (Num v) (Num w) = return . Boolean $ v <= w
-- lessEquals' v w = return . Wrong $ "lessEquals: " ++ show v ++ " and " ++ show w ++ " are not comparable"

-- greaterEquals :: (Id, Either Thunk Value)
-- greaterEquals = (id, Right val) where
--   id = "ghc-prim:GHC.Classes.>="
--   geq_2 tcf x = Fun (\y -> greaterEquals' x y) ("(>=) :: " ++ show tcf ++ " -> Bool")
--   monomophy tcf = Fun (return . geq_2 tcf) ("(>=) :: " ++ show tcf ++ " -> " ++ show tcf ++ " -> Bool")
--   val = Fun (return . monomophy) "polymorphic (>=)"

-- greaterEquals' :: Value -> Value -> IM Value
-- greaterEquals' (Num v) (Num w) = return . Boolean $ v >= w
-- greaterEquals' v w = return . Wrong $ "greaterEquals: " ++ show v ++ " and " ++ show w ++ " are not comparable"

-- fOrdInt :: (Id, Either Thunk Value)
-- fOrdInt = (id, Right val) where
--   id = "ghc-prim:GHC.Classes.$fOrdInt"
--   val = Fun lookupH (idName id)


all :: [(Id, Either Thunk Value)]
all = [ equals
--        , lessEquals
--        , greaterEquals
--        , fOrdInt
        ]
