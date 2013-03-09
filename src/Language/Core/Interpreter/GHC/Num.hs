module Language.Core.Interpreter.GHC.Num where

import Language.Core.Core
import Language.Core.Interpreter(evalHeapAddress)
import Language.Core.Interpreter.Apply
import Language.Core.Interpreter.Structures
-- | The function that adds two numbers in GHC (base:GHC.Num.+).

-- | A function takes three values: one that converts this polymorphic function
-- into a monomorphic, then x and y to return the value x + y if they are Nums
plus :: (Id, Either Thunk Value)
plus = (id, Right $ Fun monomophy "(+) :: Num a => a -> a -> a") where
  id = "base:GHC.Num.+"  
  
  monomophy tcf = do
    tcf_val <- evalHeapAddress tcf [] -- e.g. $NumInt
    let dsc = "(+) :: " ++ show tcf_val ++ " -> " ++ show tcf_val ++ " -> " ++ show tcf_val
    return $ Fun (\xa -> evalHeapAddress xa [] >>= add_1 tcf_val) dsc
    
  add_1 tcf_val x = 
    let dsc = "(+) :: " ++ show tcf_val ++ " -> " ++ show tcf_val ++ " -> " ++ show tcf_val    
    in return $ Fun (\ya -> evalHeapAddress ya [] >>= addValues x) " "


addValues :: Value -> Value -> IM Value
addValues (Num i) (Num j) = return . Num $ i + j 
addValues a b = return . Wrong $ "Trying to add values " ++ show a ++ " and " ++ show b

-- -- | The function that multiplies two numbers in GHC (base:GHC.Num.*).
-- multiply :: (Id, Either Thunk Value)
-- multiply = (id, Right val) where
--   id = "base:GHC.Num.*"
--   mul_2 x = Fun (\y -> multiplyValues x y) "(*) :: Int -> Int"
--   monomophy tcf = Fun (\x -> return $ mul_2 x) "(*) :: Int -> Int -> Int"
--   val = Fun (\tcf -> return $ monomophy tcf) "polymorphic (*)"

-- multiplyValues :: Value -> Value -> IM Value
-- multiplyValues (Num i) (Num j) = return . Num $ i * j 
-- multiplyValues a b = return . Wrong $ "Trying to multiply values " ++ show a ++ " and " ++ show b

-- -- | The apply function on Ints. It takes a function `f :: Int -> Int` and an `x :: Int` and evals to `f x :: Int`.

-- zdfNumInt = (id, Right val) where
--   id = "base:GHC.Num.$fNumInt"
--   val = Fun return (idName id)

-- zdfNumInteger = (id, Right val) where
--   id = "base:GHC.Num.$fNumInteger"
--   val = Fun return (idName id)

all :: [(Id, Either Thunk Value)]
all = [ plus
--         , multiply
--         , zdfNumInteger
--         , zdfNumInt
      ]
