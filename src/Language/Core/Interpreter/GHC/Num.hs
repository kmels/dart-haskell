module Language.Core.Interpreter.GHC.Num where

import Language.Core.Interpreter.Structures
import Language.Core.Core
import Language.Core.Interpreter.Apply

-- | The function that adds two numbers in GHC (base:GHC.Num.+).

plus :: (Id, Either Thunk Value)
plus = (id, Right val) where
  id = "base:GHC.Num.+"
  add_2 x = Fun (\y -> addValues x y) "+ :: a -> a -> a"
  monomophy tcf = Fun (\x -> return $ add_2 x) "Monophy"
  val = Fun (\tcf -> return $ monomophy tcf) "(+), monophied"

addValues :: Value -> Value -> IM Value
addValues (Num i) (Num j) = return . Num $ i + j 
addValues a b = return . Wrong $ "Trying to add values " ++ show a ++ " and " ++ show b

-- | The function that multiplies two numbers in GHC (base:GHC.Num.*).
multiply :: (Id, Either Thunk Value)
multiply = (id, Right val) where
  id = "base:GHC.Num.*"
  mul_2 x = Fun (\y -> multiplyValues x y) "+ :: a -> a -> a"
  monomophy tcf = Fun (\x -> return $ mul_2 x) "Monophy"
  val = Fun (\tcf -> return $ monomophy tcf) "(*), monophied"

multiplyValues :: Value -> Value -> IM Value
multiplyValues (Num i) (Num j) = return . Num $ i * j 
multiplyValues a b = return . Wrong $ "Trying to multiply values " ++ show a ++ " and " ++ show b

-- | The apply function on Ints. It takes a function `f :: Int -> Int` and an `x :: Int` and evals to `f x :: Int`.

zdfNumInt = (id, Right val) where
  id = "GHC.Base.$fNumInt"
  val = Fun return id

zdfNumInteger = (id, Right val) where
  id = "GHC.Base.$fNumInteger"
  val = Fun return id

all :: [(Id, Either Thunk Value)]
all = [ plus
        , multiply
        , zdfNumInteger
        , zdfNumInt
        ]
