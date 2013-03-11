----------------------------------------------------------------------------
-- |
-- Module      :  Language.Core.Interpreter.Libraries.GHC.Num
-- Copyright   :  (c) Carlos López-Camey, University of Freiburg
-- License     :  BSD-3
--
-- Maintainer  :  c.lopez@kmels.net
-- Stability   :  stable
--
--
-- Defines functions defined in GHC.Num
-----------------------------------------------------------------------------

module Language.Core.Interpreter.Libraries.GHC.Num(all) where

import DART.CmdLine
import Language.Core.Core
import Language.Core.Interpreter(evalId)
import Language.Core.Interpreter.Apply
import Language.Core.Interpreter.Libraries.Monomophy(monomophy_2)
import Language.Core.Interpreter.Structures
import Prelude hiding (all)


all :: [(Id, Either Thunk Value)]
all = [ plus
        --         , multiply
        --         , zdfNumInteger
      , zdfNumInt
      ]
      
-- | The function that adds two numbers in GHC (base:GHC.Num.+).
-- | A function takes three values: one that converts this polymorphic function
-- into a monomorphic, then x and y to return the value x + y if they are Nums

plus :: (Id, Either Thunk Value)
plus = (id, Right $ Fun (monomophy_2 addValues) "polymorphic(+)") where
  id = "base:GHC.Num.+"      

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

zdfNumInt = (id, Right val) where
  id = "base:GHC.Num.$fNumInt"
  
  evvv id env = do
    debugM $ "zdfnumInt: id " ++ id
    debugM $ "zdfnumInt: env: " ++ show (map fst env)
    return $ Wrong "Don't know"
  val = Fun (evvv) (idName id)

zdfNumInteger = (id, Right val) where
  id = "base:GHC.Num.$fNumInteger"
  val = Fun evalId (idName id)

