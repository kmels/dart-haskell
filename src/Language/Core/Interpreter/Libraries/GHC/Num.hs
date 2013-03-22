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
import Language.Core.Interpreter.Util(return')
import Language.Core.Interpreter.Libraries.Monomophy(monomophy_1, monomophy_2,mkMonomophier)
import Language.Core.Interpreter.Structures
import Prelude hiding (all)


all :: [(Id, Either Thunk Value)]
all = [ plus
      , minus
      , multiply
      , fromInteger'
      , zdfNumInteger
      , zdfNumInt
      , mkMonomophier "integer-gmp:GHC.Integer.Type.$fEqInteger"
      ]
      
-- | The function that adds two numbers in GHC (base:GHC.Num.+).
plus :: (Id, Either Thunk Value)
plus = (id, Right $ Fun (monomophy_2 "(+)" add') "polymorphic(+)") 
  where
    id = "base:GHC.Num.+"      
    add' :: Value -> Value -> IM Value
    add' (Num i) (Num j) = return . Num $ i + j 
    add' a b = return . Wrong $ "Trying to add values " ++ show a ++ " and " ++ show b

-- | The function that adds two numbers in GHC (base:GHC.Num.+).
minus :: (Id, Either Thunk Value)
minus = (id, Right $ Fun (monomophy_2 "(-)" add') "polymorphic(-)") 
  where
    id = "base:GHC.Num.-"      
    add' :: Value -> Value -> IM Value
    add' (Num i) (Num j) = return . Num $ i - j 
    add' a b = return . Wrong $ "Trying to subtract values " ++ show a ++ " and " ++ show b
    
-- | The function that multiplies two numbers in GHC (base:GHC.Num.*).
multiply :: (Id, Either Thunk Value)
multiply = (id, Right $ Fun (monomophy_2 "(*)" mul') "polymorphic(*)") 
  where
    id = "base:GHC.Num.*"
    mul' :: Value -> Value -> IM Value
    mul' (Num i) (Num j) = return . Num $ i * j 
    mul' a b = return . Wrong $ "Trying to multiply values " ++ show a ++ " and " ++ show b

-- | The function that converts an integer to any number
-- fromInteger :: Integer -> a
fromInteger' = (id, Right $ Fun (monomophy_1 "fromInteger" fromInteger'') "polymorphic(fromInteger)")
  where
    id = "base:GHC.Num.fromInteger"
    fromInteger'' :: Value -> IM Value
    fromInteger'' i@(Num a) = return i
    fromInteger'' x = return . Wrong $ "Can not convert " ++ show x ++ " to a number"

-- -- | The apply function on Ints. It takes a function `f :: Int -> Int` and an `x :: Int` and evals to `f x :: Int`.

zdfNumInt = mkMonomophier "base:GHC.Num.$fNumInt"
zdfNumInteger = mkMonomophier "base:GHC.Num.$fNumInteger"

