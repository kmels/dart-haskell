----------------------------------------------------------------------------
-- |
-- Module      :  DART.TestTypeExtractor.hs
-- Copyright   :  (c) Carlos López-Camey, University of Freiburg
-- License     :  BSD-3
--
-- Maintainer  :  c.lopez@kmels.net
-- Stability   :  stable
--
--
-- Tests conversion from extcore's Ty to DART's TypeExtractor.DataTypes
--
-- For example a TApp (Tvar ("->") (TApp (TApp (Tvar "->") (Tvar "A" ) (Tvar "B"))) should
-- be converted to LambdaAbstraction (DataType "A") (CType (DataType "B")) 
-----------------------------------------------------------------------------

module DART.TestTypeExtractor where

import DART.ExtCore.TypeExtractor
import DART.TestUtils
import Data.Maybe(fromJust)
import Language.Core.TestTy(getDefTypes)
import Text.Encoding.Z
test = unsafePerformIO testIO

-- | Loads modules, extract their types, converts them and compares to expected conversions
testIO :: IO Test
testIO = do  
  -- trees
  trees_tys <- getDefTypes "examples/testing/OnTrees.hs"
  let    
    treesc = map convertIdTy trees_tys -- extracted data types with ids on trees
    testTrees = checkExpected treesc trees_ec
    
  -- nums
  nums_tys <- getDefTypes "examples/interpreter/GHC.Num.hs"
  let    
    numsc = map convertIdTy nums_tys -- extracted data types with ids on trees
    testNums = checkExpected numsc nums_ec
  
  return $ TestList [] -- [testTrees,testNums]

-- | Expected extracted DataTypes coming from examples/interpreter/GHC.Nums.hs
nums_ec :: [(Id,Maybe GeneralType)] 
nums_ec = [
  (mdl_name @@ "numberTwenty", Just $ CType int)
  ,(mdl_name @@ "numberTen", Just $ CType int)
  ,(mdl_name @@ "tautology1", Just $ CType bool)
  ,(mdl_name @@ "fib0", Just $ CType integer)
  , ("plusOneIntreG", int `toConcrete` int)
  ] 
  where
    mdl_name = "main:DART.Examples.GHC.Num"
    
-- | Expected extracted DataTypes coming from examples/testing/OnTrees.hs
trees_ec :: [(Id,Maybe GeneralType)] 
trees_ec = [
  (mdl_name @@ "myTree", Just $ CType intTree)
  , (mdl_name @@ "sumTreeI", intTree `toConcrete` int)
  ]
  where
    mdl_name = "main:DART.Examples.Testing.OnTrees" 
    intTree = DType . DataType $ "main:DART.Examples.Testing.OnTrees.IntTree" 
    
toConcrete :: ConcreteType -> ConcreteType -> Maybe GeneralType
toConcrete a b = Just . Lambda $ LambdaAbstraction a (CType b)

int :: ConcreteType
int = DType . DataType $ "ghc-prim:GHC.Types.Int"
integer = DType . DataType $ "integer-gmp:GHC.Integer.Type.Integer"
bool = DType . DataType $ "ghc-prim:GHC.Types.Bool"
intP :: PrimitiveType
intP = PrimitiveIntType "ghc-prim:GHC.Types.Int"

listOf :: ConcreteType -> ConcreteType
listOf = DList . ListOf

convertIdTy :: (Id,Ty) -> (Id,Maybe GeneralType)
convertIdTy (id,ty) = (id, extractType ty)
