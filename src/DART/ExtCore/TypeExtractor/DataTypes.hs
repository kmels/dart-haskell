----------------------------------------------------------------------------
-- |
-- Module      :  DART.ExtCore.TypeExtractor.DataTypes
-- Copyright   :  (c) Carlos López-Camey, University of Freiburg
-- License     :  BSD-3
--
-- Maintainer  :  c.lopez@kmels.net
-- Stability   :  stable
--
--
-- Data types used by DART.ExtCore.TypeExtractor
-----------------------------------------------------------------------------

module DART.ExtCore.TypeExtractor.DataTypes where

data GeneralType = CType ConcreteType | Lambda LambdaAbstraction | NoType String

data ConcreteType = PList PrimitiveList | PType PrimitiveType

data PrimitiveList = PrimitiveList PrimitiveType

-- | The arguments to the constructors is the qualified type name from which we identified this type
-- e.g. ghc-prim:GHC.Types.Int
data PrimitiveType = PrimitiveCharType String 
                      | PrimitiveBoolType String
                      | PrimitiveIntType String

data LambdaAbstraction = LambdaAbstraction ConcreteType GeneralType

data GenericList = GenericList

instance Show GeneralType where
  show (CType concrete_type) = show concrete_type
  show (Lambda lambda_abstraction) = show lambda_abstraction
  show (NoType s) = "No Type: " ++ s

instance Show LambdaAbstraction where
  show (LambdaAbstraction concrete_type general_type) = show concrete_type ++ " -> " ++ show general_type

instance Show ConcreteType where
  show (PType ptype) = show ptype
  show (PList plist) = show plist
  
instance Show PrimitiveList where
  show (PrimitiveList ptype) = "[" ++ show ptype ++ "]"
  
instance Show PrimitiveType where
  show (PrimitiveCharType _) = "Char"
  show (PrimitiveBoolType _) = "Bool"
  show (PrimitiveIntType _) = "Int"
  

