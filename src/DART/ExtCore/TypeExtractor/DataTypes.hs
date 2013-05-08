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

data GeneralType = CType ConcreteType | Lambda LambdaAbstraction deriving Eq -- should dissapear, a lambda is acknowledged when an expression is evaluated

data ConcreteType = DList DataList | PType PrimitiveType | DType DataType deriving Eq

data DataType = DataType String deriving Eq

data DataList = ListOf ConcreteType | PrimitiveList PrimitiveType deriving Eq

-- | The arguments to the constructors is the qualified type name from which we identified this type
-- e.g. ghc-prim:GHC.Types.Int
data PrimitiveType = PrimitiveCharType String 
                      | PrimitiveBoolType String
                      | PrimitiveIntType String deriving Eq

data LambdaAbstraction = LambdaAbstraction ConcreteType GeneralType deriving Eq

data GenericList = GenericList

instance Show GeneralType where
  show (CType concrete_type) = show concrete_type
  show (Lambda lambda_abstraction) = show lambda_abstraction

instance Show LambdaAbstraction where
  show (LambdaAbstraction concrete_type general_type) = show concrete_type ++ " -> " ++ show general_type

instance Show ConcreteType where
  show (PType ptype) = show ptype
  show (DList plist) = show plist
  show (DType dataType) = show dataType
  
instance Show DataType where
  show (DataType dt) = dt
  
instance Show DataList where
  show (ListOf dtype) = "[" ++ show dtype ++ "]"
  
instance Show PrimitiveType where
  show (PrimitiveCharType _) = "Char"
  show (PrimitiveBoolType _) = "Bool"
  show (PrimitiveIntType c) = c
  

