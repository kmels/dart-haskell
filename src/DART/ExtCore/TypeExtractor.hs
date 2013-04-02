----------------------------------------------------------------------------
-- |
-- Module      :  DART.Extcore.TypeExtractor
-- Copyright   :  (c) Carlos López-Camey, University of Freiburg
-- License     :  BSD-3
--
-- Maintainer  :  c.lopez@kmels.net
-- Stability   :  stable
--
--
-- Types in extcore (Language.Core.Ty) are not refied. We only know their names and the packages they come from.
--
-- So if we have a type [Char] (list of Chars), we might only get "ghc-prim:GHC.Prim.(->)\n(ghc-prim:GHC.Types.[] ghc-prim:GHC.Types.Char)" from a Ty. We use parser combinators to convert their representation to concrete data types in order to be able to do pattern matching (e.g. in DART.FunctionFeeder).
-----------------------------------------------------------------------------

module DART.ExtCore.TypeExtractor(
  extractType,
  module DART.ExtCore.TypeExtractor.DataTypes
  )where

-- | We use Parsec to do the parsing.

import Text.Parsec
import Text.Parsec.String

import DART.ExtCore.TypeExtractor.DataTypes

import Text.Encoding.Z(zDecodeString)
import Language.Core.Core(Ty(..))
import Language.Core.Util(showExtCoreType)
import Debug.Trace

-- | A function to extract a type from external core to a data type of our own
extractType :: Ty -> Maybe GeneralType
extractType = extractZDecodedType . zDecodeString . showExtCoreType 

-- | A function to extract a type. The first argument must be a z-decoded string.
extractZDecodedType :: String -> Maybe GeneralType
extractZDecodedType ty = trace ("Doing "++ty) $ case (parse generalType "" ty) of
  Left err -> trace (show err) $ Nothing
  Right t -> Just t

-- | A parser combinator that identifies the Char primitive type
primitiveCharType :: Parser PrimitiveType
primitiveCharType = trace "Debug: primitiveCharType" $ string "ghc-prim:GHC.Types.Char" >>= return . PrimitiveCharType

-- | A parser combinator that identifies the Int primitive type
primitiveIntType :: Parser PrimitiveType
primitiveIntType = trace "debug: primitiveIntType" $ string "ghc-prim:GHC.Types.Int" >>= return . PrimitiveIntType

-- | A parser combinator that identifies the Bool primitive type
primitiveBoolType :: Parser PrimitiveType
primitiveBoolType = trace "Debug: primitiveBoolType" $ string "ghc-prim:GHC.Types.Bool" >>= return . PrimitiveBoolType

primitiveType = trace "Debug: primitiveType" $ (try primitiveIntType) <|> (trace "\t missed primitiveInt" $ try primitiveCharType) <|> (try primitiveBoolType)

-- | A Primitive list has kind * and no parametric polymorphism associated. That is, it represents a list of primitive types.
primitiveList :: Parser PrimitiveList
primitiveList = let
  listConstructor = string $ "ghc-prim:GHC.Types.[]"
  in trace "debug: primitiveList" $ do
    lc <- listConstructor 
    pt <- primitiveType
    return $ PrimitiveList pt

-- | A concrete type is any type of kind *.
concreteType :: Parser ConcreteType
concreteType = trace "debug: concreteType" $ 
  (try primitiveList >>= return . PList)
  <|> (trace "\t missed primitiveList" $ (primitiveType >>= return . PType))

-- | A Lambda abstraction is a ready-to-be beta-reduced lambda abstraction. That is, there is no unapplied arguments to the function arrow, and has kind *.

lambdaAbstraction :: Parser LambdaAbstraction
lambdaAbstraction = trace "debug: lambdaAbstraction" $ do
  functionConstructor <- string $ "ghc-prim:GHC.Prim.(->)"
  functionDomain <- concreteType
  functionCodomain <- generalType
  return $ LambdaAbstraction functionDomain functionCodomain


generalType :: Parser GeneralType
generalType = trace "debug: General type" $ do 
  fa <- try lambdaAbstraction
  return $ Lambda fa
  <|>(try primitiveList >>= return . CType . PList)
  <|> (concreteType >>= return . CType)
