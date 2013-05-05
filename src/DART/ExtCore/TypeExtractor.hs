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
  , showExtCoreType
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
extractType  = extractZDecodedType . zDecodeString . showExtCoreType 

-- | A function to extract a type. The first argument must be a z-decoded string.
extractZDecodedType :: String -> Maybe GeneralType
extractZDecodedType ty = case (parse generalType "" ty) of
  Left err -> Nothing
  Right t -> Just t

-- | A parser combinator that identifies the Char primitive type
primitiveCharType :: Parser PrimitiveType
primitiveCharType = string "ghc-prim:GHC.Types.Char" >>= return . PrimitiveCharType

-- | A parser combinator that identifies the Int primitive type
primitiveIntType :: Parser PrimitiveType
primitiveIntType = string "ghc-prim:GHC.Types.Int" >>= return . PrimitiveIntType

-- | A parser combinator that identifies the Bool primitive type
primitiveBoolType :: Parser PrimitiveType
primitiveBoolType = string "ghc-prim:GHC.Types.Bool" >>= return . PrimitiveBoolType

--primitiveType = try primitiveIntType <|> try primitiveCharType <|> try primitiveBoolType

dataType :: Parser String
dataType = do
  string "Tcon"
  char '('
  id <- tconId 
  char ')'
  return $ id
  where 
    -- Parses anything until a closing parens
    tconId :: Parser String
    tconId = do { 
      c <- noneOf ")"; 
      do { cs <- tconId; return (c:cs) } <|> return [c]
      }

-- | A Primitive list has kind * and no parametric polymorphism associated. That is, it represents a list of primitive types.
-- primitiveList :: Parser PrimitiveList
-- primitiveList = let
--   listConstructor = string $ "ghc-prim:GHC.Types.[]"
--   in do
--     lc <- listConstructor 
--     pt <- primitiveType
--     return $ PrimitiveList pt

-- | A concrete type is any type of kind *.
concreteType :: Parser ConcreteType
concreteType = 
--  (try dataList >>= return . PList)
--  <|> (try primitiveType >>= return . PType)
  (try dataType >>= return . DType . DataType)

-- | A Lambda abstraction is a ready-to-be beta-reduced lambda abstraction. That is, there is no unapplied arguments to the function arrow, and has kind *.

lambdaAbstraction :: Parser LambdaAbstraction
lambdaAbstraction = do
  functionConstructor <- string $ "Tcon(ghc-prim:GHC.Prim.(->))"
  functionDomain <- concreteType
  functionCodomain <- generalType
  return $ LambdaAbstraction functionDomain functionCodomain


generalType :: Parser GeneralType
generalType = do 
  fa <- try lambdaAbstraction
  return $ Lambda fa
  -- <|>(try primitiveList >>= return . CType . PList)
  <|> (concreteType >>= return . CType)
