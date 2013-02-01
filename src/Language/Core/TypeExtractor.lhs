----------------------------------------------------------------------------
-- |
-- Module      :  Language.Core.TypeExtractor
-- Copyright   :  (c) Carlos López-Camey, University of Freiburg
-- License     :  BSD-3
--
-- Maintainer  :  c.lopez@kmels.net
-- Stability   :  stable
--
--
-- Types in extcore (Language.Core.Ty) are not refied. We only know 
their names and the packages they come from.

So if we have a type [Char] (list of Chars), we might only get "ghc-prim:GHC.Prim.(->)\n(ghc-prim:GHC.Types.[] ghc-prim:GHC.Types.Char)" from a Ty. We use regular expressions to convert their representation to concrete data types in order to be able to do pattern matching (e.g. in DART.FunctionFeeder).
-----------------------------------------------------------------------------

> module Language.Core.TypeExtractor where

We use regex-applicative to do the convertion.

> import Text.Regex.Applicative

> import Debug.Trace

> data GeneralType = CType ConcreteType | Lambda LambdaAbstraction | NoType String deriving Show
> data ConcreteType = PList PrimitiveList | PType PrimitiveType deriving Show

> data PrimitiveType = PrimitiveCharType String 
>                      | PrimitiveBoolType String
>                        deriving Show

> data PrimitiveList = PrimitiveList PrimitiveType deriving Show

> data LambdaAbstraction = LambdaAbstraction ConcreteType GeneralType deriving Show

> data GenericList = GenericList

> primitiveCharType :: RE Char PrimitiveType
> primitiveCharType = PrimitiveCharType <$> string "ghc-prim:GHC.Types.Char"

> primitiveBoolType :: RE Char PrimitiveType
> primitiveBoolType = PrimitiveBoolType <$> string "ghc-prim:GHC.Types.Bool"

> primitiveType = trace "Debug: primitiveType" $ primitiveCharType <|> primitiveBoolType

> primitiveList :: RE Char PrimitiveList
> primitiveList = let
>   listConstructor = string $ "ghc-prim:GHC.Types.[]"
>   in trace "debug: primitiveList" $ PrimitiveList <$> (listConstructor *> primitiveType)

> concreteType :: RE Char ConcreteType
> concreteType = trace "debug: concreteType" $ (PList <$> primitiveList) <|> (PType <$> primitiveType)

> functionApplication :: RE Char LambdaAbstraction
> functionApplication = let
>   functConstructor = string $ "ghc-prim:GHC.Prim.(->)"
>   in trace "debug: functionApplication" $ LambdaAbstraction <$> (functConstructor *> concreteType) <*> ((CType . PType . PrimitiveCharType) <$> string "asdf")-- <*> generalType 

> generalType :: RE Char GeneralType
> generalType = trace "debug: General type" $ (Lambda <$> functionApplication) <|> (CType . PList <$> primitiveList)

The function to extract a type. The first argument must be a z-decoded string.

> extractType :: String -> Maybe GeneralType
> extractType ty = trace ("Doing "++ty) $ ty =~ generalType

