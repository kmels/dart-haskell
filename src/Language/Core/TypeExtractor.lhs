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

So if we have a type [Char] (list of Chars), we might only get "ghc-prim:GHC.Prim.(->)\n(ghc-prim:GHC.Types.[] ghc-prim:GHC.Types.Char)" from a Ty. We use parser combinators to convert their representation to concrete data types in order to be able to do pattern matching (e.g. in DART.FunctionFeeder).
-----------------------------------------------------------------------------

> module Language.Core.TypeExtractor where

We use Parsec to do the convertion.

> import Text.Parsec
> import Text.Parsec.String

> import Debug.Trace

> data GeneralType = CType ConcreteType | Lambda LambdaAbstraction | NoType String deriving Show
> data ConcreteType = PList PrimitiveList | PType PrimitiveType deriving Show

> data PrimitiveType = PrimitiveCharType String 
>                      | PrimitiveBoolType String
>                        deriving Show

> data PrimitiveList = PrimitiveList PrimitiveType deriving Show

> data LambdaAbstraction = LambdaAbstraction ConcreteType GeneralType deriving Show

> data GenericList = GenericList

> primitiveCharType :: Parser PrimitiveType
> primitiveCharType = string "ghc-prim:GHC.Types.Char" >>= return . PrimitiveCharType

> primitiveBoolType :: Parser PrimitiveType
> primitiveBoolType = string "ghc-prim:GHC.Types.Bool" >>= return . PrimitiveBoolType

> primitiveType = trace "Debug: primitiveType" $ primitiveCharType <|> primitiveBoolType

> primitiveList :: Parser PrimitiveList
> primitiveList = let
>   listConstructor = string $ "ghc-prim:GHC.Types.[]"
>   in trace "debug: primitiveList" $ do
>     lc <- listConstructor 
>     pt <- primitiveType
>     return $ PrimitiveList pt

PrimitiveList <$> (listConstructor *> primitiveType)

> concreteType :: Parser ConcreteType
> concreteType = trace "debug: concreteType" $ 
>   (try primitiveList >>= return . PList)
>   <|> (primitiveType >>= return . PType)

A Lambda application is a concrete lambda application. That is, there is no unapplied arguments to the function arrow, and has kind *.

> functionApplication :: Parser LambdaAbstraction
> functionApplication = trace "debug: functionApplication" $ do
>   functionConstructor <- string $ "ghc-prim:GHC.Prim.(->)"
>   functionDomain <- concreteType
>   functionCodomain <- generalType
>   return $ LambdaAbstraction functionDomain functionCodomain

LambdaAbstraction <$> (functConstructor *> concreteType) <*> ((CType . PType . PrimitiveCharType) <$> string "asdf")-- <*> generalType 

> generalType :: Parser GeneralType
> generalType = trace "debug: General type" $ do 
>   fa <- try functionApplication
>   return $ Lambda fa
>   <|> (primitiveList >>= return . CType . PList)
>   <|> (concreteType >>= return . CType)

(Lambda <$> functionApplication) <|> (CType . PList <$> primitiveList)

The function to extract a type. The first argument must be a z-decoded string.

> extractType :: String -> Maybe GeneralType
> extractType ty = trace ("Doing "++ty) $ case (parse generalType "" ty) of
>   Left err -> trace (show err) $ Nothing
>   Right t -> Just t

