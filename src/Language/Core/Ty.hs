----------------------------------------------------------------------------
-- |
-- Module      :  Language.Core.Ty
-- Copyright   :  (c) Carlos López-Camey, University of Freiburg
-- License     :  BSD-3
--
-- Maintainer  :  c.lopez@kmels.net
-- Stability   :  stable
--
--
-- Functions on the Type datatype of Languagre.Core.Core
-----------------------------------------------------------------------------

module Language.Core.Ty where

import Language.Core.Core
import Language.Core.Util(showExtCoreType,showExtCoreTypeVerbose,showQualified)
import Text.Encoding.Z(zDecodeString)
import Data.Maybe(isJust,fromJust)

-- | Z-Decodes a Tvar or otherwise shows the type
typeName :: Ty -> String
typeName (Tvar ty) = zDecodeString ty
typeName ty = showExtCoreType ty

-- | Returns true if the given type is primitive
isPrimitive :: Ty -> Bool
isPrimitive (Tcon qual_tcon) = case showQualified qual_tcon of
  "ghc-prim:GHC.Types.Int" -> True
  "integer-gmp:GHC.Integer.Type.Integer" -> True
  _ -> False
--isPrimitive (Tvar "ghczmprim:GHCziTypesziChar") = True
--isPrimitive (Tvar "ghczmprim:GHCziTypesziInt") = True
--isPrimitive (Tvar "ghczmprim:GHCziTypes.Int") = True
--isPrimitive (Tvar "ghczmprim:GHCziTypesziBool") = True
isPrimitive _ = False

-- | Checks whether "ghc-prim:GHC.Prim.(->)" is in the left-most applied type
--isLambdaArrow :: Ty -> Bool
--isLambdaArrow (Tapp (Tvar "ghczmprim:GHCziPrim.ZLzmzgZR") _) = True
--isLambdaArrow (Tapp (Tcon(ty)) _) | isTheArrow ty = True
--  where
--    isTheArrow ((Just (M (P ("ghczmprim"),["GHC"],"Prim"))),"ZLzmzgZR") = True
--    isTheArrow _ = False
--isLambdaArrow (Tapp ty _)  = isLambdaArrow ty
--isLambdaArrow _ = False

-- | Given a function type of any arity, return all its types
-- e.g. a function whose signature is `Tree->Int->Maybe Int` returns
-- Just [Tcon("Tree"),Tcon("Int"),(TApp (Tcon "Maybe") (Tcon "Int"))]
-- Nothing is returned if the given type is not a function type
functionTyArgs :: Ty -> Maybe [Ty]
functionTyArgs (Tapp i r) = extractArrowArg i >>= return . append (extractArrowReturnTy r)
  where
    append :: [a] -> a -> [a]
    append = flip (:)
    
    -- | Given a function type, return the types in its signature
    -- given another type, returns the type
    extractArrowReturnTy :: Ty -> [Ty]
    extractArrowReturnTy ty | isFunctionTy ty = fromJust $ functionTyArgs ty
                            | otherwise = [ty]

isFunctionTy :: Ty -> Bool
isFunctionTy (Tapp i _) = isJust . extractArrowArg $ i
isFunctionTy _ = False

-- | Given a type that is the applied arrow to some other type, return the other type. e.g. extractArrowArg `(-> a)` returns the type `a`. Returns nothing if the type is not the applied arrow
extractArrowArg :: Ty -> Maybe Ty
extractArrowArg (Tapp (Tcon ((Just (M (P ("ghczmprim"),["GHC"],"Prim"))),"ZLzmzgZR")) ty) = Just ty
extractArrowArg _ = Nothing

