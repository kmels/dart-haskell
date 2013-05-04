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
import Language.Core.Util(showExtCoreType)
import Text.Encoding.Z(zDecodeString)

-- | Z-Decodes a Tvar or otherwise shows the type
typeName :: Ty -> String
typeName (Tvar ty) = zDecodeString ty
typeName ty = showExtCoreType ty

-- | Returns true if the given type is primitive
isPrimitive :: Ty -> Bool
isPrimitive (Tvar "ghczmprim:GHCziTypesziChar") = True
isPrimitive (Tvar "ghczmprim:GHCziTypesziInt") = True
isPrimitive (Tvar "ghczmprim:GHCziTypesziBool") = True
isPrimitive _ = False

-- | Checks whether "ghc-prim:GHC.Prim.(->)" is in the left-most applied type
isLambdaArrow :: Ty -> Bool
--isLambdaArrow (Tapp (Tvar "ghczmprim:GHCziPrim.ZLzmzgZR") _) = True
isLambdaArrow (Tapp (Tcon(ty)) _) | isTheArrow ty = True
  where
    isTheArrow ((Just (M (P ("ghczmprim"),["GHC"],"Prim"))),"ZLzmzgZR") = True
    isTheArrow _ = False
isLambdaArrow (Tapp ty _)  = isLambdaArrow ty
isLambdaArrow _ = False
