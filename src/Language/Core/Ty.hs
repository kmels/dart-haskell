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
import Language.Core.Util(showExtCoreType,showExtCoreTypeVerbose,zDecodeQualified)
import Text.Encoding.Z(zDecodeString)
import Data.Maybe(isJust,fromJust)

--------------------------------------------------------------------------------
-- DART
import Language.Core.Interpreter.Structures
import Language.Core.Interpreter.Util(idName)

-- | Z-Decodes a Tvar or otherwise shows the type
qualTypeName :: Ty -> String
qualTypeName (Tvar ty) = zDecodeString ty
qualTypeName ty = showExtCoreType ty

-- | Returns true if the given type is primitive
isPrimitive :: Ty -> Bool
isPrimitive (Tcon qual_tcon) = case zDecodeQualified qual_tcon of
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

-- | Given a type application of any arity, conform a list of its types
-- i.e. 
-- Nothing is returned if the given type is not a function type.
-- A function of type `Tree -> Int -> Maybe Int` returns
-- Just [Tcon("Tree"),Tcon("Int"),(TApp (Tcon "Maybe") (Tcon "Int"))]

funTyArgs :: Ty -> Maybe [Ty]
funTyArgs (Tapp i r) = funArgTy i >>= return . append (extractArrowReturnTy r)
  where
    append :: [a] -> a -> [a]
    append = flip (:)
    
    -- | Given a function type, return the types in its signature
    -- given another type, returns the type
    extractArrowReturnTy :: Ty -> [Ty]
    extractArrowReturnTy ty | isFunctionTy ty = fromJust $ funTyArgs ty
                            | otherwise = [ty]
                            
funTyArgs _ = Nothing

isFunctionTy :: Ty -> Bool
isFunctionTy (Tapp i _) = isJust . funArgTy $ i
isFunctionTy _ = False

-- | Given a type that is the applied arrow to some other type, return the other type. e.g. extractArrowArg `(-> a)` returns the type `a`. Returns nothing if the type is not the applied arrow
funArgTy :: Ty -> Maybe Ty
funArgTy (Tapp (Tcon ((Just (M (P ("ghczmprim"),["GHC"],"Prim"))),"ZLzmzgZR")) ty) = Just ty
funArgTy _ = Nothing

-- | Pretty prints a list of types as a type signature
printSignature :: [Ty] -> IM String
printSignature (t:tys) = liftM2 (++) (typeName t) (mapM typeName tys >>= return . prependArrows)

--lifttypeName t ++ prependArrows (map typeName ty)
-- (String -> String -> String) -> IM String -> IM String -> IM String
--liftM2 :: Monad m => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r

-- | Given a type, this function extracts its name as a string. 
typeName :: Ty -> IM String
typeName = return . idName . qualTypeName 

prependArrows :: [String] -> String
prependArrows [] = []
prependArrows [[]] = []    
prependArrows (y:ys) = " -> " ++ y ++ prependArrows ys
