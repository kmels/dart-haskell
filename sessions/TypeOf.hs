{-# LANGUAGE ImplicitParams #-}
----------------------------------------------------------------------------
-- |
-- Module      :  ExtCore.TypeOf
-- Copyright   :  (c) Carlos López-Camey, University of Freiburg
-- License     :  BSD-3
--
-- Maintainer  :  c.lopez@kmels.net
-- Stability   :  stable
--
--
-- This file is intended to be loaded with ghci to extract a type of a value definition
-- in a given module
--
-----------------------------------------------------------------------------

module ExtCore.TypeOf where

import DART.FileIO
import Language.Core.Core
import Language.Core.Util(qualifiedVar)
import Text.Encoding.Z(zDecodeString)

typeOf :: FilePath -> Id -> IO (Maybe Ty)
typeOf filepath fname = do
  let ?be_verbose = False 
  modul@(Module mname _ vdefs) <- readModule filepath
  let id = (zDecodeString $ show  mname) ++ "." ++ fname
  return $ lookup id (alltypes vdefs)  
  where
    alltypes :: [Vdefg] -> [(Id,Ty)]
    alltypes = concatMap types 
    types :: Vdefg -> [(Id,Ty)] 
    types (Nonrec vdef@(Vdef(qvar, ty, _))) = [(qualifiedVar qvar, ty)]
    types (Rec []) = []
    types (Rec (vdef@(Vdef(qvar, ty, _)):vs)) = [(qualifiedVar qvar, ty)] ++ types (Rec vs)
  

-- type Name = String
-- data Red = TApp Red Red | Tvar Name
-- data Black = Lambda Name Black | TyVar Name

-- data TyVar = Tvar String
-- data Ty_black = Lambda TyVar Ty_black | Tvar TVar 

-- red = TApp (TApp (Tvar "->") (TApp (TApp (Tvar "->") (Tvar "A")) (Tvar "B"))) (Tvar "C")

-- flip :: Red -> Black
-- flip (Tvar n) = TyVar n
-- flip (TApp t r) =  

-- -- | Returns the right node at the most lower level 
-- -- split (TApp "->" "A") == ("A", Nothing)
-- -- split (Tapp "->" (TApp tree "B")) 
-- split :: Red -> (Name,Maybe Red)
-- split (TApp (Tvar "->") (Tvar typ)) = (typ,Nothing)
-- split (TApp (Tvar "->") right) = case split right of
--   (name,tree) -> 


-- -- | Checks whether "ghc-prim:GHC.Prim.(->)" is in the left-most applied type
-- isLambdaArrow :: Ty_red -> Bool
-- --isLambdaArrow (Tapp (Tvar "ghczmprim:GHCziPrim.ZLzmzgZR") _) = True
-- isLambdaArrow (Tapp (Tvar "->") _) | isTheArrow ty = True
--   where
--     isTheArrow ((Just (M (P ("ghczmprim"),["GHC"],"Prim"))),"ZLzmzgZR") = True
--     isTheArrow _ = False
-- isLambdaArrow (Tapp ty _)  = isLambdaArrow ty
-- isLambdaArrow _ = False

