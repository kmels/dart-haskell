{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE FlexibleInstances #-}

----------------------------------------------------------------------------
-- |
-- Module      :  Language.Core.Util
-- Copyright   :  (c) Carlos López-Camey, University of Freiburg
-- License     :  BSD-3
--
-- Maintainer  :  c.lopez@kmels.net
-- Stability   :  stable
--
--
-- Helper function on External Core
----------------------------------------------------------------------------- 

module Language.Core.Util where

import DART.InterpreterSettings
import Language.Core.Core
import Language.Core.Printer
import Text.Encoding.Z
-- | Pretty prints a Ty (from extcore) where a function from of type `[Int] -> Int` is printed as:
-- "ghc-prim:GHC.Prim.(->)ghc-prim:GHC.Types.[]ghc-prim:GHC.Types.Intghc-prim:GHC.Types.Int"
-- This representation is used to reify types thorugh Language.Core.TypeExtractor.

showExtCoreType :: Ty -> String
showExtCoreType (Tvar t) = t
showExtCoreType (Tcon qcon) = qualifiedVar qcon
showExtCoreType (Tapp t1 t2) = showExtCoreType t1 ++ showExtCoreType t2
showExtCoreType _ = "UNKNOWN"

-- | Given a type string representation from ext core e.g. ghc-prim:GHC.Prim.(->) ghc-prim:GHC.Types.[] ghc-prim:GHC.Types.Int ghc-prim:GHC.Types.Int we transform it in a more readable string form e.g. [Int] -> Int

showType :: Ty -> String
showType (Tvar t) = t
-- constructors
showType (Tcon (Just (M ((P "ghczmprim"), ["GHC"], "Tuple")),"Z2T")) = "(,)"
showType (Tcon (Just (M ((P "ghczmprim"), ["GHC"], "Types")),"ZMZN")) = "[]"
-- a primitive type from GHC
showType (Tcon (Just (M ((P "ghczmprim"), ["GHC"], "Types")),primitiveType)) = primitiveType 
showType (Tcon (Just (M ((P "integerzmgmp"), ["GHC","Integer"], "Type")),"Integer")) = "Integer"
showType (Tcon qcon@(mname,t2)) = qualifiedVar qcon
-- a type constructor applied to a type parameter e.g. a list
showType (Tapp tc@(Tcon _) innerType') = 
  let 
    constructor = showType tc
    innerType = showType innerType'
  in case constructor of
    "[]" -> if (innerType == "Char") then "String" else "[" ++ innerType ++ "]"
    _ -> constructor ++ " " ++ innerType
-- a type constructor applied to two type parameters e.g. a Tuple
showType (Tapp ta@(Tapp tc@(Tcon _) ty2) ty1) = let
  constructor = showType tc
  in case constructor of
    "(,)" -> "(" ++ showType ty1 ++ "," ++ showType ty2 ++ ")"
    _ -> constructor ++ " " ++ showType ty1 ++ " " ++ showType ty2
    
showType (Tapp t1 t2) = wrapName "Tapp" $ showType t1 ++ " " ++ showType t2
showType _ = "showType, TODO"
wrapName s r = s ++ "(" ++ r ++ ")"

-- | Given an Exp from extcore, pretty prints it
showExp :: (?tab_indentation :: Int) => Exp -> String
showExp (Var qvar) = zDecodeString . snd $ qvar
showExp (Dcon qcon) = zDecodeString . snd $ qcon
showExp (Lit lit) = showLit lit
showExp (App exp1 exp2) = showAppExp exp1 exp2
showExp (Appt exp typ) = wrapName "appt" $ showExp exp ++ "," ++ showType typ --e.g. >= Int
showExp (Lam bind exp) = "\\" ++ showBind bind ++ " -> " ++ showExp exp
showExp (Let vdefg exp) = wrapName "let" $ showVdefg vdefg ++ showExp exp
showExp (Case exp (vbind_var,vbind_ty) ty alts) = "case " ++ 
                                                  showExp exp ++ " of " ++ 
                                                  vbind_var ++ "::" ++ showType vbind_ty ++
                                                  "\n" ++ concatMap (showAlt') alts
  where 
    showAlt' :: Alt -> String
    showAlt' = let
      ?tab_indentation = ?tab_indentation + 1
      in appendNewLine . tab . showAlt -- ++ "\n" ++ "\t\t" -- tab . newline . showAlt1
      
showExp (Cast exp ty) = wrapName "case" $ showExp exp ++ showType ty
showExp (Note msg exp) = wrapName "note" $ msg ++ showExp exp
showExp (External str ty) = wrapName "external" $ str ++ showType ty
--showExp _ = "NOT IMPLEMENTED YET"

-- | Receives two expressions and pretty prints a function application expression 
showAppExp :: (?tab_indentation :: Int) => Exp -> Exp -> String
showAppExp (Dcon qualCon) (Lit lit) | snd qualCon == "Izh" = showLit lit
                                    | snd qualCon == "Czh" = showLit lit
                                    | otherwise = "TODO: show showAppExp: " ++ show (snd qualCon)
showAppExp f x = wrapName "App" $ showExp f ++ "," ++ showExp x

showCoreLit :: CoreLit -> String
showCoreLit (Lint i) = show i
showCoreLit (Lrational r) = show r
showCoreLit (Lchar c) = show c
showCoreLit (Lstring s) = s

showBind :: Bind -> String
showBind (Vb (var,ty)) = var -- wrapName "Bind" $ var ++ "::" ++ showType ty
showBind (Tb tb@(tvar,kind)) = showTbind tb

showVdefg :: (?tab_indentation :: Int) => Vdefg -> String
showVdefg (Rec vdefs) = wrapName "Rec" $ concatMap showVdef vdefs
showVdefg (Nonrec vdef) = wrapName "Nonrec" $ showVdef vdef

showVdef :: (?tab_indentation :: Int) => Vdef -> String
showVdef (Vdef (qvar@(mname,var),ty,exp)) = qualifiedVar qvar ++ 
                                            "::" ++ showType ty ++ 
                                            " = " ++ showExp exp

showAlt :: (?tab_indentation :: Int) => Alt -> String
showAlt (Adefault exp) = wrapName "_ -> " $ showExp exp
showAlt (Alit lit exp) = showLit lit ++ " -> " ++ showExp exp

showAlt (Acon qcon tbinds vbinds exp) = snd qcon  ++
                                        concatMap (space . (wrapName "tbind") . fst) tbinds ++ " " ++ 
                                        concatMap (space . (wrapName "vbind") . fst) vbinds ++
                                        " -> " ++ showExp exp

-- wrapName "Acon" $ showMname mname ++ ", " ++ dcon ++ ", " ++ concatMap (\tb -> showTbind tb ++ ",") tbinds ++ concatMap (\vb ->showVbind vb ++ ",") vbinds ++ showExp exp

showTbind :: (Tvar,Kind) -> String
showTbind (tvar,kind) = tvar ++ " :: " ++ showKind kind

showVbind :: Vbind -> String
showVbind (var,ty) = wrapName "Vbind" $ var ++ "::" ++ showType ty

showKind :: Kind -> String
showKind Klifted = "Klifted"
showKind Kunlifted = "Kunlifted"
showKind Kopen = "Kopen"
showKind (Karrow k k') = wrapName "Karrow" $ showKind k ++ " -> " ++ showKind k'
showKind (Keq ty ty') = wrapName "Keq" $ showType ty ++ " -> " ++ showType ty'

showLit :: Lit -> String
showLit (Literal coreLit ty) = showCoreLit coreLit
 where
   showCoreLit :: CoreLit -> String
   showCoreLit (Lint i) = show i
   showCoreLit (Lrational r) = show r
   showCoreLit (Lchar c) = show c
   showCoreLit (Lstring s) = show s

qualifiedVar :: Qual Var -> String
qualifiedVar (Nothing,var) = var
qualifiedVar (Just mname,var) = zDecodeString $ show mname ++ "." ++ var

bindVarName :: Bind -> Id
bindVarName (Vb (var,ty)) = var
bindVarName (Tb (tvar,kind)) = tvar
{--
instance Show AnMname where 
  show (M ((P packageName),[],s2)) = packageName ++ ":" ++ s2
  show (M ((P packageName),[s1],s2)) = packageName ++ ":" ++ s1 ++ "." ++ s2
  show (M ((P packageName),ss,s2)) = packageName ++ ":" ++ show ss ++ "." ++ s2


instance Show Cdef where
  show (Constr (_,dcon) tbinds types) = 
    let 
      tbinds' = if (not . null $ tbinds) 
                then " :: forall(?) " ++ concatMap (\tb -> showTbind tb) tbinds
                else " " 
      showType' (Tvar t) = t
      showType' (Tapp (Tcon (_,tcon)) (Tvar t)) = tcon ++ " " ++ t
      showType' (Tapp dcon@(Tapp dt tv) ty) = showType' dcon ++ " " ++ showType' ty
      showType' x = showType x
      types' = if (not . null $ types)
               then concatMap (\t -> showType' t ++ " -> ") (init types) ++ (showType' . last $ types)
               else ""
    in dcon ++ tbinds' ++ types'

instance Show Ty where show = showExtCoreType
                       
instance Show Kind where show = showKind

instance Show Exp where
        show = showExp

-}

space = (++) " "
newline = (++) "\n"
appendNewLine = flip (++) "\n"
tab :: (?tab_indentation :: Int) => String -> String
tab = (++) (replicate ?tab_indentation '\t')

