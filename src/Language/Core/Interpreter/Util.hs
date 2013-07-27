----------------------------------------------------------------------------
-- |
-- Module      :  Language.Core.Interpreter.Util
-- Copyright   :  (c) Carlos López-Camey, University of Freiburg
-- License     :  BSD-3
--
-- Maintainer  :  c.lopez@kmels.net
-- Stability   :  stable
--
--
-- Useful combinators to use in Language.Core.Interpreter or even
-- Language.Core.Interpreter.Libraries.{GHC.*}
-- 
-- Show instances for data types in Language.Core.Interpreter.Structures
-----------------------------------------------------------------------------

module Language.Core.Interpreter.Util where

--------------------------------------------------------------------------------
-- base type funs
import           Data.Either(partitionEithers,rights)
import           Data.List(findIndices,intersperse)
import           Prelude hiding (showList)
--------------------------------------------------------------------------------
import DART.CmdLine(debugM)
import DART.Util.StringUtils(separateWithSpaces)
import Language.Core.Interpreter
import Language.Core.Interpreter.Apply
  
--import Language.Core.Interpreter.Structures
-- | A function that ignores its parameters and returns a value
-- the parenthesis in the signature have no effects and are only here to understand better 
return' :: Value -> (Id -> Env -> IM Value)
return' v = \_ -> \_ -> return v

-- | There are some values that contain addresses for which we must, in order to
-- pretty print the given value, look up their actual value in the heap
showValue :: Value -> IM String
showValue (TyConApp tc ptrs) = showTyConApp tc ptrs
showValue val = return $ show val

-- | Function called by showValue that handles the showing of a type constructor application.
-- Special cases include the List and the Tuple constructors.
-- As we know from the semantics, the showing forces the evaluation of the arguments of the data constructor
showTyConApp :: DataCon-> [Pointer] -> IM String
showTyConApp (MkDataCon "ghc-prim:GHC.Types.[]" _) [] = return "[]" -- empty list
showTyConApp (MkDataCon "ghc-prim:GHC.Types.:" _) ptrs = showList ptrs -- lists
showTyConApp (MkDataCon "ghc-prim:GHC.Tuple.Z2T" _) [x,y] = do
  x_str <- evalPtr x >>= showValue
  y_str <- evalPtr y >>= showValue
  return $ show (x_str,y_str) 
showTyConApp tycon pointers = do
  vals <- mapM evalPtr pointers -- [Value]  
  whnf_strings <- mapM showValue' vals -- [String]
  
  return $ let
    tycon_name = (idName . dataConId) tycon
    arg_strings = separateWithSpaces whnf_strings
    in tycon_name ++ " " ++ arg_strings
    
  where
    -- Should we wrap a value in parenthesis? Wrap the tycon apps! (iff they have applied vals)
    showValue' :: Value -> IM String
    showValue' t@(TyConApp tycon []) = return . idName . dataConId $ tycon
    showValue' t@(TyConApp _ _) = showValue t >>= return . wrapInParenthesis
    showValue' v = showValue v

evalPtr :: Pointer -> IM Value
evalPtr = flip eval []

-- | Function in charge of showing the application of the type constructor "ghc-prim:GHC.Types.:"
showList :: [Pointer] -> IM String
showList ptrs = do
  elem_strs <- mapM showPtr ptrs  
  return $ "[" ++ separateWithSpaces elem_strs ++ "]"
  where
    showPtr :: Pointer -> IM String
    showPtr ptr = evalPtr ptr >>= showValue'
    
    -- If we find an empty list, we ought not show it as [] but rather as the empty string
    -- If we find another list, don't show the []
    showValue' :: Value -> IM String
    showValue' t@(TyConApp (MkDataCon "ghc-prim:GHC.Types.[]" _) []) = return ""
    showValue' t@(TyConApp (MkDataCon "ghc-prim:GHC.Types.[]" _) ty) = return $ "[] to " ++ show ty
    showValue' t@(TyConApp (MkDataCon "ghc-prim:GHC.Types.:" _) ptrs) = mapM showPtr ptrs >>= return . separateWithSpaces
    showValue' t@(TypeConstructor (MkDataCon "ghc-prim:GHC.Types.[]" [_]) "ghc-prim:GHC.Types.[]")= return ""
    showValue' v = showValue v
    
wrapInParenthesis s = "(" ++ s ++ ")"

-- | Take a qualified name and return only its last name. E.g. idName "main.Module.A" = "A"
idName :: Id -> String
idName id = let 
  name = drop (lastDotIndex id + 1) id -- name with a possible parenthesis at the end  
  in case name of 
    ":" -> "(:)"
    _ -> if (last name == ')')
         then init name
         else name
  where
    isDot = ((==) '.')
    dotIndexes = findIndices isDot
    lastDotIndex = last . dotIndexes
