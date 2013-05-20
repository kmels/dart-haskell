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
import Data.List(findIndices)
import           Prelude hiding (showList)
--------------------------------------------------------------------------------
import Language.Core.Interpreter
import Language.Core.Interpreter.Apply

--import Language.Core.Interpreter.Structures
-- | A function that ignores its parameters and returns a value
-- the parenthesis in the signature have no effects and are only here to understand better 
return' :: Value -> (Id -> Env -> IM Value)
return' v = \_ -> \_ -> return v

-- | There are some values that contain addresses for which we must, in order to
-- pretty print the given value, look up their actual value in the heap
showM :: Value -> IM String
showM (TyConApp tc ptrs) = showTyConApp tc ptrs
showM val = return $ show val

showTyConApp :: DataCon-> [Pointer] -> IM String
showTyConApp tycon pointers = do
  values <- mapM lookupPtr pointers
  showTyConVals tycon values
  where
    showTyConVals :: DataCon-> [Either Thunk Value] -> IM String
    showTyConVals (MkDataCon "ghc-prim:GHC.Types.[]" []) [] = return "[]" -- empty list
    showTyConVals (MkDataCon "ghc-prim:GHC.Types.:" _) cns = showList cns -- lists
    showTyConVals (MkDataCon "ghc-prim:GHC.Tuple.Z2T" _) [x,y] = return $ show (x,y) -- tuples
    -- otherwise
    showTyConVals (MkDataCon tycon_name []) vals = return $ idName tycon_name ++ " " ++ showVals vals
    showTyConVals (MkDataCon tycon_name _) vals = return $ idName tycon_name ++ " " ++ showVals vals

showVals :: [Either Thunk Value] -> String
showVals vs = case partitionEithers vs of
  ([],vals) -> concatMap (wrapCons) vals
  (tnks,vals) -> concatMap (\tnk -> show tnk ++ " ") tnks ++ " ; " ++ concatMap (wrapCons) vals
  where
    wrapCons :: Value -> String
    wrapCons t@(TyConApp (MkDataCon _ []) _) = show $ t -- if tycon expects no types, don't wrap
    wrapCons t@(TyConApp _ _) = wrapInParenthesis . show $ t
    wrapCons v = show v ++ " "

showList :: [Either Thunk Value] -> IM String
showList elems = case partitionEithers elems of
  ([],[]) -> return $ "" -- no thunks, no vals
  ([],(head:t:[])) -> return $ "[" ++ show head ++ showTail t ++ "]" -- no thunks, only vals
  elems@(tnks,vals) -> do
    tnk_vals <- mapM (\t -> eval t []) tnks
    strs <- mapM showM $ tnk_vals
    --return . show $ strs
    return $ show elems
    --strs <- mapM showM tnk_vals -- thunks and vals
    --return $ show $ strs 
    --return $ show tnk_vals
  where
    showTail :: Value -> String    
--    showTail (TyConApp (MkDataCon "ghc-prim:GHC.Types.:" _) ((Right th):(Right tt):[])) = "," ++ show th ++ showTail tt
--    showTail (TyConApp (MkDataCon "ghc-prim:GHC.Types.[]" _) []) = ""
    showTail w@(Wrong _) = "," ++ show w
    showTail xs = "????\t\t\t" ++ show xs ++ " \t\t\t"
    
wrapInParenthesis s = "(" ++ s ++ ")"

-- | Take a qualified name and return only its last name. E.g. idName "main.Module.A" = "A"
idName :: Id -> String
idName id = let 
  name = drop (lastDotIndex id + 1) id 
  in case name of 
    ":" -> "(:)"
    _ -> name
  where
    isDot = ((==) '.')
    dotIndexes = findIndices isDot
    lastDotIndex = last . dotIndexes
