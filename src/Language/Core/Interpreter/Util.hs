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
import DART.CmdLine(debugM)
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
--  ([],(head:t:[])) -> return $ "[" ++ show head ++ showTail t ++ "]" -- no thunks, only vals
  elems@((t:ts:[]),vals) -> do  
    debugM $ " Printing list with " ++ (show . length) (t:ts:[]) ++ " thunks "
      ++ "and " ++ (show . length) vals ++ " values"
    
    debugM $ " head thunk == " ++ show t
    head_val <- eval t []
    debugM $ " head thunk eval == " ++ show head_val    
    head_str <- showValue head_val
    debugM $ " head_str == " ++ head_str    
                
    debugM $ " tail thunk == " ++ show ts
    tail <- eval ts []
    debugM $ " tail thunk val == " ++ show tail
    
    tail_str <- case tail of
      (TyConApp (MkDataCon "ghc-prim:GHC.Types.[]" _) []) -> do
        debugM $ "found the end"
        return $ ""
      t -> do
        debugM $ "Found no end: " ++ show tail
        showTail t
      
    --tail_str <- eval ts [] >>= showTail
    debugM $ " tail_str == " ++ head_str
    
    return $ "[" ++ head_str ++ tail_str ++ "]"
  where
    showTail :: Value -> IM String
    showTail (TyConApp (MkDataCon "ghc-prim:GHC.Types.[]" _) []) = return ""
    showTail (TyConApp (MkDataCon "ghc-prim:GHC.Types.:" _) (h:t:[])) = do
      head_str <- eval h [] >>= showValue
      
      debugM $ " Showing tail, head_str = " ++ head_str
      head <- lookupPtr h
      debugM $ " Showing tail, head = " ++ show head

      tail <- lookupPtr t
      debugM $ " Showing tail, tail = " ++ show tail
      tail_val <- eval t []
      debugM $ " Showing tail, tail_val = " ++ show tail_val
      tail_str <- showTail tail_val
      debugM $ " Showing tail, tail_str = " ++ tail_str
      
      return $ "," ++ head_str ++ tail_str
    
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
