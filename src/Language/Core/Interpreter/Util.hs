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
import DART.Util.StringUtils(separateWithSpaces,separateWithCommas)
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
showValue (TyConApp tc ptrs) = do
  --io $ putStrLn "tyconapp.."
  showTyConApp tc ptrs
showValue val = do
  {-io $ putStrLn "not tyconapp?"
  case val of 
    tc@(TyCon tycon ty_name) -> do
      io . putStrLn $ "TyCon " ++ (show tycon) ++ ty_name
      io . putStrLn . show$ tc
      io $ putStrLn "1"
    SumType _ -> io $ putStrLn "2"
    MkListOfValues _ -> io $ putStrLn "3"
    FreeTypeVariable _ -> io $ putStrLn "4"
    Pointer _ -> io $ putStrLn "5"
    Pair _ _ -> io $ putStrLn "6"
    Fun _ _ -> io $ putStrLn "7"
    String _ -> io $ putStrLn "8"
    Char _ -> io $ putStrLn "9"
    Boolean _ -> io $ putStrLn "10"
    Rat _ -> io $ putStrLn "11"
    Wrong _ -> io $ putStrLn "41" -}
    
  return $ show val

-- | Function called by showValue that handles the showing of a type constructor application.
-- Special cases include the List and the Tuple constructors.
-- As we know from the semantics, the showing forces the evaluation of the arguments of the data constructor
showTyConApp :: DataCon-> [Pointer] -> IM String
showTyConApp (MkDataCon "ghc-prim:GHC.Types.[]" _ _) [] = return "[]" -- empty list
showTyConApp (MkDataCon "ghc-prim:GHC.Types.:" (ty:_) _) ptrs = showList ty ptrs
showTyConApp (MkDataCon "ghc-prim:GHC.Tuple.Z2T" _ _) [x,y] = do
  x_str <- evalPtr x >>= showValue
  y_str <- evalPtr y >>= showValue
  return $ show (x_str,y_str) 
showTyConApp tycon@(MkDataCon datacon_name' signature applied_types) ptrs = do
  debugM $ " Constructor " ++ (show datacon_name') ++ " , signature: " ++ (show signature) ++ ", applied: " ++ (show applied_types) ++ ", to pointers: " 
  
  vals <- mapM evalPtr ptrs -- [Value]  
  mapM (showValue) vals >>= debugM . show
  whnf_strings <- mapM showValue' vals -- [String]
  
  return $ let
    tycon_name = idName datacon_name'
    arg_strings = separateWithSpaces whnf_strings
    in tycon_name ++ " " ++ arg_strings
    
  where
    -- Should we wrap a value in parenthesis? Wrap the tycon apps! (iff they have applied vals)
    showValue' :: Value -> IM String
    showValue' t@(TyConApp tycon []) = return . idName . datacon_name $ tycon
    showValue' t@(TyConApp _ _) = showValue t >>= return . wrapInParenthesis
    showValue' v = showValue v

evalPtr :: Pointer -> IM Value
evalPtr = flip eval []

-- | Function in charge of showing the application of the type constructor "ghc-prim:GHC.Types.:"
showList :: Ty -> [Pointer] -> IM String
showList ty ptrs = do
  elem_strs <- mapM (showPtr ?? showValue) ptrs
  case ty of
    Tvar("ghc-prim:GHC.Types.Char") -> return $ "\"" ++ map (!! 1) elem_strs ++ "\""
    _ -> return $ "[" ++ separateWithCommas elem_strs ++ "]"
      
    -- If we find an empty list, we ought not show it as [] but rather as the empty string
  where
    -- If we find another list, don't show the []
    showValue' :: Value -> IM String
    showValue' t@(TyConApp (MkDataCon "ghc-prim:GHC.Types.[]" _ _) []) = return ""
    showValue' t@(TyConApp (MkDataCon "ghc-prim:GHC.Types.[]" _ _) [ptr]) = (showPtr ?? showValue') ptr       
    showValue' t@(TyConApp (MkDataCon "ghc-prim:GHC.Types.:" _ _) ptrs) = mapM (showPtr ?? showValue') ptrs >>= return . separateWithSpaces
    showValue' t@(TyCon (MkDataCon "ghc-prim:GHC.Types.[]" [] _) "ghc-prim:GHC.Types.[]")= return ""
    showValue' v = showValue v
  
-- from the operators of the package `lens`
(??) :: Functor f => f (a -> b) -> a -> f b
fab ?? a = fmap ($ a) fab

showPtr :: Pointer -> (Value -> IM String) -> IM String
showPtr ptr showValue' = evalPtr ptr >>= showValue'

wrapInParenthesis :: String -> String  
wrapInParenthesis s = "(" ++ s ++ ")"

-- | Take a qualified name and return only its last name. E.g. idName "main.Module.A" = "A"
idName :: Id -> String
idName id = let 
  name = drop (lastDotIndex id + 1) id -- name with a possible parenthesis at the end  
  in case name of 
    [] -> id
    ":" -> "(:)"    
    _ -> if (last name == ')')
         then init name
         else name
  where
    isDot = ((==) '.')
    dotIndexes = findIndices isDot
    lastDotIndex s = 
      case dotIndexes s of
        [] -> 0
        idxs -> last idxs
--    last . dotIndexes
