----------------------------------------------------------------------------
-- |
-- Module      :  Language.Core.Interpreter.Libraries.Monomophy
-- Copyright   :  (c) Carlos López-Camey, University of Freiburg
-- License     :  BSD-3
--
-- Maintainer  :  c.lopez@kmels.net
-- Stability   :  stable
--
--
-- Exports functions that convert polymorphic functions e.g. (+) :: Num a -> a -> a -> a
-- to monomorphic functions e.g. (+) :: Int -> Int -> Int
-----------------------------------------------------------------------------

module Language.Core.Interpreter.Libraries.Monomophy(
  mkMonomophier
  , monomophy_1, monomophy_2
  ) where

import DART.CmdLine
import Language.Core.Interpreter(evalId)
import Language.Core.Interpreter.Structures
import Language.Core.Interpreter.Util(return')
-- | Create a function that is indented to be passed to a polymorphic function
-- to create a monomorphic function. 
mkMonomophier :: Id -> (Id, Either Thunk Value)
mkMonomophier id = (id, Right $ Fun (evalId) (idName id))

--monomophy :: Value -> Id -> Env -> IM Value
--monomophy v = \i e -> return 

-- | Converts a polymorphic unary function on values to a monomorphic one.
monomophy_1 :: String -> (Value -> IM Value) -> Id -> Env -> IM Value
monomophy_1 d f id env = evalId id env >>= return . f_unary
  where
    f_unary ty = Fun (\i e -> evalId i e >>= f') (mkMonoDesc "unary" d ty)

    -- before calling f, check if the argument holds an error
    f' :: Value -> IM Value
    f' x = case x of
      (Wrong _) -> return x
      _ -> f x


mkMonoDesc :: String -> String -> Value -> String
mkMonoDesc arity f_name ty = arity ++ " " ++ f_name ++ " @ " ++ show ty

-- | Converts a polymorphic binary function on values to a monomorphic one.
monomophy_2 :: String -> (Value -> Value -> IM Value) -> Id -> Env -> IM Value
monomophy_2 d f id env = evalId id env >>= return . f_binary                       
  where
    f_unary ty x = return $ Fun (\i e -> evalId i e >>= f' x) (mkMonoDesc "unary" d ty)
    f_binary ty = Fun (\i e -> evalId i e >>= f_unary ty) (mkMonoDesc "binary" d ty)
  -- tyclass_val <- evalId id env -- e.g. Var("zdfNumInt") => Fun(f :: (Num a) -> Int) "Int"
--   debugM $ id ++ " = " ++ show tyclass_val
--   let
--     -- before calling f, check whether any argument holds an error
    f' :: Value -> Value -> IM Value
    f' v1 v2 = case v1 of
      (Wrong _) -> return v1
      _ -> case v2 of
        (Wrong _) -> return v2
        _ -> f v1 v2
    
-- --    tyclass_show = show tyclass_val  
      --f_unary tcv x = return $ Fun (\i -> \e -> evalId i e >>= f' x) (show tcv ++ " Unary.")
--     f_binary = Fun (\tcv_i -> \tcv_e -> evalId tcv_i tcv_e >>= f' tyclass_val) (show tyclass_val ++ " monomophier")
--   return $ f_binary
