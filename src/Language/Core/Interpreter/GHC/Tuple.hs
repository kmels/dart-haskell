module Language.Core.Interpreter.GHC.Tuple where

import Language.Core.Core
import Language.Core.Interpreter.Structures
import Language.Core.Interpreter.Apply
import Language.Core.Util
-- (,)

-- tupleConstructor :: Env -> (Id, Either Thunk Value)
-- tupleConstructor env = (id, Right val) where
--   id = "ghc-prim:GHC.Tuple.(,)"
--   mkPair :: Id -> Id -> IM Value
--   mkPair xid yid = do
--     x <- lookupId xid env
--     y <- lookupId yid env
--     return $ Pair x y
--   tuple xid = Fun (mkPair xid) "(,) :: a -> b -> (a,b)"
--   val = Fun (return . tuple) "(,) :: a -> b -> (a,b)"

-- all :: [Env -> (Id, Either Thunk Value)]
-- all = [ tupleConstructor 
--       ]
