module Language.Core.Interpreter.GHC.Tuple where

import Language.Core.Core
import Language.Core.Interpreter.Structures
import Language.Core.Util
-- (,)

tupleConstructor :: (Id, Either Thunk Value)
tupleConstructor = (id, Right val) where
  id = "ghc-prim:GHC.Tuple.(,)"
  tuple x = Fun (\y -> return $ Pair x y) "(,) :: a -> b -> (a,b)"
  val = Fun (\x -> return $ tuple x) "(,) :: a -> b -> (a,b)"

all :: [(Id, Either Thunk Value)]
all = [ tupleConstructor 
      ]
