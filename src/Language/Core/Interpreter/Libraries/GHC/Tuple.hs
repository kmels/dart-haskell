module Language.Core.Interpreter.Libraries.GHC.Tuple where

import Language.Core.Core
import Language.Core.Interpreter.Structures
import Language.Core.Interpreter.Apply
import Language.Core.Util

tupleConstructor :: (Id, Either Thunk Value)
tupleConstructor = (id, Right $ Fun (\i e -> lookupId i e >>= mkPair) "(,)") 
  where
    id = "ghc-prim:GHC.Tuple.(,)"
    mkPair :: (Either Thunk Value) -> IM Value
    mkPair x = return $ Fun (\i e -> lookupId i e >>= return . (Pair x)) "unary(,)"  

all :: [(Id, Either Thunk Value)]
all = [ tupleConstructor 
      ]
