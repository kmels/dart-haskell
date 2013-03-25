module Language.Core.Interpreter.Libraries.GHC.Tuple where

import Language.Core.Core
import Language.Core.Interpreter.Structures
import Language.Core.Interpreter.Apply
import Language.Core.Util

tupleConstructor :: (Id, Either Thunk Value)
tupleConstructor = (id, Right $ Fun (\i e -> getPointer i e >>= mkPair_1) "unary(,)") 
  where    
    id = "ghc-prim:GHC.Tuple.(,)"
    
    mkPair_1 :: Value -> IM Value
    mkPair_1 x = return $ Fun (\i e -> getPointer i e >>= mkPair_2 x) "binary(,)"  
               
    mkPair_2 :: Value -> Value -> IM Value
    mkPair_2 (Pointer x) (Pointer y) = return $ Pair x y
    mkPair_2 w@(Wrong _) _ = return w
    mkPair_2 _ w@(Wrong _) = return w
    mkPair_2 _ _  = return . Wrong $ "tupleConstructor: The impossible happened"
    
all :: [(Id, Either Thunk Value)]
all = [ tupleConstructor 
      ]
