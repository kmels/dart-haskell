module Language.Core.Interpreter.GHC.CString where

import Language.Core.Interpreter.Structures
import Language.Core.Core
import Language.Core.Interpreter.Apply

-- | Evaluate a function variable in ghc-prim:GHC.CString
evalVar :: Qual Var -> Maybe Value

evalVar ((Just (M (P ("ghczmprim"),["GHC"],"CString"))),"unpackCStringzh") = let
  in return $ Fun return "unpackString# = id"

evalVar ((Just (M (P ("ghczmprim"),["GHC"],"CString"))),"unpackCStringUtf8zh") = let
  in return $ Fun return "unpackStringUtf8# = id"
  
evalVar _ = Nothing
