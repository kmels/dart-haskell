module Language.Core.Interpreter.Libraries.GHC.CString where

import Language.Core.Interpreter.Structures
import Language.Core.Interpreter(evalId)
import Language.Core.Core
import Language.Core.Interpreter.Apply

all :: [(Id, Either Thunk Value)]
all = [ unpackCString
        , unpackCStringUtf8
      ]
      
unpackCString = (id, Right val) where
  id = "ghc-prim:GHC.CString.unpackCString#"
  val = Fun (\i e -> evalId i e >>= return) "unpackString#"

unpackCStringUtf8 = (id,Right val) where
  id = "ghc-prim:GHC.CString.unpackCStringUtf8#"
  val = Fun (\i e -> evalId i e >>= return) "unpackStringUtf8#"
  

