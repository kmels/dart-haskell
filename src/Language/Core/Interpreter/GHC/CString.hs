module Language.Core.Interpreter.GHC.CString where

import Language.Core.Interpreter.Structures
import Language.Core.Core
import Language.Core.Interpreter.Apply

unpackCString = (id, Right val) where
  id = "ghczmprim:GHC.CString.unpackCStringzh"
  val = Fun return "unpackString# = id"

unpackCStringUtf8 = (id,Right val) where
  id = "ghczmprim:GHC.CString.unpackCStringUtf8zh"
  val = Fun return "unpackStringUtf8# = id"
  
evalVar _ = Nothing

all :: [(Id, Either Thunk Value)]
all = [ unpackCString
        , unpackCStringUtf8
      ]
