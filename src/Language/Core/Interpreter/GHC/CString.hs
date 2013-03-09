module Language.Core.Interpreter.GHC.CString where

import Language.Core.Interpreter.Structures
import Language.Core.Core
import Language.Core.Interpreter.Apply

-- unpackCString = (id, Right val) where
--   id = "ghc-prim:GHC.CString.unpackCString#"
--   val = Fun (return . String) "unpackString# = id"

-- unpackCStringUtf8 = (id,Right val) where
--   id = "ghc-prim:GHC.CString.unpackCStringUtf8#"
--   val = Fun (return . String) "unpackStringUtf8# = id"
  
-- evalVar _ = Nothing

-- all :: [(Id, Either Thunk Value)]
-- all = [ unpackCString
--         , unpackCStringUtf8
--       ]
