module Language.Core.Interpreter.GHC.Types where

import Language.Core.Interpreter.Structures
import Language.Core.Util
import Language.Core.Core

-- | Evaluates definitions found in ghc-prim:GHC.Types  

cons :: (Id,Either Thunk Value) -- (:) :: a -> [a] -> [a]
cons = (id,Right val) where
  id = "ghc-prim:GHC.Types.:"
  -- type parameters 
  typeArgs :: [Ty]
  typeArgs = [Tvar "a", Tvar "[a]"]
  val = TyCon $ AlgTyCon id typeArgs

listConstructor :: (Id,Either Thunk Value) -- ([]) :: [a], kind * -> *
listConstructor = (id,Right val) where
  id = "ghc-prim:GHC.Types.[]"
  -- type parameters 
  typeArgs :: [Ty]
  typeArgs = [Tvar "a"]
  val = TyCon $ AlgTyCon id typeArgs
  
all :: [(Id, Either Thunk Value)]
all = [ cons
        , listConstructor 
      ]
