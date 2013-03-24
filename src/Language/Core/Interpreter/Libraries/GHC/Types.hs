module Language.Core.Interpreter.Libraries.GHC.Types where

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
  typeConstructor :: DataCon
  typeConstructor = MkDataCon id typeArgs
  val = TyConApp typeConstructor []

listConstructor :: (Id,Either Thunk Value) -- ([]) :: [a], kind * -> *
listConstructor = (id,Right $ TyConApp typeConstructor []) where
  id = "ghc-prim:GHC.Types.[]"
  -- type parameters 
  typeArgs :: [Ty]
  typeArgs = [Tvar "a"]
  typeConstructor :: DataCon
  typeConstructor = MkDataCon id typeArgs
  
true :: (Id, Either Thunk Value)
true = (id, Right $ TyConApp tc []) -- has no applied values
       where
         id = "ghc-prim:GHC.Types.True"
         tc = MkDataCon id [] -- awaits no types

false :: (Id, Either Thunk Value)
false = (id, Right $ TyConApp tc []) -- has no applied values
       where
         id = "ghc-prim:GHC.Types.False"
         tc = MkDataCon id [] -- awaits no types
         
all :: [(Id, Either Thunk Value)]
all = [ cons
        , listConstructor
        , true
        , false
      ]
