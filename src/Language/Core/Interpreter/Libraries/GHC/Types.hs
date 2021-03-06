module Language.Core.Interpreter.Libraries.GHC.Types where

import Language.Core.Interpreter.Structures
import Language.Core.Util
import Language.Core.Core
import Language.Core.Interpreter(evalId)

-- | Evaluates definitions found in ghc-prim:GHC.Types  

cons :: (Id,Either Thunk Value) -- (:) :: a -> [a] -> [a]
cons = (cons_name, Right $ TyCon tycon ty_name) where
  cons_name = "ghc-prim:GHC.Types.:"
  ty_name = "ghc-prim:GHC.Types.[]"  
  tycon_args = [Tvar "a", Tvar "[a]"]
  tycon = MkDataCon cons_name tycon_args []
  
listConstructor :: (Id,Either Thunk Value) -- ([]) :: [a], kind * -> *
listConstructor = (cons_name, Right $ TyCon tycon ty_name) where
  cons_name = "ghc-prim:GHC.Types.[]"
  ty_name = "ghc-prim:GHC.Types.[]"
  tycon_args = [Tvar "a"]
  tycon = MkDataCon cons_name tycon_args []
  
-- | An Int constructor, receives a literal and constructs an integer
intConstructor :: (Id, Either Thunk Value)
intConstructor = (id, Right $ Fun mkInt "I#") where
  id = "ghc-prim:GHC.Types.I#"
  mkInt :: Id -> Env -> IM Value 
  mkInt id env = evalId id env

true :: (Id, Either Thunk Value)
true = (id, Right $ TyConApp tc []) -- has no applied values
       where
         id = "ghc-prim:GHC.Types.True"
         tc = MkDataCon id [] [] -- awaits no types, and no applied types

false :: (Id, Either Thunk Value)
false = (id, Right $ TyConApp tc []) -- has no applied values
       where
         id = "ghc-prim:GHC.Types.False"
         tc = MkDataCon id [] [] -- awaits no types, plus no applied types
         
-- | GHC.Types.Char is a type constructor that receives no further types
char :: (Id, Either Thunk Value)
char = let id = "ghc-prim:GHC.Types.Char"
       in (id, Right $ TyCon (MkDataCon id [] []) id)
       
all :: [(Id, Either Thunk Value)]
all = [ cons
        , listConstructor
        , intConstructor
        , true
        , false
        , char
      ]
