> module Language.Core.Interpreter.Apply where

> import Language.Core.Interpreter.Structures

> apply :: Value -> Value -> IM Value
> apply fun@(Fun f _) v = f v
> apply w@(Wrong _) _ = return w
> apply f m = return . Wrong $ "Applying something that is not a function, namely " ++ show f
