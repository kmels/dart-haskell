> module Language.Core.Interpreter.GHC.Classes where

> import Language.Core.Interpreter.Structures
> import Language.Core.Core
> import Language.Core.Interpreter.Apply

Evaluates variables, equivalent to library functions in GHC.Classes, to Values.

> evalVar :: Qual Var -> Maybe Value

Value equality

> evalVar ((Just (M (P ("ghczmprim"),["GHC"],"Classes"))),"zeze") = let
>   eq_2 x = Fun (\y -> equality x y) "== :: a -> a -> Bool"
>   monomophy tcf = Fun (\x -> return $ eq_2 x) "Monophy"
>   in return $ Fun (\tcf -> return $ monomophy tcf) "(==), monomophied"

> evalVar _ = Nothing

> equality :: Value -> Value -> IM Value
> equality v w = return . Boolean $ v == w
