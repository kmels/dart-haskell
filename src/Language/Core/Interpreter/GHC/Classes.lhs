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


> evalVar ((Just (M (P ("ghczmprim"),["GHC"],"Classes"))),"zlze") = let
>   eq_2 x = Fun (\y -> lessEquals x y) "== :: a -> a -> Bool"
>   monomophy tcf = Fun (\x -> return $ eq_2 x) "Monophy"
>   in return $ Fun (\tcf -> return $ monomophy tcf) "(<=), monomophied"

> evalVar ((Just (M (P ("ghczmprim"),["GHC"],"Classes"))),"zgze") = let
>   eq_2 x = Fun (\y -> greaterEquals x y) "== :: a -> a -> Bool"
>   monomophy tcf = Fun (\x -> return $ eq_2 x) "Monophy"
>   in return $ Fun (\tcf -> return $ monomophy tcf) "(<=), monomophied"

> evalVar _ = Nothing

> equality :: Value -> Value -> IM Value
> equality v w = return . Boolean $ v == w

> lessEquals :: Value -> Value -> IM Value
> lessEquals (Num v) (Num w) = return . Boolean $ v <= w
> lessEquals v w = return . Wrong $ "lessEquals: " ++ show v ++ " and " ++ show w ++ " are not comparable"

> greaterEquals :: Value -> Value -> IM Value
> greaterEquals (Num v) (Num w) = return . Boolean $ v >= w
> greaterEquals v w = return . Wrong $ "greaterEquals: " ++ show v ++ " and " ++ show w ++ " are not comparable"
