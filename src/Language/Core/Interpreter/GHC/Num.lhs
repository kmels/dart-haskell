> module Language.Core.Interpreter.GHC.Num where

> import Language.Core.Interpreter.Structures
> import Language.Core.Core
> import Language.Core.Interpreter.Apply

> evalVar :: Qual Var -> Maybe Value

The function that adds two numbers in GHC (base:GHC.Num.+).

> evalVar ((Just (M (P ("base"),["GHC"],"Num"))),"zp") = let
>   add n = Fun (\m -> addValues n m) "GHC.Num.+ :: Num -> Fun (Num -> Num)"
>  in return $ Fun (\n -> return (add n)) "+ " -- :: Fun (Num -> Fun (Num -> Num))"

The function that multiplies two numbers in GHC (base:GHC.Num.*).

> evalVar ((Just (M (P ("base"),["GHC"],"Num"))),"zt") = let
>   mul n = Fun (\m -> multiplyValues n m) "GHC.Num.* :: Num -> Fun (Num -> Num)"
>  in return $ Fun (\n -> return (mul n)) "*" -- :: Fun (Num -> Fun (Num -> Num))"

The apply function on Ints. It takes a function `f :: Int -> Int` and an `x :: Int` and evals to `f x :: Int`.

> evalVar ((Just (M (P ("base"),["GHC"],"Num"))),"zdfNumInt") = let
>   ap f = Fun (\x -> apply f x) "GHC.Base.$fNumInt :: Fun(Int -> Int)"
>  in return $ Fun (\f -> return (ap f)) "GHC.Base.$fNumInt :: Fun (Int -> Int) -> Fun(Int -> Int)"

We, GHC.Num, don't contain that variable.

> evalVar _ = Nothing

> addValues :: Value -> Value -> IM Value
> addValues (Num i) (Num j) = return . Num $ i + j 
> addValues a b = return . Wrong $ "Trying to add values " ++ show a ++ " and " ++ show b

> multiplyValues :: Value -> Value -> IM Value
> multiplyValues (Num i) (Num j) = return . Num $ i * j 
> multiplyValues a b = return . Wrong $ "Trying to multiply values " ++ show a ++ " and " ++ show b

