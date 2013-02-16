> module Language.Core.Interpreter.GHC.Num where

> import Language.Core.Interpreter.Structures
> import Language.Core.Core
> import Language.Core.Interpreter.Apply

Given a polymorphic function and a tcf (generated at compile time from a type class), returns a monomorphic function.

> --monotype :: Value -> Value -> Value
> --monotype polyF@(Fun f desc) tcf = Fun (\x -> apply (polyF ) tcf) $ desc  ++ " being monomorphied with " ++ show tcf
> --monotype v _ = Wrong $ "Trying to instanciate a monomorphic function from value " ++ show v

> evalVar :: Qual Var -> Maybe Value

The function that adds two numbers in GHC (base:GHC.Num.+).

> evalVar ((Just (M (P ("base"),["GHC"],"Num"))),"zp") = let
>   add_2 x = Fun (\y -> addValues x y) "+ :: a -> a -> a"
>   add_1 = Fun (\x -> return $ add_2 x) "+ :: a -> (a -> a)"
>   monotype tcf = Fun (\x -> return $ add_2 x) " saber"
>  in return $ Fun (\tcf -> return $ monotype tcf) "+ :: (Num a) -> a -> a -> a" -- tcf can be $fNumInt for instance.

The function that multiplies two numbers in GHC (base:GHC.Num.*).

> evalVar ((Just (M (P ("base"),["GHC"],"Num"))),"zt") = let
>   add x = Fun (\y -> addValues x y) "+ :: a -> a -> a"
>   

>   mul n = Fun (\m -> multiplyValues n m) "GHC.Num.* :: Num -> Fun (Num -> Num)"
>  in return $ Fun (\n -> return (mul n)) "*" -- :: Fun (Num -> Fun (Num -> Num))"

The apply function on Ints. It takes a function `f :: Int -> Int` and an `x :: Int` and evals to `f x :: Int`.

> evalVar ((Just (M (P ("base"),["GHC"],"Num"))),"zdfNumInt") = return $ Fun (\f -> return f) "GHC.Base.$fNumInt"

--return $ Fun (\f -> return (ap f)) "GHC.Base.$fNumInt :: Fun (Int -> Int) -> Fun(Int -> Int)"

We, GHC.Num, don't contain that variable.

> evalVar _ = Nothing

> addValues :: Value -> Value -> IM Value
> addValues (Num i) (Num j) = return . Num $ i + j 
> addValues a b = return . Wrong $ "Trying to add values " ++ show a ++ " and " ++ show b

> multiplyValues :: Value -> Value -> IM Value
> multiplyValues (Num i) (Num j) = return . Num $ i * j 
> multiplyValues a b = return . Wrong $ "Trying to multiply values " ++ show a ++ " and " ++ show b

