> module Language.Core.Interpreter.GHC.Num where

> import Language.Core.Interpreter.Structures
> import Language.Core.Core
> import Language.Core.Interpreter.Apply

Given a polymorphic function and a tcf (generated at compile time from a type class), returns a monomorphic function.

>{- monomophy :: Value -> Value -> Value
> monomophy pf@(Fun f desc) tcf = Fun (\x -> return $ pf x) $ " Monomophy with " ++ show tcf
> monomophy v _ = Wrong $ "Trying to instanciate a monomorphic function from value " ++ show v -}

> --monomophy f tcf = Fun (\x -> return $ f x) "Monophy"

> evalVar :: Qual Var -> Maybe Value

The function that adds two numbers in GHC (base:GHC.Num.+).

> evalVar ((Just (M (P ("base"),["GHC"],"Num"))),"zp") = let
>   add_2 x = Fun (\y -> addValues x y) "+ :: a -> a -> a"
>   monomophy tcf = Fun (\x -> return $ add_2 x) "Monophy"
>   in return $ Fun (\tcf -> return $ monomophy tcf) "(+), monophied"

-- $ Fun (\tcf -> return $ monotype tcf) $ "Monomophy with " ++ show tcf -- tcf can be $fNumInt for instance.

The function that multiplies two numbers in GHC (base:GHC.Num.*).

> evalVar ((Just (M (P ("base"),["GHC"],"Num"))),"zt") = let
>   mul_2 x = Fun (\y -> multiplyValues x y) "(*) :: a -> a -> a"
>   monomophy tcf = Fun (\x -> return $ mul_2 x) "Monophy"
>  in return $ Fun (\tcf -> return $ monomophy tcf) "(*), monophied"

The apply function on Ints. It takes a function `f :: Int -> Int` and an `x :: Int` and evals to `f x :: Int`.

> evalVar ((Just (M (P ("base"),["GHC"],"Num"))),"zdfNumInt") = return $ Fun (\f -> return f) "GHC.Base.$fNumInt"
> evalVar ((Just (M (P ("base"),["GHC"],"Num"))),"zdfNumInteger") = return $ Fun (\f -> return f) "GHC.Base.$fNumInteger"

--return $ Fun (\f -> return (ap f)) "GHC.Base.$fNumInt :: Fun (Int -> Int) -> Fun(Int -> Int)"

We, GHC.Num, don't contain that variable.

> evalVar _ = Nothing

> addValues :: Value -> Value -> IM Value
> addValues (Num i) (Num j) = return . Num $ i + j 
> addValues a b = return . Wrong $ "Trying to add values " ++ show a ++ " and " ++ show b

> multiplyValues :: Value -> Value -> IM Value
> multiplyValues (Num i) (Num j) = return . Num $ i * j 
> multiplyValues a b = return . Wrong $ "Trying to multiply values " ++ show a ++ " and " ++ show b

