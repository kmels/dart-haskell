-- Parametric polymorphism on lists

module ParametricPolymorphism where

import System.Environment

concatenateIfGreaterThan :: [a] -> [a] -> Int -> [a]
concatenateIfGreaterThan xs ys n = if ((length xs > n) && (length ys > n))
                                   then xs ++ ys
                                   else xs
                                   
main = getArgs >>= \args -> putStrLn $ case args of
  [a,b] -> concatenateIfGreaterThan a b 2
  _ -> error "Two string parameters only please."
  
