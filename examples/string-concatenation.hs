module Examples.StringConcatenation(
  myString --, myString'
) where

import System.Environment

main = do
  [arg] <- getArgs
  putStrLn $ myString arg
--  putStrLn $ myString' arg arg

myString w = if (length w >= 2) 
             then "hello" ++ w
             else error "word can't be of length less than 2"
             

--myString' w w2 = if (length w >= 2 && length w2 >= 2)
--             then "hello" ++ w ++ w2
--             else error "word can't be of length less than 2"