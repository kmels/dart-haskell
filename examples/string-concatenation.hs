module Examples.StringConcatenation where

import System.Environment

main = do
  [arg] <- getArgs
  putStrLn $ myString arg

myString w = if (length w >= 2) 
             then "hello" ++ w
             else error "word can't be of length less than 2"
             
