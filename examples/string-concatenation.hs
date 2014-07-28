module Examples.StringConcatenation(
  myString, 
  myStringConcat,
  myStringLength --, myString'
) where

import System.Environment 
import Prelude hiding (concat)

main = do
  [arg] <- getArgs
  putStrLn $ myString arg
  putStrLn $ myStringConcat arg
--  putStrLn $ myString' arg arg

myString w = let length_of_it = length w
             in if (length_of_it > 20) 
             then hi ++ w
             else error $ "word can't be of length less than 2, it is " ++ (show length_of_it) ++ " characters long!"
             
myStringLength w = let l = length w in  w ++ " has " ++ (show l) ++ " characters."


--myStringConcat v = v ++ " plus "

hi = 'h' : 'i' : []

--myString' w w2 = if (length w >= 2 && length w2 >= 2)
--             then "hello" ++ w ++ w2
--             else error "word can't be of length less than 2"

concat :: [a] -> [a] -> [a]
[] `concat` ys = ys
(x:xs) `concat` ys = x:(xs `concat` ys)

myStringConcat :: String -> String
myStringConcat s | length s < 7 = error "Too small" 
                 | otherwise = myError s -- s `concat` "b"


myError s = error s
