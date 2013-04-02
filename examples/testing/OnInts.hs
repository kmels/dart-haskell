module OnInt(
  ackermann, ackermannE, ackermannU
  , f1, positive, positive', negative
--  , division
)where

-- | The two-argument Ackermann-Péter function. This function is total computable but not a primitive recursive function. It is undefined for m
-- fails with "Non-exhaustive patterns in function ackermann"
ackermann :: Int -> Int -> Int
ackermann m n  | m == 0 = n + 1
                | m > 0 && n == 0 = ackermann (m-1) 1
                | m > 0 && n > 0 = ackermann (m-1) (ackermann m (n-1))
                
-- | The two-argument Ackermann-Péter function. This function is total computable but not a primitive recursive function. It is undefined for m
-- fails with `error`
ackermannE :: Int -> Int -> Int
ackermannE m n | m == 0 = n + 1
               | m > 0 && n == 0 = ackermann (m-1) 1
               | m > 0 && n > 0 = ackermann (m-1) (ackermann m (n-1))
               | otherwise = error "undefined"
                             
-- | The two-argument Ackermann-Péter function. This function is total computable but not a primitive recursive function. It is undefined for m
-- fails with undefined
ackermannU :: Int -> Int -> Int
ackermannU m n | m == 0 = n + 1
                | m > 0 && n == 0 = ackermann (m-1) 1
                | m > 0 && n > 0 = ackermann (m-1) (ackermann m (n-1))
                | otherwise = undefined

-- | A partial function on ints
f1  :: Int -> Int
f1 0 = error "Invalid: 0"
f1 25 = error "Invalid: 25"
f1 1401 = error "Invalid: 1401"
f1 2905 = error "Invalid: 2905"
f1 96291 = error "Invalid: 96291"
f1 n = n

-- | Retuns error if the argument is even
odd :: Int -> Int
odd n | n `mod` 2 == 0 = error "Argument is an even number"
      | otherwise = n
      
even :: Int -> Int
even n | n `mod` 2 == 0 = n
       | otherwise = error "Argument is an odd number"

-- | Returns error if the argument is negative or zero
positive :: Int -> Int
positive n | n > 0 = n
           | otherwise = error $ "Argument is negative or zero" ++ show n 

-- | Returns error if the argument is negative
positive' :: Int -> Int
positive' n | n >= 0 = n
           | otherwise = error $ "Argument is negative" ++ show n 

-- | Returns error if the argument is positive or zero
negative :: Int -> Int
negative n | n < 0 = n
           | otherwise = error $ "Argument is positive or zero" ++ show n 

