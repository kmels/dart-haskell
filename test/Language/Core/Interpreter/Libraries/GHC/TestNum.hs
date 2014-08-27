{-# LANGUAGE ImplicitParams #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Language.Core.Interpreter.Libraries.GHC.TestNum
-- Copyright   :  (c) Carlos López-Camey, University of Freiburg
-- License     :  BSD-3
--
-- Maintainer  :  c.lopez@kmels.net
-- Stability   :  stable
--
--
-- Tests functions implemented in Language.Core.Interpreter.Libraries.GHC.Num
-----------------------------------------------------------------------------

module Language.Core.Interpreter.Libraries.GHC.TestNum(test) where

import DART.TestUtils
--import Test.HUnit hiding (test)

type Result = (Id,Value)
type ExpectedResult = (Id,Value)

expected_nums :: [ExpectedResult]
expected_nums = [ ("main:DART.Examples.GHC.Num.numberTen", Num 10)
                  , ("main:DART.Examples.GHC.Num.numberEleven", Num 11)
                  , ("main:DART.Examples.GHC.Num.numberTwentyTwo", Num 22)
                  ]

--checkExpected :: [Result] -> ExpectedResult -> Test
--checkExpected results expected = lookup (fst expected) results ~=? (Just $ snd expected)

-- | This function loads the file located in examples/interpreter/GHC.Num.hs and verifies the expected results
testIO :: IO Test 
testIO = do
  results <- evalFile "examples/interpreter/GHC.Num.hs"
  
  let prettyPrint :: (Id,Value) -> String
      prettyPrint (id',val) = show id' ++ " => " ++ show val
          
  _ <- mapM (putStrLn . prettyPrint) results
  return $ checkExpected results expected_nums

evalFile :: FilePath -> IO [(Id,Value)]
evalFile filepath = initDART testSettings >>= evalStateT evalFile'
  where
  testSettings :: DARTSettings
  testSettings = interpret { max_time_per_function = 25} -- seconds
  
  evalFile' :: IM [(Id,Value)]
  evalFile' = do
    loadLibraries
    
    let ?be_verbose = False        
    modul <- io . readModule $ filepath
    module_env <- acknowledgeModule modul    
    evalModule modul module_env

test :: Test
test = unsafePerformIO testIO
