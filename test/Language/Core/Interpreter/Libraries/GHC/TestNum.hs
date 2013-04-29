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

import Test.HUnit hiding (test)
import DART.TestUtils

type Result = (Id,Value)
type ExpectedResult = (Id,Value)

expected_nums :: [ExpectedResult]
expected_nums = [ ("main:Examples.GHC.Num.numberTen", Num 10)
                  , ("main:Examples.GHC.Num.numberEleven", Num 11)
                  , ("main:Examples.GHC.Num.numberTwentyTwo", Num 22)
                  ]

checkExpected :: [Result] -> ExpectedResult -> Test
checkExpected results expected = lookup (fst expected) results ~=? (Just $ snd expected)

-- | This function loads the file located in examples/interpreter/GHC.Num.hs and verifies the expected results
testIO :: IO Test 
testIO = do
--  initial_state <- initDART interpreter 
  results <- evalFile "examples/interpreter/GHC.Num.hs"
  
  let expected = expected_nums
  let prettyPrint :: (Id,Value) -> String
      prettyPrint (id,val) = show id ++ " => " ++ show val
          
  mapM (putStrLn . prettyPrint) results
  return $ TestList $ map (checkExpected results) expected

evalFile :: FilePath -> IO [(Id,Value)]
evalFile filepath = initDART interpret >>= evalStateT evalFile'
  where
  evalFile' :: IM [Result]
  evalFile' = do
    let ?be_verbose = False
    modul <- io . readModule $ filepath
    module_env <- acknowledgeModule modul
    libs_env <- mkLibsEnv
    evalModule modul (module_env ++ libs_env)
    
test = unsafePerformIO testIO
