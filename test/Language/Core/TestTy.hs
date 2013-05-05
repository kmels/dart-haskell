{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE FlexibleInstances #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Language.Core.TestTy
-- Copyright   :  (c) Carlos López-Camey, University of Freiburg
-- License     :  BSD-3
--
-- Maintainer  :  c.lopez@kmels.net
-- Stability   :  stable
--
--
-- Tests functions in Language.Core.Ty
-----------------------------------------------------------------------------

module Language.Core.TestTy where

import DART.TestUtils
import Language.Core.Core
import Language.Core.Ty
test :: Test
test = unsafePerformIO $ testIO 
  
testIO :: IO Test
testIO = do
  -- trees
  onTreesTys <- getDefTypes "examples/testing/OnTrees.hs"
  let trees_test = checkExpectedProperties onTreesTys expectedTyPropertiesOnTrees
  
  -- nums
  onNumsTys <- getDefTypes "examples/interpreter/GHC.Num.hs"
  let nums_test = checkExpectedProperties onNumsTys expectedTyPropertiesOnNums
  return $ TestList [trees_test] --, onNumsTest]

-- | Properties that we expect on the types of definitions in OnTrees
expectedTyPropertiesOnTrees :: [(Id,Ty -> Bool)]
expectedTyPropertiesOnTrees = [("main:DART.Examples.Testing.OnTrees.sumTreeI", not . isPrimitive)
                              , ("main:DART.Examples.Testing.OnTrees.sumTreeI", isLambdaArrow)
                              , ("main:DART.Examples.Testing.OnTrees.failOnOddSumI", isLambdaArrow)
                              ]

-- | Properties that we expect on the types of definitions in OnTrees
expectedTyPropertiesOnNums :: [(Id,Ty -> Bool)]
expectedTyPropertiesOnNums = [("main:DART.Examples.GHC.Num.numberTen", isPrimitive)
                             , ("main:DART.Examples.GHC.Num.fib0", isPrimitive)
                             , ("main:DART.Examples.GHC.Num.isTenEven", isLambdaArrow)
                             , ("main:DART.Examples.GHC.Num.plusOneIntreX", isLambdaArrow)
                             , ("main:DART.Examples.GHC.Num.sumPlusOne", isLambdaArrow)
                             ]

-- | Given a file, extract all the identifiers with its types
getDefTypes :: FilePath -> IO [(Id,Ty)]
getDefTypes filepath = do
  let ?be_verbose = False
  modl@(Module _ _ vdefs) <- readModule filepath  
  return $ concatMap extractTypes vdefs
  where
    extractTypes :: Vdefg -> [(Id,Ty)] 
    extractTypes (Nonrec vdef@(Vdef(qvar, ty, _))) = [(qualifiedVar qvar, ty)]
    extractTypes (Rec []) = []
    extractTypes (Rec (vdef@(Vdef(qvar, ty, _)):vs)) = [(qualifiedVar qvar, ty)] ++ extractTypes (Rec vs)
    
