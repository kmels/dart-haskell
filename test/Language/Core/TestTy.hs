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
  let
    -- map an id,type to id,function signature    
    funSignature :: (Id,Ty) -> (Id,Maybe [Ty])
    funSignature (id,ty) = (id, funTyArgs ty)
                               
  -- trees
  onTreesTys <- getDefTypes "examples/testing/OnTrees.hs"
  let
    trees_funsignatures_test = checkExpected (map funSignature onTreesTys) onTreesFunSignatures
    trees_typeproperties_test = checkExpectedProperties onTreesTys expectedTyPropertiesOnTrees
    trees_test = TestList [ trees_typeproperties_test, trees_funsignatures_test]
  
  -- nums
  onNumsTys <- getDefTypes "examples/interpreter/GHC.Num.hs"
  let
    nums_test_funsignatures = checkExpected (map funSignature onNumsTys) onNumsFunSignatures
    nums_test_typroperties = checkExpectedProperties onNumsTys expectedTyPropertiesOnNums
    nums_test = TestList [nums_test_typroperties, nums_test_funsignatures]

  {-putStrLn $ case lookup "plusOneIntreG" (map funSignature onNumsTys) of
    Just _ -> "found ty"
    Nothing -> "Found nothing ty"-}

  _ <- mapM (putStrLn . show) $ map funSignature onTreesTys
  
  return $ TestList [trees_test,nums_test]

-- | Expected function signatures on DART.Examples.GHC.Num
onNumsFunSignatures :: [(Id, Maybe [Ty])]
onNumsFunSignatures = []
{-  ("plusOneIntrgm", Just [intTy,intTy])
  , ("twicergn", Just [intTy,intTy])
  ,  (mdl_name @@ "sumPlusOne", Just [intTy,intTy,intTy])
  ]-}
  where
    mdl_name = "main:DART.Examples.GHC.Num"

-- | Expected function signatures on DART.Examples.GHC.Num
onTreesFunSignatures :: [(Id, Maybe [Ty])]
onTreesFunSignatures = [
  (mdl_name @@ "depthFirstSearch", Just [intTreeTy,listOfTy intTy])
  , (mdl_name @@ "sumTreeI", Just [intTreeTy,intTy])
  , (mdl_name @@ "sumOfTreeSums", Just [listOfTy $ intTreeTy,intTy])
  , (mdl_name @@ "failOnOddSumI", Just [intTreeTy, intTy])
  , (mdl_name @@ "failOnEvenSumI", Just [intTreeTy, intTy])
  , (mdl_name @@ "takeElems", Just [intTreeTy, intTy, maybeTy $ listOfTy intTy])
  , (mdl_name @@ "foldFold", Just [maybeTy $ listOfTy $ intTy,intTy])
  ]
  where
    mdl_name = "main:DART.Examples.Testing.OnTrees"
    
-- | Properties that we expect on the types of definitions in OnTrees
expectedTyPropertiesOnTrees :: [(Id,Ty -> Bool)]
expectedTyPropertiesOnTrees = [("main:DART.Examples.Testing.OnTrees.sumTreeI", not . isPrimitive)
                              , ("main:DART.Examples.Testing.OnTrees.sumTreeI", isFunctionTy)
                              , ("main:DART.Examples.Testing.OnTrees.failOnOddSumI", isFunctionTy)
                              ]

-- | Properties that we expect on the types of definitions in OnTrees
expectedTyPropertiesOnNums :: [(Id,Ty -> Bool)]
expectedTyPropertiesOnNums = [("main:DART.Examples.GHC.Num.numberTen", isPrimitive)
                             , ("main:DART.Examples.GHC.Num.fib0", isPrimitive)
                             , ("main:DART.Examples.GHC.Num.isTenEven", not . isFunctionTy)
--                             , ("plusOneIntreG", isFunctionTy)
                             , ("main:DART.Examples.GHC.Num.sumPlusOne", isFunctionTy)
                             ]

-- | Given a file, extract all the identifiers with its types
getDefTypes :: FilePath -> IO [(Id,Ty)]
getDefTypes filepath = do
  let ?be_verbose = False
  modl@(Module _ _ vdefs) <- readModule filepath  
  return $ concatMap extractTypes vdefs
  where
    extractTypes :: Vdefg -> [(Id,Ty)] 
    extractTypes (Nonrec vdef@(Vdef(qvar, ty, _))) = [(zDecodeQualified qvar, ty)]
    extractTypes (Rec []) = []
    extractTypes (Rec (vdef@(Vdef(qvar, ty, _)):vs)) = [(zDecodeQualified qvar, ty)] ++ extractTypes (Rec vs)
    
intTy :: Ty
intTy = mkTcon "ghczmprim" ["GHC"] "Types" "Int"

listOfTy :: Ty -> Ty
listOfTy t = Tapp (mkTcon "ghczmprim" ["GHC"] "Types" "ZMZN") t

intTreeTy = mkTcon "main" ["DART","Examples","Testing"] "OnTrees" "IntTree"

mkTcon :: String -> [String] -> String -> String -> Ty
mkTcon pkg_name pkg_ids modl_id tcon_id = Tcon (Just (M (P (pkg_name),pkg_ids,modl_id)), tcon_id)

maybeTy :: Ty -> Ty
maybeTy t = Tapp (mkTcon "base" ["Data"] "Maybe" "Maybe") t
