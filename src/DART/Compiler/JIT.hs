{-# LANGUAGE ImplicitParams #-} 
----------------------------------------------------------------------------
-- |
-- Module      :  DART.Compiler.JIT
-- Copyright   :  (c) Carlos López-Camey, University of Freiburg
-- License     :  BSD-3
--
-- Maintainer  :  c.lopez@kmels.net
-- Stability   :  stable
--
--
-- A Just in time compiler
-----------------------------------------------------------------------------

module DART.Compiler.JIT where

import DART.CmdLine
import DART.FileIO
import DART.InterpreterSettings
import Language.Core.Core
import Language.Core.Interpreter.Structures
-- | A function that receives an expression of the form `exp`,
-- creates a haskell file with one definition, myDefinition = exp,
-- compiles it to external core, parses it, and possibly returns a 
-- single value definition
jitCompile :: HaskellExpression -> IM (Maybe Vdefg)
jitCompile (HaskellExpression expression_string _) = do
  -- are we verbose?
  stgs <- gets settings
  let ?be_verbose = verbose stgs
  
  -- write and read
  io $ writeFile "dart-jit.hs" (mkContent expression_string)
  modl <- io $ readModule "dart-jit.hs"
  case modl of
    Module _ _ [vdefg] -> return . Just $ vdefg
    Module _ _ vdefs  -> debugM "Something wrong happened"
                         >> debugM ("Here are the value definitions" ++ show vdefs)
                         >> return Nothing
  where
    mkContent :: String -> String
    mkContent exp = "module DART where\njitDef = " ++ expression_string
