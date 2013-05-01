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
import Language.Core.Vdefg(vdefgNames, findVdefg)
import Text.Regex.Posix
  
-- | A function that receives an expression of the form `exp`,
-- creates a haskell file with one definition, myDefinition = exp,
-- compiles it to external core, parses it, and possibly returns a 
-- single value definition
jitCompile :: HaskellExpression -> IM (Maybe Vdefg)
jitCompile (HaskellExpression expression_string menv@(Module _ _ env_defs)) = do
  -- are we verbose?
  stgs <- gets settings
  let ?be_verbose = verbose stgs
  
  -- write and read
  io $ writeFile "dart-jit.hs" (mkContent expression_string)
  modl <- io $ readModule "dart-jit.hs"
  case modl of
    Module _ _ vdefs  -> do
       debugM $ "JIT-Compiled module, looking for the definition of " ++ def_name
       return $ findVdefg modl def_name
  where
    def_name :: String
    def_name = "jitDef"
    mkContent :: String -> String
    mkContent exp = "module DART where\n" ++ def_name ++ " = " ++ expression_string ++ content_env
    content_env :: String
    content_env = concatMap mkUndefined env_defs
    mkUndefined :: Vdefg -> String
    mkUndefined vdefg = concatMap (\name -> "\n" ++ mkName name ++ " = undefined") $ vdefgNames vdefg
    mkName :: String -> String
    -- | If we defined `fib`, ghc might have compiled this to e.g. `fibreZZ`
    -- or `twice` to `twicereX`. We try to catch these definitions
    mkName name = case (name =~ "(.*?)re" :: [[String]]) of
      [] -> name
      [m] -> last m
      _ -> error "The impossible happened"
