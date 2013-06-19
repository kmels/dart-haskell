----------------------------------------------------------------------------
-- |
-- Module      :  DART.Util.StringUtils
-- Copyright   :  (c) Carlos López-Camey, University of Freiburg
-- License     :  BSD-3
--
-- Maintainer  :  c.lopez@kmels.net
-- Stability   :  stable
--
--
-- This module contains combinators for strings, used to pretty print
-----------------------------------------------------------------------------

module DART.Util.StringUtils(separateWithSpaces,
                            separateWithNewLines) where

--------------------------------------------------------------------------------
-- prelude
import Data.List(intercalate)

space,newLine :: String
space = " "
newLine = "\n"

intercalateWith :: [String] -> String -> String
intercalateWith = flip intercalate

separateWithSpaces :: [String] -> String
separateWithSpaces = intercalate space

separateWithNewLines = intercalate newLine
