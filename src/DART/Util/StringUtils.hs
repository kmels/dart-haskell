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
                             separateWithCommas,
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
separateWithSpaces = intercalate space . filter (not . (==) empty_str) 
  where
    empty_str = ""
    
separateWithCommas :: [String] -> String
separateWithCommas = separateWith comma

comma = ","

separateWith :: String -> [String] -> String
separateWith c = intercalate c . filter (not . (==) "")

separateWithNewLines = intercalate newLine
