----------------------------------------------------------------------------
-- |
-- Module      :  Language.Core.Module
-- Copyright   :  (c) Carlos López-Camey, University of Freiburg
-- License     :  BSD-3
--
-- Maintainer  :  c.lopez@kmels.net
-- Stability   :  stable
--
--
-- This module contains functions that work upon Language.Core.Core.Module
----------------------------------------------------------------------------- 

module Language.Core.Module where

import Data.List(find)
import Language.Core.Core
import Language.Core.Vdefg(findVdefg,findVdefByName)

moduleFindVdefByName :: Module -> String -> Maybe Vdef
moduleFindVdefByName m def_name = m `findVdefg` def_name >>= findVdefByName def_name

