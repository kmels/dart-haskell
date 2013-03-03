{-# LANGUAGE DeriveDataTypeable #-}
module DART.InterpreterSettings where

import System.Console.CmdArgs

data InterpreterSettings = InterpreterMode { 
  file :: String
  , eval :: String
  , debug :: Bool
  , show_heap :: Bool
--, show_time :: Bool
  , show_expressions :: Bool
  , show_subexpressions :: Bool
  , show_tmp_variables :: Bool 
  , watch_reduction :: Bool
  , debug_tab_level :: Int
  } deriving (Show, Data, Typeable)

-- | We'll use the package cmdargs to identify flags, parameters, etc.,  from the command line
-- The interpreter mode reads an external core file and evaluates the declarations of its module.
             
interpret = InterpreterMode {
  file = def &= typFile &= groupname "USAGE"
  , eval = def &= typ "FUNCTION_NAME" &= groupname "USAGE" &= help "The function to evaluate (if not provided, all function declarations will be evaluated)"
  , debug = def &= groupname "DEBUG" &= help "Be verbose about what this program is doing"
  , show_heap = def &= groupname "DEBUG" &= help "Shows binded values in the heap"
  , show_expressions = def &= groupname "DEBUG" &= help "Shows the external core expression for every value being evaluated"
  , show_subexpressions = def &= groupname "DEBUG" &= help "Shows *every* (external core) expression being evaluated"
--   , show_time = def &= groupname "DEBUG" &= help "Shows the time in which an evaluation was done (if depends flag is on)"
  , show_tmp_variables = def &= groupname "DEBUG" &= help "Shows debug messages for temporal variables (if depends flag is on)"
  , watch_reduction = def &= groupname "DEBUG" &= help "Shows debug messages for the evaluation of a value definition (shows reductions of expressions)"
  , debug_tab_level = 0
  } &= summary "Reads a .hcr file and evaluates its declarations. "


increase_tab_level :: InterpreterSettings -> InterpreterSettings
increase_tab_level s = 
  s { debug_tab_level = c + 1 } where c = debug_tab_level s
  

decrease_tab_level :: InterpreterSettings -> InterpreterSettings
decrease_tab_level s = 
  s { debug_tab_level = c - 1 } where c = debug_tab_level s

