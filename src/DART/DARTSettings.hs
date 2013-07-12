{-# LANGUAGE DeriveDataTypeable #-}
module DART.DARTSettings where

import System.Console.CmdArgs

data DARTSettings = InterpreterMode { 
  file :: FilePath -- The file we will read information from, it can be either .hs, .lhs or .hcr
  , evaluate_function :: String -- the interpreter evaluates some function, defined in `file`
  , test_function :: String -- some function to test, defined in `file`
  , debug :: Bool -- debug flags
  , verbose :: Bool -- prints messages when loading, reading and acknowledging modules
  , timeout_seconds :: Integer
  , show_heap :: Bool
--, show_time :: Bool
  , show_expressions :: Bool
  , show_subexpressions :: Bool
  , show_tmp_variables :: Bool 
<<<<<<< Updated upstream
  , show_included_definitions :: Bool -- show every definition being loaded from the libraries
=======
  , show_include_definitions :: Bool -- show every definition being loaded from the libraries?
>>>>>>> Stashed changes
  , watch_reduction :: Bool
  , watch_test :: Bool 
  , watch_smt :: Bool
  , debug_tab_level :: Int
  , include :: [FilePath]
  , benchmark :: Bool

  , number_of_tests :: Int  
  -- primitive types
  , max_int_bound :: Int
  , min_int_bound :: Int
  } deriving (Show, Data, Typeable)

-- | We'll use the package cmdargs to identify flags, parameters, etc.,  from the command line
-- The interpreter mode reads an external core file and evaluates the declarations of its module.
             
interpret = InterpreterMode {
  benchmark = def &= groupname "USAGE" &= help "Benchmark"
  , file = def &= typFile &= groupname "USAGE"
  
  , evaluate_function = def &= groupname "USAGE" &= help "The function to evaluate (if not provided, all function declarations will be evaluated)"
  , test_function = def &= groupname "USAGE" &= help "The function to test (if not provided, all functions will be tested)"
  , debug = def &= groupname "DEBUG" &= help "Prints messages about the interpretation and testing of module definitions"
  , verbose = def &= groupname "DEBUG" &= help "Prints messages when loading, reading and acknowledging modules or definitions"
  
  , timeout_seconds = 2
  
  , show_heap = def &= groupname "DEBUG" &= help "Shows binded values in the heap"
  , show_expressions = def &= groupname "DEBUG" &= help "Shows the external core expression for every value being evaluated"
  , show_subexpressions = def &= groupname "DEBUG" &= help "Shows *every* (external core) expression being evaluated"
<<<<<<< Updated upstream
  , show_included_definitions = def &= groupname "DEBUG" &= help "Shows every definition loaded from the libraries (Prelude included)"
=======
  , show_include_definitions = def &= groupname "DEBUG" &= help "Shows every definition loaded from the libraries (Prelude included)"
>>>>>>> Stashed changes
--   , show_time = def &= groupname "DEBUG" &= help "Shows the time in which an evaluation was done (if depends flag is on)"
  , show_tmp_variables = def &= groupname "DEBUG" &= help "Shows debug messages for temporal variables (if depends flag is on)"
  , watch_reduction = def &= groupname "DEBUG" &= help "Shows debug messages for the evaluation of a value definition (shows reductions of expressions)"
  , watch_test = def &= groupname "DEBUG" &= help "Shows debug messages when testing a value definition"
  , watch_smt = def &= groupname "DEBUG" &= help "Shows debug messages of the usage of the SMT solver"
  , debug_tab_level = 0
  , include = def &= groupname "USAGE" &= help "List of source directories to include in the module namespace, the base package is included by default"  
  -- tests config
  , number_of_tests = def &= groupname "TESTING" &= help "Number of tests to do per test case i.e. per function definition"  
  -- primitives
  , min_int_bound = def &= help "Minimum random integer to be generated"
  , max_int_bound = def &= help "Maximum random integer to be generated"
  } &= summary "Reads a .hcr file and evaluates its declarations. "


increase_debug_tab_level :: DARTSettings -> DARTSettings
increase_debug_tab_level s = s { debug_tab_level = debug_tab_level s + 1 } 
decrease_debug_tab_level :: DARTSettings -> DARTSettings
decrease_debug_tab_level s = s { debug_tab_level = debug_tab_level s - 1 } 

