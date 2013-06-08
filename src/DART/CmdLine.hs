{-# LANGUAGE ImplicitParams #-}

module DART.CmdLine where

import           Control.Monad(when)
import           Control.Monad.IO.Class
import           Control.Monad.State(gets)
import qualified Data.HashTable.IO as H
import           Language.Core.Core -- Exp
import           Language.Core.Util(showExp)
import           Control.Monad.State.Class(modify)

--------------------------------------------------------------------------------
-- Common data types
import           Data.Either.Utils(forceEither)
import           Data.Foldable
import           Data.Monoid
import           Prelude hiding (foldr)
--------------------------------------------------------------------------------
-- System
import           System.Directory

--------------------------------------------------------------------------------
-- Control
import           Control.Applicative((<|>))
--------------------------------------------------------------------------------
-- DART
import           DART.DARTSettings
import           Language.Core.Interpreter.Structures

--------------------------------------------------------------------------------
-- config file 
import qualified Data.ConfigFile as Conf

-- | Prints a debug message with the number of the current reduction prepended
debugMStep :: String -> IM ()
debugMStep msg = do
  modify increase_number_of_reductions -- and then, increase the number
  prependStep -- print * 0, * 1, * 2, etc.
  debugMOTL msg -- print the message with a prepended tab, should have { at the end
  resetStepPart
  where
    -- | resets the subheading number to none or 0
    resetStepPart :: IM ()
    resetStepPart = modify reset_reduction_part
        
    reset_reduction_part :: DARTState -> DARTState
    reset_reduction_part s = s { number_of_reductions_part = 0 }

-- | Prints a debug message with the number of the current reduction prepended
-- debugMStepPart :: String -> IM ()
-- debugMStepPart msg = do
--   prependStepPart -- and then, increase the number
--   debugMOTL (msg ++ " {") 
    
-- | Prints a debug message that indicates the end of the current reduction
-- it increases then the step
debugMStepEnd :: IM ()
debugMStepEnd = do
  debugM "}" -- append the number, print it
                
-- | Prints the reduction number
prependStep :: IM () 
prependStep = do
  n <- gets number_of_reductions -- get the number
  ti <- gets tab_indentation 
  let
    stars = replicate ti '*' ++ " "    
  debugMNLNT $ stars ++ show n ++ "."

-- | Prints the reduction number with its part number 
-- In the spirit of subheadings, this prints a message such as * 1.1 or ** 2.4
prependStars :: IM () 
prependStars = do
--  n <- gets number_of_reductions -- get the heading number 
--  m <- increaseStepPart >> gets number_of_reductions_part -- get the subheading number 
  ti <- gets tab_indentation
  let
    stars = replicate (ti) '*' ++ " "
  debugMNLNT $ stars 
  where
    increaseStepPart :: IM ()
    increaseStepPart = modify increase_reduction_part 
    
    -- | When debugging, increases the subheading number by one
    increase_reduction_part :: DARTState -> DARTState
    increase_reduction_part s = s { number_of_reductions_part = 
                                   number_of_reductions_part s + 1}
                       
-- | Prepends a new line
prependNewLn :: String -> String
prependNewLn = flip (++) "\n"

-- | Prints a debug message with a new line at the end
debugM :: String -> IM ()
debugM msg = do 
  s <- gets settings
  ti <- gets tab_indentation  
  when (debug s) $
    let tab = replicate ti '\t' 
    in io . putStrLn $ tab ++ msg

-- | Prints a debug message with a new line at the end 
-- and without a prepended tab
debugMNT :: String -> IM ()
debugMNT msg = do 
  s <- gets settings
  when (debug s) $
    io . putStrLn $ msg

-- | Prints a debug message with a new line at the end 
-- and with one less prepended tab
debugMOTL :: String -> IM ()
debugMOTL msg = do 
  s <- gets settings
  ti <- gets tab_indentation  
  when (debug s) $
    let tab = replicate (ti-1) '\t' 
    in io . putStrLn $ (tab ++ msg) 

    
-- | Prints a debug message without a new line at the end
-- and without a prepended tab
debugMNLNT :: String -> IM ()
debugMNLNT msg = do 
  s <- gets settings
  when (debug s) $ io . putStr $ msg      
          
-- | Prints a debug message without a new line at the end
-- with a prepended tab
debugMNL :: String -> IM ()
debugMNL msg = do 
  s <- gets settings
  ti <- gets tab_indentation  
  when (debug s) $
    let tab = replicate ti '*' 
    in io . putStr $ (tab ++ msg) 
            
printHeap :: Heap -> IO ()
printHeap heap = do
  putStrLn $ "-------------------- Heap begins --------------------" 
  H.mapM_ printVar heap
  putStrLn $ "-------------------- Heap ends --------------------" 
  where
    showVal :: Either Thunk Value -> String
    showVal (Left t) = show t
    showVal (Right v) = show v
    
    printVar (id,val) = putStrLn $ show id ++ " => " ++ showVal val

-- | If we are in the IM Monad, we might want to print expressions being reduced as they are interpreted. 
-- If the flag --show-subexpressions was specified, shows the given expression
debugSubexpression :: Exp -> IM ()
debugSubexpression e = do
  whenFlag show_subexpressions $ do
    ti <- gets tab_indentation
    let ?tab_indentation = ti
    --prependStars
    debugM $ "Subexpression: " ++ showExp e

-- | If the flag --verbose is on, prints messages about Loading, Reading and Acknowledging of modules
beVerboseM :: String -> IM ()
beVerboseM msg = whenFlag verbose $ debugM msg

-- | If we are in the IM Monad, we might want to watch expressions being reduced as they are interpreted. 
-- If the flag --watch-reduction was specified, prints a debug message.
watchReductionM :: String -> IM ()
watchReductionM msg = whenFlag watch_reduction $ prependStars >> debugM msg

-- | Prints a debug message when the flag --watch-test was provided
watchTestM :: String -> IM ()
watchTestM msg = whenFlag watch_test $ debugMNT msg

whenFlag :: (DARTSettings -> Bool) -> IM () -> IM ()
whenFlag f a = do
  sttgs <- gets settings
  when (f sttgs) $ a
  
indentExp :: Exp -> IM String
indentExp e = gets tab_indentation >>= \ti -> let ?tab_indentation = ti in return $ showExp e

increaseIndentation :: IM ()
increaseIndentation = get >>= put . increase_indentation
decreaseIndentation :: IM ()
decreaseIndentation = get >>= put . decrease_indentation

increase_indentation :: DARTState -> DARTState
increase_indentation s = s { tab_indentation  = tab_indentation s + 1 }
decrease_indentation :: DARTState -> DARTState
decrease_indentation s = s { tab_indentation  = tab_indentation s - 1 }


instance Foldable (Either e) where
  foldMap f (Left  _) = mempty
  foldMap f (Right r) = f r

  foldr _ z (Left  _) = z
  foldr f z (Right r) = f r z       
  
-- | Merges settings between the command line and the configuration file.
-- the values from command line are more prioritary
mergeConfigSettings :: DARTSettings -> Conf.ConfigParser -> IO DARTSettings
mergeConfigSettings st cp = do
  return $ st {
     file = mergeStr "interpreter" "file" (file st)
     -- primitives
     , min_int_bound = mergeInt "primitive types" "min_int_bound" (min_int_bound st)
     , max_int_bound = mergeInt "primitive types" "max_int_bound" (max_int_bound st)
  }
  where
    -- merge a string setting
    mergeStr :: Conf.SectionSpec -> Conf.OptionSpec -> String -> String
    mergeStr sec opt [] = case Conf.get cp sec opt of
       Left _ -> error $ "Missing settings field '" ++ opt ++ "' in section '" ++ sec ++ "'"
       Right val -> val
    mergeStr _ _ cmd_val = cmd_val
    
    -- merge an int setting
    mergeInt :: Conf.SectionSpec -> Conf.OptionSpec -> Int -> Int
    mergeInt sec opt 0 = case Conf.get cp sec opt of
       Left _ -> error $ "Missing settings field '" ++ opt ++ "' in section '" ++ sec ++ "'"
       Right val -> val
    mergeInt _ _ cmd_val = cmd_val
    
-- | Reads the configuration file, if it doesn't exist, it is created
-- on Unix-like systems: ~/.dart-haskell
-- on Windows: C:/Documents And Settings/user/Application Data/dart-haskell
configSettings :: IO Conf.ConfigParser
configSettings = do
  path_to_config <- getAppUserDataDirectory "dart-haskell"
  config_file_exists <- doesFileExist path_to_config
  when (not config_file_exists) mkConfigFile
  cp <- Conf.readfile Conf.emptyCP path_to_config
  return . forceEither $ cp
  
-- | Creates a config file with default settings
-- on Unix-like systems: ~/.dart-haskell
-- on Windows: C:/Documents And Settings/user/Application Data/dart-haskell
mkConfigFile :: IO ()
mkConfigFile = do
  path_to_config <- getAppUserDataDirectory "dart-haskell"
  writeFile path_to_config contents
  where
    contents = "################################################################################\n"
               ++ "# NOTE:\n"
               ++ "# data Bool = true|on|1|false|off|false\n"
               ++ "\n"
               ++ "[primitive types]\n"               
               ++ "min_int_bound = -100\n"
               ++ "max_int_bound = 100\n"
               ++ "double_min = order of 10^2\n"
               ++ "double_max = order of 10^2\n"
               ++ "\n"
               ++ "[data structures]\n"
               ++ "size = order of 10^2\n"
               ++ "\n"
               ++ "[interpreter]\n"
               ++ "#path to the file that contains the module you want to interpret or test :: FilePath\n"
               ++ "#file =\n"
               ++ "\n"
               ++ "#name of the function to evaluate, defined within the module in $file :: String\n"
               ++ "#evaluate_function =\n"
               ++ "\n"
               ++ "#name of the function to test, defined within the module in $file :: String\n"
               ++ "#test_function =\n"
               ++ "\n"
               ++ "# Print debug messages :: Bool\n"
               ++ "debug = off\n"
               ++ "# Be verbose: prints environment, loaded modules, etc.\n"
               ++ "verbose = off\n"
               ++ "\n"
               ++ "# Maximum amount of seconds permitted to a computation in order to finish (includes tests and evaluations)\n"
               ++ "timeout_seconds = 5\n"
               ++ "\n"
               ++ "################################################################################\n"               
               ++ "# Debugging the interpreter\n"
               ++ "show_heap = off\n"
               ++ "show_expressions = off\n"
               ++ "show_subexpressions = off\n"
               ++ "watch_reduction = off\n"
               ++ "watch_test = off\n"
