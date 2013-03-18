{-# LANGUAGE ImplicitParams #-}

module DART.CmdLine where

import           Control.Monad(when)
import           Control.Monad.IO.Class
import           Control.Monad.State(gets)
import           DART.InterpreterSettings
import qualified Data.HashTable.IO as H
import           Language.Core.Core -- Exp
import           Language.Core.Interpreter.Structures
import           Language.Core.Util(showExp)
import Control.Monad.State.Class(modify)

-- | Prints a debug message with the number of the current reduction prepended
debugMStep :: String -> IM ()
debugMStep msg = prependStep >> debugM msg

-- | Prints a debug message that indicates the end of the current reduction
-- it increases then the step
debugMStepEnd :: IM ()
debugMStepEnd = debugM "}" -- append the number, print it
                
-- | Prints the reduction number
prependStep :: IM () 
prependStep = do
  n <- gets number_of_reductions -- get the number 
  debugMNLNT . flip (++) "." . (++) (replicate n '*' ++ " ") . show $ n -- preceed the number, print it
  modify increase_number_of_reductions -- and then, increase the number
              
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
    in io . putStrLn $ (tab ++ msg) 

-- | Prints a debug message with a new line at the end 
-- and without a prepended tab
debugMNT :: String -> IM ()
debugMNT msg = do 
  s <- gets settings
  when (debug s) $
    io . putStrLn $ msg

-- | Prints a debug message without a new line at the end
-- and without a prepended type
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
            
printHeap :: (?settings :: InterpreterSettings) => Heap -> IO ()
printHeap heap = do
  putStrLn $ "-------------------- Heap begins --------------------" 
  H.mapM_ printVar heap
  putStrLn $ "-------------------- Heap ends --------------------" 
  where
    showVal :: Either Thunk Value -> String
    showVal (Left t) = "Thunk"
    showVal (Right v) = show v
    
    printVar (id,val) = putStrLn $ show id ++ " => " ++ showVal val

-- | If we are in the IM Monad, we might want to print expressions being reduced as they are interpreted. 
-- If the flag --show-subexpressions was specified, shows the given expression
debugSubexpression :: Exp -> IM ()
debugSubexpression e = do
  s <- gets settings  
  whenFlag show_subexpressions $ indentExp e >>= debugM . (++) "Sub-expression: "

-- | If we are in the IM Monad, we might want to watch expressions being reduced as they are interpreted. 
-- If the flag --watch-reduction was specified, prints a debug message.
watchReductionM :: String -> IM ()
watchReductionM msg = whenFlag watch_reduction $ debugM msg

whenFlag :: (InterpreterSettings -> Bool) -> IM () -> IM ()
whenFlag f a = do
  sttgs <- gets settings
  when (f sttgs) $ a
  
indentExp :: Exp -> IM String
indentExp e = gets tab_indentation >>= \ti -> let ?tab_indentation = ti in return $ showExp e
