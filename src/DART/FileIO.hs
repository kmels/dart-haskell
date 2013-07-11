{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ImplicitParams #-}

module DART.FileIO where 

--------------------------------------------------------------------------------
-- Control
import           Control.Monad.IO.Class(liftIO)
------------------------------------------------------------------------------------------
-- DART
import           DART.CmdLine(beVerboseM)
import           DART.DARTSettings
import qualified Data.ByteString.Char8 as Char8
import           Data.ByteString.Lazy.UTF8 (toString)
--------------------------------------------------------------------------------
-- Extcore
import           Language.Core.Core
import           Language.Core.Core (Module)
import           Language.Core.Interpreter.Acknowledge(acknowledgeTypes,acknowledgeVdefgs,acknowledgeModule)
import           Language.Core.Interpreter.Structures
import           Language.Core.ParseGlue
import           Language.Core.Parser

--------------------------------------------------------------------------------
-- System
import           System.Directory (getCurrentDirectory, doesDirectoryExist, doesFileExist, getDirectoryContents)
import           System.Environment
import           System.FilePath (dropExtension,takeExtension)
import           System.IO
import           System.Process(readProcess)

readHcrFile :: (?be_verbose :: Bool) => FilePath -> IO String
readHcrFile filepath = case takeExtension filepath of
  ".hcr" -> do
    when (?be_verbose) $ putStrLn $ "Reading " ++ filepath
    readFile filepath
  ".hs" -> do
    --currentDir <- getCurrentDirectory
    --let pathToFile = currentDir </> filepath
    putStrLn $ "Compiling " ++ filepath
    --putStrLn $ "in " ++ currentDir
    
    inp <- readProcess "ghc" ["--make","-fext-core",filepath] ""
    --inp <- [cmd|ghc --make -fext-core #{filepath} |] 
    
    readHcrFile $ dropExtension filepath ++ ".hcr"
  ".lhs" -> do
    --currentDir <- getCurrentDirectory
    --let pathToFile = currentDir </> filepath
    putStrLn $ "Compiling " ++ filepath
    inp <- readProcess "ghc" ["--make","-fext-core",filepath] ""
    readHcrFile $ dropExtension filepath ++ ".hcr"
  ext -> error $ "Invalid extension when loading " ++ filepath ++ ". Use either an .hcr, .lhs or a .hs file, found: " ++ ext

hasHsExtension :: FilePath -> Bool
hasHsExtension filepath = case takeExtension filepath of
  ".hs" -> True
  ".lhs" -> True
  ".hcr" -> True
  _ -> False

(</>) :: FilePath -> FilePath -> FilePath
p </> c = p ++ "/" ++ c

-- | Every .hcr file corresponds to a haskell module

readModule :: (?be_verbose :: Bool) => FilePath -> IO Module
readModule fp = readHcrFile fp >>= \c -> case parse c 0 of
  OkP m -> return m
  FailP msg -> error msg

-- | Given an absolute filepath (dir or file), find all the haskell source files within (if dir), compile (if needed) and acknowledge its value definitions.
-- We only compile a source code if there is no .hcr file
loadFilePath :: FilePath -> IM Env
loadFilePath filepath = do
  beVerboseM $ "Loading filepath " ++ filepath
  is_dir <- liftIO $ doesDirectoryExist filepath
  is_file <- liftIO $ doesFileExist filepath
  
  env <- if (is_dir)
         then do
           files <- liftIO $ getDirectoryContents filepath
           envs <- mapM (loadFilePath . (</>) filepath ) (filter (not . beginsWithDot) files)
           return $ concat envs
         else 
           if (hasHsExtension filepath && is_file) 
           then do
             stgs <- gets settings
             let ?be_verbose = verbose stgs
             
             -- if it is a .hcr file, read and parse the module.
             module' <- liftIO $ readModule filepath
             settings' <- gets settings
             let ?settings = settings'
             acknowledgeModule module'
           else return []
  return env
  
beginsWithDot :: FilePath -> Bool
beginsWithDot ('.':_) = True
beginsWithDot _ = False

prepend :: FilePath -> FilePath -> FilePath
prepend dir file = dir ++ "/" ++ file
