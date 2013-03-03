{-# LANGUAGE QuasiQuotes #-}

module DART.FileIO where 

import qualified Data.ByteString.Char8 as Char8
import           Data.ByteString.Lazy.UTF8 (toString)
import           Data.Conduit.Process
import           Language.Core.Core (Module)
import           System.Directory (getCurrentDirectory)
import           System.Environment
import           System.FilePath (dropExtension,takeExtension)
import           System.IO
import           System.Process.QQ(cmd)
import Language.Core.Core
import Language.Core.Parser
import Language.Core.ParseGlue

readHcrFile :: FilePath -> IO String
readHcrFile filepath = case takeExtension filepath of
  ".hcr" -> do
    putStrLn $ "Reading " ++ filepath
    readFile filepath
  ".hs" -> do
    currentDir <- getCurrentDirectory
    let pathToFile = currentDir </> filepath
    putStrLn $ "Compiling " ++ pathToFile
    putStrLn $ "in " ++ currentDir
    inp <- [cmd|ghc --make -fext-core #{pathToFile} |] 
    readHcrFile $ dropExtension pathToFile ++ ".hcr"
  ".lhs" -> do
    currentDir <- getCurrentDirectory
    let pathToFile = currentDir </> filepath
    inp <- [cmd|ghc --make -fext-core #{pathToFile} |] 
    readHcrFile $ dropExtension pathToFile ++ ".hcr"
  _ -> error "Invalid extension. Use either an .hcr, .lhs or a .hs file"

(</>) :: FilePath -> FilePath -> FilePath
p </> c = p ++ "/" ++ c

-- | Every .hcr file corresponds to a haskell module

readModule :: FilePath -> IO Module
readModule fp = readHcrFile fp >>= \c -> case parse c 0 of
  OkP m -> return m
  FailP msg -> error msg
