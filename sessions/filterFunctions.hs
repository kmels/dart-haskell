-- To be loaded within ghci with :load

import Data.Maybe
import Language.Core.ValueDefinition
import Language.Core.Core
import Language.Core.Parser
import Language.Core.ParseGlue

type FileName = FilePath

filterFunctions :: FileName -> IO [FunctionApplication] 
filterFunctions f = do
  contents <- readFile f
  return $ let module'@(Module _ _ vdefgs) = case parse contents 0 of
                 OkP modl -> modl
                 FailP msg -> error msg
           in mapMaybe vdefgToMaybeTapp vdefgs
  
-- extract the expression of the first function definition in a ExtCore module
firstExpression :: FileName -> IO Exp
firstExpression f = filterFunctions f >>= return . funExp . head

funExp :: FunctionApplication -> Exp
funExp (FunApp i r exp) = exp
