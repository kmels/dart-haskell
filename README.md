Description
------
An interpreter for Haskell's External Core Language.

Source Tree description
----
 - ParserCore.y (copied from GHC's compiler/parser/), Grammar file that describe the external code syntax and is parsed to Iface Core (to be read by parser generator Happy) 
 - TcIface.lhs (copied from GHC's compiler/iface/), converts Iface Core to Core
 - MkExternalCore.lhs (copied from GHC's compiler/coreSyn/), converts Core to External Core
 - PprExternalCore (copied from GHC's compiler/coreSyn/), prints the External Core data type to the external core language (into external core's syntax)



