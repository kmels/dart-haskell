Description
------
An interpreter for Haskells' External Core Language.

Source Tree description
----
 - ParserCore.y (copied from GHC's compiler/parser/), parses external code syntax to Iface Core
 - TcIface.lhs (copied from GHC's compiler/iface/), converts Iface Core to Core
 - MkExternalCore.lhs (copied from GHC's compiler/coreSyn/), converts Core to External Core
 - PprExternalCore (copied from GHC's compiler/coreSyn/), prints the External Core data type to the external core language (into external core's syntax)



