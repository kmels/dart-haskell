Description
------
An interpreter for Haskells' External Core Language.

Source Tree description
----
 - ParserCore.y (copied from GHC's compiler/parser/), parses external code syntax to Iface Core
 - TcIface.lhs (copied from GHC's compiler/iface/), converts Iface Core to Core
 - MkExternalCore.lhs (copied from GHC's compiler/coreSyn/), converts Core to External Core


