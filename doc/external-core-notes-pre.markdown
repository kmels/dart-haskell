External Core (EC) is a data structure in GHC that can be constructed from a program written in the External Core syntax, where type information is more verbose than in Haskell programs. 

These notes are remarks on the status quo of EC.

<pre class="textdiagram">


                                              /------------------------\
                                              |       Haskell (.hs)    |
                                              |                        |
                                              \------------------------/
                                                            |
                                                            : GHC
                                                            v
/-------------------------------------\        /------------------------\
|            External Core            |        |           Core         |
|                                     |        |                        |
| o data type (coreSyn/ExternalCore)  | <----- |                        |
|                                     |        |                        | 
|                                     |        |                        |
|                                     |        |                        |
\-------------------------------------/        \------------------------/
</pre>

<!-- http://hackage.haskell.org/trac/ghc/attachment/ticket/5844/core.png -->

Role in GHC
----

Relevant files in GHC's source tree
----
 - compiler/coreSyn/MkExternalCore.lhs, converts Core to External Core 

 - ParserCore.y (copied from GHC's compiler/parser/), Grammar file that describe the external code syntax and is parsed to Iface Core (to be read by parser generator Happy) 
 - TcIface.lhs (copied from GHC's compiler/iface/), converts Iface Core to Core
 
 - PprExternalCore (copied from GHC's compiler/coreSyn/), prints the External Core data type to the external core language (into external core's synta

Who uses it
----
[LHC](http://lhc-compiler.blogspot.de/2010/05/limited-release.html)
[Z encoding?](http://osdir.com/ml/lang.haskell.cvs.ghc/2003-10/msg00222.html)

Links
----
 * [Don Stewart's GHC Core overview on Stackoverflow](http://stackoverflow.com/questions/6121146/reading-ghc-core). Includes Z encoding