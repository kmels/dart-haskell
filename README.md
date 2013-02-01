Description
------
Automated Testing for typed-programs as powerful as in [System FC](http://hackage.haskell.org/trac/ghc/wiki/Commentary/Compiler/FC)

We take a module expressed in Haskell's External Core (EC) language and through the following steps, simulate the program against likely runs to test it:

 - parse EC syntax [Done](http://hub.darcs.net/kmels/hs-dart/browse/src/Main.lhs)
 - feed the program (functions) and evaluate (symbolically) some steps, testing [todo] 
 - find paths that were not run using an SMT solver [todo]
 - evaluate more steps, testing [todo]

The idea is motivated by [Godefroid, Patrice and Klarlund, Nils and Sen, Koushik, DART: Directed Automated Random Testing}, PLDI2005, 2005](http://doi.acm.org/10.1145/1065010.1065036)

It reports

 - How many functions were run. How many functions seem normal.
 - Which functions terminated because of an error and what kind of parameters make them crash.
