Description
------
Automated Testing for typed-programs as powerful as in [http://hackage.haskell.org/trac/ghc/wiki/Commentary/Compiler/FC](System FC)

We take a module expressed in Haskell's External Core (EC) language and through the following steps, simulate the program against likely runs to test it:
 - parse EC syntax [Doing]
 - feed the program (functions) and evaluate (symbolically) some steps, testing [todo]
 - find paths that were not run using an SMT solver [todo]
 - evaluate more steps, testing [todo]

It reports
 - How many functions were run. How many functions seem normal.
 - Which functions terminated because of an error and what kind of parameters make them crash.

TODO
----
 [#A] Parse 3 examples, print AST.
 [#C] Write a lexer for External Core and target ASCII only.

[#A] is vital
[#B] is relevant
[#C] is luxury

Questions
----
 - Working on HEAD, ok?
 - How updated is wiki/ExternalCore? last change was 5 years ago http://hackage.haskell.org/trac/ghc/wiki/ExternalCore?action=history
 - External core or ICore?

Remarks
----
 - API change. FastString in ghc-7.4.1 is different from the one in HEAD.
 
To report/update in GHC
----
 - Where's the doc for the CORE pragma mentioned here? http://www.haskell.org/ghc/docs/7.0.2/html/users_guide/ext-core.html

External Core bugs in GHC
----
 - Integer literals fail to parse [#5844, Panic on generating Core code](http://hackage.haskell.org/trac/ghc/ticket/5844)
