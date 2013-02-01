Description
------
Automated Testing for typed-programs as powerful as in [System FC](http://hackage.haskell.org/trac/ghc/wiki/Commentary/Compiler/FC)

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
[#A] is vital
[#B] is relevant
[#C] is luxury

 - [#A] Parse 3 examples, print AST.
 - [#C] Write a lexer for External Core and target ASCII only.

Questions
----
 - PT. Working with packdep ghc-7.4.1, ok?
 - PT. External core or ICore data structure?
 - GHC How updated is wiki/ExternalCore? last change was 5 years ago http://hackage.haskell.org/trac/ghc/wiki/ExternalCore?action=history
 - GHC Where's [utils/ext-core/Language/Core](http://www.haskell.org/pipermail/cvs-ghc/2009-January/047129.html)? Don't know trac nor git enough.
 - GHC testsuite/ still exists? [1](http://hackage.haskell.org/trac/ghc/wiki/Building/RunningTests/Running). Where to find ["attached test"](http://hackage.haskell.org/trac/ghc/ticket/7239#comment:4)?
 
Remarks
----
 - API change. FastString in ghc-7.4.1 is different from the one in HEAD.
 
To report/update in GHC
----
 - Where's the doc for the CORE pragma mentioned here? http://www.haskell.org/ghc/docs/7.0.2/html/users_guide/ext-core.html

External Core bugs in GHC
----
 - Integer literals fail to parse [#5844, Panic on generating Core code](http://hackage.haskell.org/trac/ghc/ticket/5844)

To read
----
[HsSyn](http://permalink.gmane.org/gmane.comp.lang.haskell.cvs.ghc/17611) from [Get EC to work with readline](http://www.mail-archive.com/cvs-all@haskell.org/msg28422.html)

Acknowledgments
----

<a href="http://www.cs.uu.nl/~daan/parsec.html">
 <img src="http://www.cs.uu.nl/~daan/images/parsec.gif"
      alt="Parsec logo" hspace=20 border=0
      style="float: right; border: none; padding-left: 4pt">
</a>
