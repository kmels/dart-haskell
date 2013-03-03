Description
------
Automated testing for Haskell programs. We take a program, simulate it against likely runs and report inputs that make it crash.

Roadmap
------
 - Compile .hs and interpret expressions -- Pretty much complete, some libraries functions are missing though, please report bugs.
 - Record reduction paths -- TODO 
 - Find paths that were not run using an SMT solver -- TODO
 - Simulate further -- TODO

The idea is motivated by [Godefroid, Patrice and Klarlund, Nils and Sen, Koushik, DART: Directed Automated Random Testing}, PLDI2005, 2005](http://doi.acm.org/10.1145/1065010.1065036)

It reports
------
 - How many functions were run. How many functions seem normal.
 - Which functions terminated because of an error and what kind of parameters make them crash.

Install
------
    > git clone https://github.com/kmels/dart-haskell
    > cd dart-haskell
    > cabal install

Usage examples  
-------
    > dart-haskell -f examples/interpreter/GHC.Num.hs -e numberTen
    > 10

    > dart-haskell -f examples/interpreter/data-constructors.hs -e tree1
    > Branch Branch Leaf 1 Leaf 2 Leaf 3
