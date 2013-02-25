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

Install
------
    > git clone https://github.com/kmels/dart-haskell
    > cd dart-haskell
    > cabal install

Usage examples  
-------
    > dart-haskell -f examples/interpreter/GHC.Num.hs -e numberTen
10

    > dart-haskell -f examples/interpreter/GHC.Num.hs --show-heap

-------------------- Heap begins --------------------
main:FirstOrderFunctions.numberTen => 10
main:FirstOrderFunctions.numberEleven => 11
main:FirstOrderFunctions.numberFourtyTwo => 82
main:FirstOrderFunctions.isHundredLargerThanZZero => True
main:FirstOrderFunctions.isTenEven => False
main:FirstOrderFunctions.numberTwenty => 20
main:FirstOrderFunctions.numberTwentyTwo => 22
-------------------- Heap ends --------------------

    > dart-haskell --help
Reads a .hcr file and evaluates its declarations.

interpret [OPTIONS]

USAGE:
  -f --file=FILE          
  -e --eval=FUNCTION_NAME   The function to evaluate (if not provided, all
                            function declarations will be evaluated)
DEBUG:
  -d --debug                Be verbose about what this program is doing
     --show-heap            Shows binded values in the heap
     --show-expressions     Shows the external core expression for every
                            value being evaluated
     --show-subexpressions  Shows *every* (external core) expression being
                            evaluated
     --show-tmp-variables   Shows debug messages for temporal variables (if
                            depends flag is on)
  -? --help                 Display help message
  -V --version              Print version information
