# hiccup
A hybrid functional programming language I'm designing for fun. Originally started as an exercise in learning Haskell while also practicing writing a parser by hand.

Currently includes a lexer, a recursive-descent parser, and an interpreter. A compiler for the language is also under construction. If I decide to keep pursuing it, I may switch to a parser generator to support a more complex language grammar.

## Run the REPL

Simply compile the Repl.hs file with GHC to test out Hiccup (e.g. `ghc Repl.hs -outputdir build`). Run the resulting executable via the command line. Running it with no arguments takes you to the REPL. You can also pass it a file as an argument. It will then execute the file, print the results, and then take you to a REPL. You can then access functions and variables from that source file from the REPL.

## Run the Compiler

To build the compiler, build the Hiccup.hs file with GHC. The resulting executable takes a file as an argument.
