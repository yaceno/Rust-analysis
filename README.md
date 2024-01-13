# Rust-analysis
Lexical and syntactical analysis on a simplified version of Rust langage as part of a course on compilation.

## Prerequisites

You must have Ocaml installed in order to use the Analyser.

## Usage 

Launch `make` then use the executable `./rustine` with a .rs (there are many files in the folder tests) file to print its AST tree.

## Futures improvements

- Currently some tests do not pass lexical analysis correctly, will correct it in the future
- Correct the conflicts due to the syntactical analysis
- Make a deep semantic analyzer
- Extend the analyser to understand complete Rust langage
