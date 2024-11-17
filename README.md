# Caml-doku
Two sudoku solver algorithms for COMP302 (Programming Languages and Paradigms) at McGill University\
By Raina, Polly, Lauren\
\
Our backtracking algorithm is in `bruteforce.ml` and we also represented Sudoku as a Constraint Satisfaction Problem which we solved using Microsoft's Z3. The code is inside `B/bin/main.ml`
\
\
To initialize the environment:\
run `opam switch`, you should see an arrow next to the version you want\
run `opam init` and whatever it prompts you to do\
double check that everything works by running some file with `ocaml [filename.ml]` (i suggest validator.ml after uncommenting the last couple lines)\
\
To run the Z3 solver, you must have dune set up. HIghly recommend doing this in a linux environment, i spent way too long bashing my head against a block of tofu trying to do this on windows \
`cd B`\
`dune build`\
`dune exec B`\
\
to compile: ocamlopt -o [output filename] [compiled filename].ml\
to run compiled: ./[output filename]\

Sudoku board is of type int list list
