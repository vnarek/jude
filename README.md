# Jude - an actor model written in OCaml

## Requirements

* OCaml > 4.08
* Dune  > 2.4

## Running examples

1) Clone the repository
2) Create local switch
```sh
opam switch create ./ ocaml-base-compiler.4.10.0
```
3) Run
```sh
opam install -t jude
```
4) Try examples/tests
```sh
dune exec $EXAMPLE_PATH
dune test
```
5) Generate documentation
```sh
dune build @doc
```

## Name

Jude was named after a character from the book called "A Little life" because
both are good actors (hopefully).