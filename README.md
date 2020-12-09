# OCaml effects examples

[![Build Status](https://travis-ci.org/ocaml-multicore/effects-examples.svg?branch=master)](https://travis-ci.org/ocaml-multicore/effects-examples)    

Examples to illustrate the use of algebraic effects in OCaml. See
* [Effective Concurrency with Algebraic Effects](http://kcsrk.info/ocaml/multicore/2015/05/20/effects-multicore/)
* [Pearls of Algebraic Effects and Handlers](http://kcsrk.info/ocaml/multicore/effects/2015/05/27/more-effects/)

## Examples

* [A concurrent round-robin scheduler](https://github.com/kayceesrk/ocaml-eff-example/blob/master/sched.ml)
* [Mutable state](https://github.com/kayceesrk/ocaml-eff-example/blob/master/state.ml)
* [ML-style refs](https://github.com/kayceesrk/ocaml-eff-example/blob/master/ref.ml)
* [Transactional state](https://github.com/kayceesrk/ocaml-eff-example/blob/master/transaction.ml)
* [Asynchronous IO in direct-style](https://github.com/kayceesrk/ocaml-eff-example/blob/master/aio)
* [Delimcc encoding](https://github.com/kayceesrk/ocaml-eff-example/blob/master/delimcc.ml)
* [Dynamic wind](https://github.com/kayceesrk/ocaml-eff-example/blob/master/dyn_wind.ml)
* [Deriving generator from any interator](https://github.com/kayceesrk/ocaml-eff-example/blob/master/generator.ml)
* [Promises](https://github.com/kayceesrk/ocaml-eff-example/blob/master/promises.ml)
* [Backtracking N-Queens](https://github.com/kayceesrk/ocaml-eff-example/blob/master/queens.ml)
* [Monadic reflection](https://github.com/kayceesrk/ocaml-eff-example/blob/master/reify_reflect.ml)
* [MVars](https://github.com/kayceesrk/ocaml-eff-example/blob/master/mvar/MVar.ml)
* [Chameneos-redux](https://github.com/kayceesrk/ocaml-eff-example/blob/master/mvar/chameneos.ml)
* [Memoization](https://github.com/kayceesrk/ocaml-eff-example/blob/master/memo.ml)
* [Nondeterminism](https://github.com/kayceesrk/ocaml-eff-example/blob/master/nondeterminism.ml)
* [A mathematical game: Nim](https://github.com/kayceesrk/ocaml-eff-example/blob/master/nim.ml)
* [Message-passing pipeline: Sieve of Eratostheneses](https://github.com/kayceesrk/ocaml-eff-example/blob/master/eratosthenes.ml)
* [Deep pipes](https://github.com/kayceesrk/ocaml-eff-example/blob/master/pipes.ml)
* [Non termination from effects](https://github.com/kayceesrk/ocaml-eff-example/blob/master/loop.ml)
* [Continuation cloning is tricky](https://github.com/kayceesrk/ocaml-eff-example/blob/master/clone_is_tricky.ml)
* [A solution to the Same Fringe Problem](https://github.com/kayceesrk/ocaml-eff-example/blob/master/fringe.ml)
* [Reverse-mode Algorithmic Differentiation](https://github.com/kayceesrk/effects-examples/blob/master/algorithmic_differentiation.ml)

## Running the examples

Follow the instructions to [install Multicore OCaml](https://github.com/ocaml-multicore/multicore-opam#install-multicore-ocaml). Then,

```bash
$ opam install dune lwt
$ make
```

This builds all of the examples.
