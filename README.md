# OCaml effects examples

[![Build Status](https://travis-ci.org/ocaml-multicore/effects-examples.svg?branch=master)](https://travis-ci.org/ocaml-multicore/effects-examples)    

Examples to illustrate the use of algebraic effects in OCaml. See
* [Effective Concurrency with Algebraic Effects](http://kcsrk.info/ocaml/multicore/2015/05/20/effects-multicore/)
* [Pearls of Algebraic Effects and Handlers](http://kcsrk.info/ocaml/multicore/effects/2015/05/27/more-effects/)

## Examples

* [A concurrent round-robin scheduler](https://github.com/ocaml-multicore/effects-examples/blob/master/sched.ml)
* [Mutable state](https://github.com/ocaml-multicore/effects-examples/blob/master/state.ml)
* [ML-style refs](https://github.com/ocaml-multicore/effects-examples/blob/master/ref.ml)
* [Transactional state](https://github.com/ocaml-multicore/effects-examples/blob/master/transaction.ml)
* [Asynchronous IO in direct-style](https://github.com/ocaml-multicore/effects-examples/blob/master/aio)
* [Dynamic wind](https://github.com/ocaml-multicore/effects-examples/blob/master/dyn_wind.ml)
* [Deriving generator from any interator](https://github.com/ocaml-multicore/effects-examples/blob/master/generator.ml)
* [Promises](https://github.com/ocaml-multicore/effects-examples/blob/master/promises.ml)
* [Monadic reflection](https://github.com/ocaml-multicore/effects-examples/blob/master/reify_reflect.ml)
* [MVars](https://github.com/ocaml-multicore/effects-examples/blob/master/mvar/MVar.ml)
* [Chameneos-redux](https://github.com/ocaml-multicore/effects-examples/blob/master/mvar/chameneos.ml)
* [Message-passing pipeline: Sieve of Eratostheneses](https://github.com/ocaml-multicore/effects-examples/blob/master/eratosthenes.ml)
* [Deep pipes](https://github.com/ocaml-multicore/effects-examples/blob/master/pipes.ml)
* [Non termination from effects](https://github.com/ocaml-multicore/effects-examples/blob/master/loop.ml)
* [Continuation cloning is tricky](https://github.com/ocaml-multicore/effects-examples/blob/master/clone_is_tricky.ml)
* [A solution to the Same Fringe Problem](https://github.com/ocaml-multicore/effects-examples/blob/master/fringe.ml)
* [Reverse-mode Algorithmic Differentiation](https://github.com/ocaml-multicore/effects-examples/blob/master/algorithmic_differentiation.ml)

The original implementation of Multicore OCaml allowed a user to `Obj.clone_continuation`. This has been removed, the examples that used this are in the `multishot` directory. [See this conversation about the removal of this feature](https://discuss.ocaml.org/t/multi-shot-continuations-gone-forever/9072). They now use the [ocaml-multicont](https://github.com/dhil/ocaml-multicont) library.

* [Delimcc encoding](https://github.com/ocaml-multicore/effects-examples/blob/master/multishot/delimcc.ml)
* [Nondeterminism](https://github.com/ocaml-multicore/effects-examples/blob/master/multishot/nondeterminism.ml)
* [Backtracking N-Queens](https://github.com/ocaml-multicore/effects-examples/blob/master/multishot/queens.ml)
* [Memoization](https://github.com/ocaml-multicore/effects-examples/blob/master/multishot/memo.ml)
* [A mathematical game: Nim](https://github.com/ocaml-multicore/effects-examples/blob/master/multishot/nim.ml)

## Running the examples

To run the examples with Multicore OCaml, be sure to install [Opam with these instructions](https://opam.ocaml.org/doc/Install.html).

```bash
# After cloning this repository, create a 5.00 switch
$ opam switch create 5.00.0+trunk
$ opam install . --deps-only
$ make
```

This builds all of the examples. If you want to run a single executable that is build with `dune` you can run:

```
$ dune exec -- ./<executable_name>.exe
```

## External examples

These are other examples that utilise OCaml effect handlers that are not in this repo:

* [Reactive UI and animation](https://gopiandcode.uk/logs/log-bye-bye-monads-algebraic-effects.html)
* [Probabilisitic Programming](https://github.com/Arnhav-Datar/EffPPL)
  + and the [project report](https://github.com/Arnhav-Datar/EffPPL/blob/main/reports/final_report/EffPPL_Report.pdf)
