Hindley Milner Type Inference
===

[![Build Status](https://travis-ci.org/prakhar1989/type-inference.svg?branch=master)](https://travis-ci.org/prakhar1989/type-inference)

The [Hindley Milner Type Inference](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system) or Algorithm W is a type-inference algorithm that infers types in a programming language.

This repository contains a working implementation written in OCaml to demonstrate type-inference on a small functional language.

Note: This is still under construction and might have bugs. Come back in a month or two when its ready.

### REPL
The project ships with an interactive Read-Eval-Print-Loop (REPL) that you can use to play with the algorithm. To build the REPL, you need OCaml installed. Compile the REPL with `make` and if all goes well, you should be good to go.

```
$ ./repl

Welcome to the REPL.
Type in expressions and let Hindley-Milner Type Inference run its magic.

Out of ideas? Try out a simple lambda expression: (fun x -> x + 10)

> 10 + 20 > 40
bool
> (fun x -> (x && true) || false)
(bool -> bool)
> (fun x -> x + 10) 20
num
> (fun f -> f 3)
((int -> 'a) -> 'a)
>  (fun f -> (fun g -> (fun x -> f (g x))))
(('a -> 'b) -> (('c -> 'a) -> ('c -> 'b)))
```

### Tests

To run the tests, you need [Alcotest](https://github.com/mirage/alcotest) package installed. Install it by running `opam install alcotest`.

```
$ make test
```

### Thanks
Huge thanks to these [lecture notes](http://www.cs.cornell.edu/courses/cs3110/2011sp/lectures/lec26-type-inference/type-inference.htm) for providing an understandable breakdown of the algorithm.
