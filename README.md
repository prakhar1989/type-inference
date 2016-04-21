Hindley Milner Type Inference for Beginners
===

The [Hindley Milner Type Inference](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system) or Algorithm W is a type-inference algorithm that infers types in a programming language.

This repository contains a working implementation written in OCaml to demonstrate type-inference on a small functional language.

Note: This is still under construction and might have bugs. Come back in a month or two when its ready.

### Build
```
$ make
```

### Run the repl

```
$ ./repl.native

Welcome to the REPL.
Type in expressions and let Hindley-Milner Type Inference run its magic.

Out of ideas? Try out a simple lambda expression: (fun x -> x + 10)

> (fun x -> (x + 10) > 20)
(fun x -> (((x: num) + (10: num): num) > (20: num): bool)): (num -> bool)
```

### Thanks
Huge thanks to these [lecture notes](http://www.cs.cornell.edu/courses/cs3110/2011sp/lectures/lec26-type-inference/type-inference.htm) for providing an understandable breakdown of the algorithm.
