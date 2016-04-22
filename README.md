Hindley Milner Type Inference
===

[![Build Status](https://travis-ci.org/prakhar1989/type-inference.svg?branch=master)](https://travis-ci.org/prakhar1989/type-inference)

The [Hindley Milner Type Inference](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system) or Algorithm W is a type-inference algorithm that infers types in a programming language.

This repository contains a working implementation written in OCaml to demonstrate type-inference on a small functional language.

### λ-calculus

The language that this implementation works on is a small subset called the [lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus). In essence, the lambda calculus allows one to express any computation purely in terms of anonymous functions and application of these functions.
```ocaml
> (fun x -> x * x)          (* function declaration *)
> (fun x -> x * x) 10       (* function application *)
```
In pure lambda calculus, [numerals](https://en.wikipedia.org/wiki/Church_encoding#Church_numerals) and [booleans](https://en.wikipedia.org/wiki/Church_encoding#Church_Booleans) are also expressed in terms of functions but to make it easy, the language supports integer and boolean literals, alongwith binary operations such as addition, multiplication, boolean and etc.

##### Types

Before we jump on to the type-inference algorithm, we need to define the types in our language. There are three primitive types that our language supports -

- `int`: An integer type for integer literals. Binary operations such as `+` and `*`, work only on integers and return an integer type.
- `bool`: Our language has boolean literals `true` and `false`, both of which have a `bool` type. To operate on bools `&&` and `||` are provided. Lastly, two additional operators `>` and `<` work on any type, but return a bool type.
- `T -> U`: The function type where the `T` is the type of the input and `U` is the return type of the function. So for example, a square function above has a type `int -> int`.

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
int
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
