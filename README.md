# lala

A _very_ simple language transpilable to everything (some day).

## Idea

The project aims to provide a common interface for as many languages as possible, using the simplest possible interface - curried function application.
The idea is to separate trivial function application from not-so-trivial function definition.
Given a large enough library of existing functions, most (all?) programs can be represented by a single expression built purely by applying functions. It is therefore
not necessary to pollute this part of the program with complex logic required to actually define said functions, leaving us with a fairly simple tool that can be
represented in any language, as long as it supports curried function application.

The function application part is referred to below as _dynamic_, while the function definition part is called _static_, for no particular reason.

### Static

TLDR: collects external implementations written in host languages.

Function implementations in a host language are provided statically, before evaluating any expressions.
A database of all predefined implementations is prepared before evaluating any dynamic expressions.

Each static item contains enough information to understand its type (as defined in a custom type system) and to produce a string representation in the target language.
There exists a special kind of static items defined in terms of extended L-Expression, transpilable to any supported type.

### Dynamic

TLDR: connects implementations provided in the static phase.

The dynamic part allows the user to build _very_ simple expressions, transpile them into the host language and evaluate them.
The only operation a dynamic expression can perform is function application, referencing items defined in the static phase.

### L-Expressions

Dynamic expressions are represented using a minimal variant of an [S-expression](https://en.wikipedia.org/wiki/S-expression), internally called L-expressions, e.g.:

```
Add 1 (Add 2 3)
Len (Cons 2 (Cons 1 Nil))
```

L-Expressions are evaluated from left to right, consuming arguments one by one. The left hand side operator is a function, the right hand one
is an argument.

Note, that L-Expressions are basically binary trees - the syntax above is but one way to represent them. As a result, it is fairly trivial to build custom syntaxes
on top of L-Expressions, tailored to specific use cases.

All expressions are type checked - it is not possible to evaluate an invalid expression, as long as static definitions correctly define their types.
The library supports type inference. Inferred type is, however, omitted from the main function's output for convenience.

## REPL

The project is written in pure haskell, which means we get a repl for free:
`stack repl`

A simple, one-off variant is available in `Main.hs` - it will parse an expression, type check it and produce a single string
representing the expression in target language's source code (currently hardcoded to JavaScript):

```
>>> echo "Len (Cons 1 Nil)" | stack run | node -
1
```

## Note

The project is far from being mature and should not be considered stable.
Not that anyone cares, but still.
