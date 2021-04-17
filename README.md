# lala

A *very* simple language transpilable to everything.

## Idea

The language consists of two parts: static and dynamic.

### Static phase

TLDR: collects external implementations written in a host language

Function implementations in a host language are provided statically, before evaluating any expressions.
A database of available functions is built, to be used in the dynamic phase.

### Dynamic phase

TLDR: applies implementations built in the static phase

The dynamic part allows the user to build *very* simple expressions, transpile them into the host language and evaluate them.
The only operation a dynamic expression can perform is function application, referencing items defined in the static phase.

## REPL

The project is written in pure haskell, which means we get a repl for free:
`stack repl`

Perhaps one day a dedicated repl evaluating dynamic expressions will be added.
