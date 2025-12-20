The compiler is written using OCaml.

## Lexing

Lexing is done using OCaml's stdlib `ocamllex`.

## Parsing

Parsing is done using (Menhir)[https://gallium.inria.fr/~fpottier/menhir/], a LR(1) parser generator.

## Type Checking

Type checking is pretty straight forward. The only interesting bits are empty lists and paramteric polymorphism for built in functions.

Empty lists are typed as a type variable and resolved using unification.

The type checker explicitly implements parametric polymorphism for a few built in functions. Type variables are generated each time the function is called and resolved using unification. There is no general mechanism for parametric polymorphism. Thus, the language does not provide parametric polymorphism as a feature.

## Analysis

There is a step that checks functions to ensure that all paths eventually return the expected value.
