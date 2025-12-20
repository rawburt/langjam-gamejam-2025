This project is a submission for Langjam Gamejam 2025.

This project contains a programming language, a JavaScript game engine, and a game built using both.

## The Programming Language

The programming language is a statically typed programming language which compiles to JavaScript. The compilation process specifically targets the game engine. The compiler is written in [OCaml](https://ocaml.org/).

See [docs/LANGUAGE.md](docs/LANGUAGE.md) for more information.

## The Game Engine

The game engine is built using JavaScript. The engine code is located at [./engine/engine.js](./engine/engine.js). There are some helper functions for simplifying the programming language compilation process. [Lodash](https://lodash.com/) is used for structural equality.

## The Game

The game is a simple 2D adventure game. To compile the game:

```sh
sh build.sh
```

Then open `engine/index.html` in a browser and play the game.

Alternatively, play the game online at [https://rawburt.itch.io/call-of-the-void](https://rawburt.itch.io/call-of-the-void).
