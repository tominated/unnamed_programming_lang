# Unnamed Programming Language

**This is entirely a hobby project - don't expect anything useful from it please!**

This will hopefully be a purely functional language that compiles to JS (maybe wasm if the GC spec gets finalised). It's very heavily inspired by [Gluon](https://gluon-lang.org/) and I'm using a lot of its features as a baseline for my implementation.

The main features I want from this language are:
* ML/Haskell-like syntax
* Static typing with Hindley-Milner inference (or similar)
* Extensible records (row polymorphism)
* ML-like module system using records as modules - modules are just values
* Implicits similar to Gluon/OCaml modular implicits
* A stdlib with all the usual static functional stuff (foldable, traversable, functor, applicative, monad, et al)
* (Stretch goal) Some kind of JS FFI with DOM bindings
* (Stretch goal) Some kind of React.js style UI library
* (Stretch goal) An effect system akin to [Koka](https://koka-lang.github.io/koka/doc/kokaspec.html#sec-effect-types)

If you've read up on Gluon, a lot of these are pretty similar. I would have tried to create a JS backend, but it seems very tied to rust, threads and it's own GC. I also think it'll be a fun learning exprience to start from nothing.

I'm going to try keep the syntax in line with gluon's with some minor tweaks. I namely want to avoid using `#[implicit]` and instead make it a keyword, and I also want to change up the imports a tad.

## Useful reading materials for implementation:

* [Grow your own type system](https://github.com/tomprimozic/type-systems/)
* [Row Polymorphism](https://github.com/willtim/row-polymorphism)
* [Extensible records with scoped labels](https://www.microsoft.com/en-us/research/publication/extensible-records-with-scoped-labels/)
* [Koka: Programming with Row-Polymorphic Effect Types](https://www.microsoft.com/en-us/research/publication/koka-programming-with-row-polymorphic-effect-types/)
