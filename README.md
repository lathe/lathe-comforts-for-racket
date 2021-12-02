# Lathe Comforts for Racket

[![CI](https://github.com/lathe/lathe-comforts-for-racket/actions/workflows/ci.yml/badge.svg)](https://github.com/lathe/lathe-comforts-for-racket/actions/workflows/ci.yml)

Lathe Comforts for Racket is a collection of utilities that are handy for writing Racket code. This is a non-intrusive toolkit; in most cases it should only make certain Racket code easier to write, without substantially changing the architecture of the project it's used in.

Some of these utilities are designed with Parendown in mind. In some cases, Parendown's weak opening brackets make it easier to get by with higher-order functions instead of custom syntax.


## Installation and use

This is a library for Racket. To install it from the Racket package index, run `raco pkg install lathe-comforts`. Then you can put an import like `(require lathe-comforts)` in your Racket program.

To install it from source, run `raco pkg install --deps search-auto` from the `lathe-comforts-lib/` directory.

[Documentation for Lathe Comforts for Racket](http://docs.racket-lang.org/lathe-comforts/index.html) is available at the Racket documentation website, and it's maintained in the `lathe-comforts-doc/` directory.
