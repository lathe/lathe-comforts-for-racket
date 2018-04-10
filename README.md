# Lathe Comforts for Racket

[![Travis build](https://travis-ci.org/lathe/lathe-comforts-for-racket.svg?branch=master)](https://travis-ci.org/lathe/lathe-comforts-for-racket)

Lathe Comforts for Racket is a collection of utilities that are handy for writing Racket code. This is a non-intrusive toolkit; in most cases it should only make certain Racket code easier to write, without substantially changing the architecture of the project it's used in.

Some of these utilities are designed with Parendown in mind. In some cases, Parendown's weak opening brackets make it easier to get by with higher-order functions instead of custom syntax.


## Installation and use

This is a library for Racket. To install it, run `raco pkg install --deps search-auto` from the `lathe-comforts/` directory, and then put an import like `(require lathe-comforts)` in your Racket program.

Most of the utilities in Lathe Comforts are pretty unstable. The stable ones will eventually be documented in the `lathe-comforts-doc/` package's documentation.
