#lang parendown/slash scribble/manual

@; lathe-comforts/scribblings/lathe-comforts/syntax.scrbl
@;
@; Utilities for macros.

@;   Copyright 2025 The Lathe Authors
@;
@;   Licensed under the Apache License, Version 2.0 (the "License");
@;   you may not use this file except in compliance with the License.
@;   You may obtain a copy of the License at
@;
@;       http://www.apache.org/licenses/LICENSE-2.0
@;
@;   Unless required by applicable law or agreed to in writing,
@;   software distributed under the License is distributed on an
@;   "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
@;   either express or implied. See the License for the specific
@;   language governing permissions and limitations under the License.


@(require lathe-comforts/scribblings/private/shim)
@(init-shim)


@title[#:tag "syntax"]{
  Utilities for Macros
}

@defmodule[lathe-comforts/syntax]

The @tt{lathe-comforts/syntax} module defines utilities for use at macroexpansion time.

Some of these utilities are useful for parsing syntax in a way that requires a property we call @deftech{autopticity}. We consider a macro call to be @deftech{autoptic} when all the cons cells, keywords, @racket[unquote]s, and other structural elements that need to be crawled into or checked for identity (excluding things at the leaves like expressions, data literals, and identifiers not given meaning by the macro's own design) are all right there at the macro call site, where we can see them all at once. In technical detail, a syntax object representing one of these cons cells or other structural elements is required to have a set of scopes that's equal to or a superset of the set of scopes on the overall macro call, as though the macro call has created local bindings of what these particular structural elements mean in these particular positions. This helps ensure that even though Racket expressions are often made of cons cells, an arbitrary Racket expression inserted into one of these positions by a macro's syntax template will not have its cons cells misinterpreted.

An example of a situation which would be ameliorated by use of autoptic parsing is the @racket[cond] syntax. Racket's @racket[cond] is extended by both a @racket[=>] clause notation and the ability to use internal definitions. Someone who wants to define another control flow operation that allows internal definitions might simply insert the user's definitions into the cond body, but this can inadvertently allow a user to write @racket[=>] at the start of the body as a way to elicit unintended behavior from the control flow abstraction. If the @racket[=>] notation were recognized as a structural element whose meaning was bound by the @racket[cons] form, then it would simply not be in scope within the user's inserted definitions, just as for any other local binding within a macroexpansion template.

@defproc[(scopes-empty? [stx syntax?]) boolean?]{
  Returns whether the scopes of the given syntax object are the empty set.
}

@defproc[(scopes<=? [a syntax?] [b syntax?]) boolean?]{
  Returns whether the scopes of the first given syntax object are a subset of the scopes of the second one.
}

@(define @also-enforces-autopticity[]
  @list{
    This syntax itself must also be called with @tech{autopticity}. An occurrence of a cons cell that's part of the call syntax must have a set of scopes that's equal to or a superset of the set of scopes on the entire call, as though the call has created a local binding of what a cons cell means in these positions. This helps ensure that even though Racket expressions are often made of cons cells, an expression inserted into one of these positions by a macro's syntax template will not have its cons cells misinterpreted.
  })

@(define @pattern-that-ensures-autopticity[]
  @list{
    Parsing using this pattern helps to ensure @tech{autopticity} of the call site being parsed.
    
    @also-enforces-autopticity[]
  })

@defform[
  #:kind "pattern expander"
  {~autoptic-list-to surrounding-stx pattern}
  #:contracts ([surrounding-stx syntax?])
]{
  A @tech[#:doc syntax-doc]{pattern expander} that parses according to @racket[pattern], but only if the syntax object is a syntax list, each of whose syntax object tails has a set of scopes that's equal to or a superset of the set of scopes on the given syntax object @racket[surrounding-stx].
  
  @pattern-that-ensures-autopticity[]
}

@defform[#:kind "pattern expander" {~autoptic pattern}]{
  A @tech[#:doc syntax-doc]{pattern expander} that parses according to @racket[pattern], but only if the syntax object has a set of scopes that's equal to or a superset of the scopes of the syntax object @racket[this-syntax] that's currently being parsed.
  
  @pattern-that-ensures-autopticity[]
}

@defform[#:kind "pattern expander" {~autoptic-list pattern}]{
  A @tech[#:doc syntax-doc]{pattern expander} that parses according to @racket[pattern], but only if the syntax object is a syntax list, each of whose syntax object tails has a set of scopes that's equal to or a superset of the set of scopes on the syntax object @racket[this-syntax] that's currently being parsed.
  
  @pattern-that-ensures-autopticity[]
}
