#lang parendown/slash scribble/manual

@; lathe-comforts/scribblings/lathe-comforts/knowable.scrbl
@;
@; A pair of types, `unknown` and `known`, intended for values
@; that may or may not have been established yet in the contemporary
@; state of the library ecosystem.

@;   Copyright 2024 The Lathe Authors
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


@title[#:tag "knowable"]{Knowable Values, for Not-Yet-Specified Results}

@defmodule[lathe-comforts/knowable]

@deftech{Knowable values} are a way to represent data that may be missing for some versions of a program but that may eventually be present once the program has been sufficiently upgraded.

Other conventional approaches sometimes seen in these situations are run time errors, @tech{maybe values}, or special-cased sentinel values like @racket[#f]. The use of a knowable value helps to convey the library author's intent that certain data which is absent cannot be relied upon to be absent forever.

There's a little bit of hubris in being so explicit about this intent, since many library ecosystems thrive while having a non-zero tolerance for breaking changes in practice anyway. Nevertheless, this type is offered in case it's useful.


@; TODO KNOWABLE DOCS: Document these.
@;{@[
(provide /own-contract-out
  expressly-possibly-unknown-impl?
  prop:expressly-possibly-unknown
  make-expressly-possibly-unknown-impl
  unknown?)
]}

@deftogether[(
  @defidform[example-unknown]
  @defform[#:link-target? #f (example-unknown)]
  @defform[#:kind "match expander" #:link-target? #f (example-unknown)]
  @defproc[(example-unknown? [v any/c]) boolean?]
)]{
  Struct-like operations which construct and deconstruct a @tech{knowable value} that represents the absence of information.
  
  It's unspecified whether two @tt{example-unknown} values are @racket[equal?].
}

@defproc[(unknown) example-unknown?]{
  Returns a @tech{knowable value} that represents the absence of information. Specifically, this is equivalent to @racket[example-unknown], except that it isn't a match expander.
  
  @; TODO KNOWABLE DOCS: Explain why it isn't a match expander.
}

@deftogether[(
  @defidform[known]
  @defform[#:link-target? #f (known value-expr)]
  @defform[#:kind "match expander" #:link-target? #f (known value-pat)]
  @defproc[(known? [v any/c]) boolean?]
  @defproc[(known-value [inst known?]) any/c]
)]{
  Struct-like operations which construct and deconstruct a @tech{knowable value} that represents the positive presence of information.
  
  Two @tt{known} values are @racket[equal?] if they contain @racket[equal?] elements.
}

@; TODO KNOWABLE DOCS: Document these.
@;{@[
(provide /own-contract-out
  knowable?
  knowable/c
  knowable-bind
  knowable-map
  knowable-if
  knowable->falsable
  falsable->uninformative-knowable
  expressly-knowable-predicate-impl?
  prop:expressly-knowable-predicate
  make-expressly-knowable-predicate-impl
  call-knowable
  make-procedure-impl-for-knowable-predicate
  make-procedure-impl-for-knowable-predicate-with-arity-of-procedure
  makeshift-knowable-predicate)
]}
