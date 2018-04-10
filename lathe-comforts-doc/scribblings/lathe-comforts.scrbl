#lang parendown scribble/manual

@; lathe-comforts/scribblings/lathe-comforts.scrbl
@;
@; Evergreen utilities.

@;   Copyright 2018 The Lathe Authors
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


@(require #/for-label racket/base)
@(require #/for-label lathe-comforts)
@(require #/for-label #/only-in racket/contract/base -> any any/c)


@title{Lathe Comforts}

@defmodule[lathe-comforts]

TODO: Write this documentation.



@table-of-contents[]



@section[#:tag "evergreen"]{Evergreen utilities for binding syntax and pure FP}


@subsection[#:tag "binding-syntax"]{Binding syntax utilities}

TODO: Write this section.


@subsection[#:tag "fp"]{Functional programming utilities}


@subsubsection[#:tag "bindings-and-recursion"]{Bindings and recursion}

TODO: Document the rest of the utilities in this section.

@defproc[(pass [arg any/c] [func (-> any/c any)]) any]{
  Invokes the given procedure with the given argument value. In other words, @racket[(pass arg func)] is just like @racket[(func arg)] but in a different order.
}


@subsubsection[#:tag "conditionals"]{Conditionals}

TODO: Write this section.


@section[#:tag "test"]{Unit testing utilities}

@defproc[(destx [x any/c]) any]{
  Deeply traverses the given datum and converts every syntax object inside to a datum. This simply uses @racket[datum->syntax] followed by @racket[syntax->datum].
}
