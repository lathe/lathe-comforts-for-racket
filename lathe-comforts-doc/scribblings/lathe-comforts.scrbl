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
@(require #/for-label #/only-in syntax/parse expr id)


@title{Lathe Comforts}

@defmodule[lathe-comforts]

TODO: Write this documentation.



@table-of-contents[]



@section[#:tag "evergreen"]{Evergreen utilities for binding syntax and pure FP}


@subsection[#:tag "binding-syntax"]{Binding syntax utilities}

@defidform[#:kind "splicing syntax class" binds]{
  Matches syntax in any of three formats:
  
  @itemlist[
    @item{
      A single syntax object which is a list of two-element lists which each contain an identifier and an expression. This uses the pattern @racket[([_var:id _val:expr] ...)], and the user writes something like @racket[(w- ([_a 1] [_b 2]) (+ _a _b))] when they use this format.
    }
    @item{
      A single syntax object which is a list of an even number of elements which alternate between identifiers and expressions. This uses the pattern @racket[[(~seq _var:id _val:expr) ...]], and the user writes something like @racket[(w- [_a 1 _b 2] (+ _a _b))] when they use this format.
    }
    @item{
      An even number of syntax objects alternating between identifiers and expressions, preceding the end of the list being matched or preceding another syntax object (not included in the match) which is not an identifier. This uses the head pattern @racket[(~seq (~seq _var:id _val:expr) ... (~peek-not __:id))], and the user writes something like @racket[(w- _a 1 _b 2 (+ _a _b))] when they use this format.
    }
  ]
  
  In all cases, this binds two attributes of ellipsis depth 1, namely @racket[_var] and @racket[_val], and they carry the same number of matches.
  
  (See @racket[_], @racket[expr], and @racket[id].)
}

@defidform[#:kind "splicing syntax class" bindbody]{
  Matches according to the @racket[binds] splicing syntax class, then matches zero or more expressions after that (as in the @racket[expr] syntax class).
  
  This binds three attributes. The @racket[_var] and @racket[_val] attributes are of ellipsis depth 1 and carry the same number of matches. The @racket[_body] attribute is of ellipsis depth 1 as well, but its number of matches is independent of the others.
}


@subsection[#:tag "fp"]{Functional programming utilities}


@subsubsection[#:tag "bindings-and-recursion"]{Bindings and recursion}

@defproc[(pass [arg any/c] [func (-> any/c any)]) any]{
  Invokes the given procedure with the given argument value. In other words, @racket[(pass arg func)] is just like @racket[(func arg)] but in a different order.
}

@defform[(w- bindbody)]{
  TODO: Document this.
}

@defform[(fn arg ... body-expr)]{
  TODO: Document this.
}

@defform[(w-loop next-id bindbody)]{
  TODO: Document this.
}

@defform[(loopfn next-id arg ... body-expr)]{
  TODO: Document this.
}


@subsubsection[#:tag "conditionals"]{Conditionals}

@defform[(mat val-expr pat then-expr else-expr ...)]{
  TODO: Document this.
}

@defform[(matfns val-expr pat then-expr elsefn-expr)]{
  TODO: Document this.
}

@defform[(expect val-expr pat else-expr then-expr ...)]{
  TODO: Document this.
}

@defform[(expectfn pat else-expr then-expr ...)]{
  TODO: Document this.
}

@defform[(dissect val-expr pat then-expr ...)]{
  TODO: Document this.
}

@defform[(dissectfn pat then-expr ...)]{
  TODO: Document this.
}


@section[#:tag "test"]{Unit testing utilities}

@defproc[(destx [x any/c]) any]{
  Deeply traverses the given datum and converts every syntax object inside to a datum. This simply uses @racket[datum->syntax] followed by @racket[syntax->datum].
}
