#lang parendown scribble/manual
@(require #/for-label racket/base)
@(require #/for-label lathe-comforts)
@(require #/for-label #/only-in racket/contract/base -> any any/c)


@title{Lathe Comforts}

@defmodule[lathe-comforts]

TODO: Write this documentation.



@table-of-contents[]



@section[#:tag "misc"]{Miscellaneous}

TODO: Write this documentation.


@defproc[(pass [arg any/c] [func (-> any/c any)]) any]{
  Invokes the given procedure with the given argument value. In other words, @racket[(pass arg func)] is just like @racket[(func arg)] but in a different order.
}
