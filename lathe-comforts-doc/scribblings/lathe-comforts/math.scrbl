#lang parendown/slash scribble/manual

@; lathe-comforts/scribblings/lathe-comforts/sequence.scrbl
@;
@; Utilities for Racket's numeric types.

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


@(define (tech/racket-reference . body)
  (apply tech #:doc '(lib "scribblings/reference/reference.scrbl") body))


@title[#:tag "math"]{Math: Utilities for Racket's Numeric Types}

@defmodule[lathe-comforts/math]


@defproc[(nan-number? [v any/c]) boolean?]{
  Returns whether the given value is a @racket[number?] and, if so, whether it has any NaN components.
}

@defproc[(non-nan-number? [v any/c]) boolean?]{
  Returns whether the given value is a @racket[number?] and, if so, whether its components are numbers other than NaNs.
}

@defproc[(normalize-non-nan-number [n non-nan-number?]) non-nan-number?]{
  Returns a @racket[non-nan-number?] similar to the given one, but with complex components that are finite inexact numbers updated into their exact rational counterparts, and with complex components that are single-precision infinities updated into their double-precision counterparts. The results of normalization are @racket[equal?] and @racket[equal-always?] when the originals are @racket[=], making these normalizations potentially more suitable as keys of an @racket[equal?]- or @racket[equal-always?]-based hash table.
}

@defproc[(nan-extflonum? [v any/c]) boolean?]{
  Returns whether the given value is an @racket[extflonum?] and, if so, whether it's NaN.
}

@defproc[(non-nan-extflonum? [v any/c]) boolean?]{
  Returns whether the given value is an @racket[extflonum?] and, if so, whether it's an extflonum other than NaN.
}

@defproc[(normalize-non-nan-extflonum [n non-nan-extflonum?]) non-nan-extflonum?]{
  Returns a @racket[non-nan-extflonum?] similar to the given one, but with negative zero updated to be positive zero. The results of normalization are @racket[equal?] and @racket[equal-always?] when the originals are @racket[extfl=], making these normalizations potentially more suitable as keys of an @racket[equal?]- or @racket[equal-always?]-based hash table.
}
