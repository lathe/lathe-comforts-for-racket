#lang parendown/slash scribble/manual

@; lathe-comforts/scribblings/lathe-comforts/sequence.scrbl
@;
@; Utilities for sequences.

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


@title[#:tag "sequence"]{Utilities for Sequences}

@defmodule[lathe-comforts/sequence]


@defproc[
  (endless-sequence/c [element-value/c contract?] ...)
  contract?
]{
  Returns a contract that recognizes a value if it's an endless @tech/racket-reference{sequence} where each element has a number of values equal to the number of @racket[element-value/c] contracts given, and where the values are recognized by these corresponding contracts. The first-order part of the contract merely checks that the value is a @racket[sequence?]; the higher-order part of the contract checks on the sequence elements and the assertion that the sequence never ends. The result of projecting a value through this contract is in general a new sequence, and it doesn't necessarily implement other generic interfaces the original sequence implemented, nor is it necessarily @racket[chaperone-of?] that sequence.
}

@defform[
  #:literals (values)
  
  (sequence* elem-multi-value-expr ... rest-expr)
  
  #:grammar
  [
    (elem-multi-value-expr
      (code:line (values elem-value-expr ...))
      (code:line elem-value-expr))]
  
  #:contracts ([rest-expr sequence?] [elem-value-expr any/c])
]{
  Returns a @tech/racket-reference{sequence} which begins with the given @racket[elem-multi-value-expr] elements and proceeds to have the elements of the given @racket[rest-expr] sequence after that. Like @racket[stream*], this is lazy, forcing the evaluation of each @racket[elem-multi-value-expr] and @racket[tail-expr] only as needed during the iteration of the sequence.
  
  May be used as a @racket[match] pattern. This pattern matches a sequence with at least as many elements as there are @racket[elem-multi-value-expr] patterns. While traversing the sequence to check whether it matches, if it has an element with a number of values that isn't anticipated by the corresponding @racket[elem-multi-value-expr] pattern, then an @racket[exn:fail:contract] exception is raised.
}

@defproc[(sequence-first [s sequence?]) any]{
  Returns the first element of the given @tech/racket-reference{sequence}. The sequence must not be empty.
}

@defproc[
  (sequence-zip*-map
    [sequences (non-empty-listof (sequence/c any/c))]
    [on-element (-> (non-empty-listof any/c) any/c)])
  (sequence/c any/c)
]{
  Converts a non-empty list of @tech/racket-reference{sequences} into a single sequence whose elements are non-empty lists collecting the respective elements of the given sequences. The given sequences must all be of the same length. (Zero and infinity count as lengths for this purpose.)
}

@defproc[
  (endless-sequence-zip*-map
    [sequences (listof (endless-sequence/c any/c))]
    [on-element (-> list? any/c)])
  (endless-sequence/c any/c)
]{
  Converts a list of endless @tech/racket-reference{sequences} into a single endless sequence whose elements are lists collecting the respective elements of the given sequences.
  
  Unlike @racket[sequence-zip*-map], the list of sequences is allowed to be empty, since the resulting sequence's length remains well-defined.
}
