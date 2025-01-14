#lang parendown/slash scribble/manual

@; lathe-comforts/scribblings/lathe-comforts/yknow.scrbl
@;
@; Yknow objects, a representation for pending computations that can
@; turn out to allow user-specified results.

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


@title[#:tag "yknow"]{Yknow Objects: Sometimes-User-Specified Computations}

@defmodule[lathe-comforts/yknow]

@deftech{Yknow objects} are a way to represent @racket[promise?]-like computations that may turn out to have not-yet-specified or user-specifiable results. The not-yet-specified results may be missing for some versions of a program but may eventually be present once the program has been sufficiently upgraded, as with @tech{knowable values}. The user-specifiable results represent a stable absence of information that the user can handle in the way they see fit, as with @tech{maybe values}. Specifically, a yknow object is expressible in terms of a @racket[promise?] containing a @tech{knowable value} containing a possible @tech{maybe value} containing a possible @racket[promise?].


@section[#:tag "yknow-proper"]{Yknow Objects Themselves}

@defproc[(yknow? [v any/c]) boolean?]{
  Returns whether the given value is a @tech{yknow object}.
}

@defproc[
  (yknow-value-promise-maybe-knowable-promise [y yknow?])
  (promise/c (knowable/c (maybe/c (promise/c any/c))))
]{
  Given a @tech{yknow object}, returns a @racket[promise?] which results in a @tech{knowable value} which may contain a @tech{maybe value} which may contain a @racket[promise?] resulting in the yknow object's value. If the yknow object's computation results in a fully specified value, then the knowable value will be @racket[known?], and the maybe value will be a @racket[just?]. If the computation turns out to have a user-specified result, the maybe value will be a @racket[nothing?]. If it turns out to be unspecified whether or not the result is user-specified, then the knowable value will be @racket[unknown?].
}

@defproc[(yknow/c [c contract?]) contract?]{
  Returns a contract that recognizes a @tech{yknow object} if the object's computation result is either not fully specified or is recognized by the given contract.
}

@defproc[
  (make-yknow-from-value-promise-maybe-knowable-promise
    [value-pmkp (promise/c (knowable/c (maybe/c (promise/c any/c))))])
  yknow?
]{
  Returns a @tech{yknow object} whose computation works by forcing the given promise. Its computation result is not-yet-specified if the promise's result is @racket[unknown?]. It turns out to be user-specified if the promise's result is a @racket[known?] @racket[nothing?]. It's fully specified if the promise's result is a @racket[known?] @racket[just?], and in that case the computation can be resumed by forcing the second promise held within.
}

@defproc[
  (make-yknow-from-value-knowable-promise
    [value-kp (promise/c (knowable/c any/c))])
  yknow?
]{
  Returns a @tech{yknow object} whose computation works by forcing the given promise. Its computation result is not-yet-specified if the promise's result is @racket[unknown?]. Otherwise, it's fully specified as the @racket[known-value] contained within, and there's no further computation to do.
}

@defproc[(make-yknow-from-value-knowable [value-k knowable?]) yknow?]{
  Returns a @tech{yknow object} which has no computation to do. Its computation result is not-yet-specified if the given @tech{knowable value} is @racket[unknown?]. Otherwise, it's fully specified as the @racket[known-value] contained within.
}

@defproc[(make-yknow-from-value [value any/c]) yknow?]{
  Returns a @tech{yknow object} which has no computation to do and simply has the given value as its fully specified result.
}

@defproc[(uninformative-yknow) (yknow/c none/c)]{
  Returns a @tech{yknow object} which has no computation to do and simply has a not-yet-specified computation result.
}

@defproc[(yknow-value-promise-knowable [y yknow?]) (knowable/c (promise/c any/c))]{
  Given a @tech{yknow object}, runs part of its computation. If the computation result turns out to be not-yet-specified or user-specified, this returns an @racket[unknown?] result. Otherwise, this returns a @racket[known?] result that contains the @racket[promise?] that computes the yknow object's fully specified result value.
  
  Note that this flattens the sometimes useful distinction between a yknow object with a not-yet-specified result and a yknow object with a user-specified result.
}

@defproc[(yknow-value-knowable [y yknow?]) knowable?]{
  Given a @tech{yknow object}, runs its computation. If the computation result turns out to be not-yet-specified or user-specified, this returns an @racket[unknown?] result. Otherwise, this returns a @racket[known?] result that contains the yknow object's fully specified result value.
  
  Note that this flattens the sometimes useful distinction between a yknow object with a not-yet-specified result and a yknow object with a user-specified result.
}

@defproc[(yknow-known-specified? [v any/c]) boolean?]{
  Given a value, checks whether it's a @tech{yknow object} (returning @racket[#f] if it isn't), runs part of its computation if it is one, and returns whether that computation's result is fully specified.
}

@defproc[(yknow-value [y yknow-known-specified?]) any/c]{
  Given a @tech{yknow object} which is known to have a fully specified result, runs its computation, and returns that result value.
}

@defproc[
  (yknow-value-promise-maybe-knowable-promise-map
    [y yknow?]
    [on-value-pmkp
      (-> (promise/c (knowable/c (maybe/c (promise/c any/c))))
        (promise/c (knowable/c (maybe/c (promise/c any/c)))))])
  yknow?
]{
  Given a @tech{yknow object}, returns a new one obtained by updating its @racket[yknow-value-promise-maybe-knowable-promise] result according to the given function.
}

@defproc[
  (yknow-value-promise-maybe-knowable-map
    [y yknow?]
    [on-value-pmk
      (-> (knowable/c (maybe/c (promise/c any/c)))
        (knowable/c (maybe/c (promise/c any/c))))])
  yknow?
]{
  Given a @tech{yknow object}, returns a new one obtained by updating its @racket[yknow-value-promise-maybe-knowable-promise] result's @racket[force] computation result according to the given function. The function won't be called until the resulting yknow object's computation is partly forced.
}

@defproc[
  (yknow-map [y yknow?] [on-value (-> any/c any/c)])
  yknow?
]{
  Given a @tech{yknow object}, returns a new one obtained by updating its fully specified result value (if any) according to the given function. The function won't be called until the resulting yknow object's computation is fully forced.
}

@defproc[
  (yknow-zip*-map
    [y-list (listof yknow?)]
    [on-value (-> list? any/c)])
  yknow?
]{
  Given a list of @tech{yknow objects}, returns a new one obtained by updating their fully specified result values (if they all have them) according to the given function. If any of the given yknow objects has a result that's not-yet-specified or user-specified, then the resulting yknow object has a not-yet-specified result. The function won't be called until the resulting yknow object's computation is fully forced.
  
  Note that this flattens the sometimes useful distinction between a yknow object with a not-yet-specified result and a yknow object with a user-specified result.
}

@defproc[
  (yknow-map/knowable
    [y yknow?]
    [on-value-knowable (-> any/c knowable?)])
  yknow?
]{
  Given a @tech{yknow object}, returns a new one. When the new one has its computation partly forced, it fully forces the original computation. If the original computation turns out to have a not-yet-specified or user-specified result, then that revelation is passed along as-is. Otherwise, the given function is called on the original computation's fully specified result value. If the function returns an @racket[unknown?] result, then the resulting yknow object's computation turns out to be not-yet-specified. Otherwise, it turns out to be fully specified as the @racket[known-value] of the function's result (with no additional computation pending).
}

@defproc[
  (yknow-joininfo*-resumably
    [y-list (listof yknow?)]
    [on-known-specified
      (-> (promise/c any/c) (listof yknow?) (promise/c any/c))])
  yknow?
]{
  Given a list of @tech{yknow objects}, returns a new one whose computation works by attempting each of their computations in turn until one of them has a not-yet-specified or fully-specified result, and then passing the remainder of the computation (as a @racket[promise?]) and a list of the remaining given yknow objects to the given @racket[on-known-specified] function to obtain a new computation for a fully specified result (as another @racket[promise?]). If it turns out all the given yknow objects' computations all have user-specified results, then so does the resulting yknow object's computation.
  
  This is a way of exercising one's prerogative as a user to replace a user-specified result with a fully specified one.
  
  If there's no reason to process further yknow objects in the list and the known-specified computation tail doesn't need to be updated in any way, then @racket[yknow-joininfo*] should be sufficient. The extra flexibility of @tt{yknow-joininfo*-resumably} is so that it's easy to join the information of data structures that involve multiple yknow object layers, as seen in @racket[yknow-maybe-yknow-joininfo*].
}

@defproc[(yknow-joininfo* [y-list (listof yknow?)]) yknow?]{
  Given a list of @tech{yknow objects}, returns a new one whose computation works by attempting each of their computations in turn until one of them has a not-yet-specified or fully-specified result, and then using that outcome as-is without proceeding to attempt the others. If it turns out all the given yknow objects' computations have user-specified results, then so does the resulting yknow object's computation.
  
  This is a way of exercising one's prerogative as a user to replace a user-specified result with a fully specified one.
  
  This operation is idempotent and associative. If no more than one yknow object in the list has a fully specified result, or if their fully specified results are equivalent to each other, then this operation is also commutative.
}

@defproc[
  (maybe-min-yknow-zip*-map
    [y-list (listof (yknow/c maybe?))]
    [on-value (-> list? any/c)])
  (yknow/c maybe?)
]{
  Given a list of @tech{yknow objects} possibly resulting in @tech{maybe values}, returns a new one obtained by updating their fully specified @racket[just-value] result values (if they all have them) according to the given function. If any of the given yknow objects has a result that's a fully specified @racket[nothing?] value, then the resulting yknow object does as well. Otherwise, if any of them has a result that's not-yet-specified or user-specified, then the resulting yknow object has a not-yet-specified result. The function won't be called until the resulting yknow object's computation is fully forced.
  
  Note that this flattens the sometimes useful distinction between a yknow object with a not-yet-specified result and a yknow object with a user-specified result.
}

@defproc[
  (maybe-min-yknow-zip*-map/unambitious
    [y-list (listof (yknow/c maybe?))]
    [on-value (-> list? any/c)])
  (yknow/c maybe?)
]{
  Given a list of @tech{yknow objects} possibly resulting in @tech{maybe values}, returns a new one obtained by updating their fully specified @racket[just-value] result values (if they all have them) according to the given function. If any of the given yknow objects has a result that's not-yet-specified or user-specified, then the resulting yknow object has a not-yet-specified result. Otherwise, if any of them has a result that's a fully specified @racket[nothing?] value, then the resulting yknow object does as well. The function won't be called until the resulting yknow object's computation is fully forced.
  
  Note that this flattens the sometimes useful distinction between a yknow object with a not-yet-specified result and a yknow object with a user-specified result.
}

@defproc[
  (yknow-maybe-yknow-joininfo*
    [ymy-list (listof (yknow/c (maybe/c (yknow/c any/c))))])
  (yknow/c (maybe/c (yknow/c any/c)))
]{
  Given a list of @tech{yknow objects} containing possible @tech{maybe values} containing possible yknow objects, returns a new outer yknow object whose computation works by attempting each of the outer yknow objects' computations in turn until one of them has a not-yet-specified or fully-specified result, and then using that outcome as-is without proceeding to attempt the others. If it turns out all the given outer yknow objects' computations have user-specified results, then so does the resulting outer yknow object's computation. When the resulting outer yknow object has a fully specified result, and when that result is a @racket[just?] value, then there is a resulting inner yknow object. This inner yknow object's computation works by attempting the given inner yknow object's computation. If that one turns out to have a user-specified result, then the computation proceeds to attempt additional @emph{outer} yknow objects until another providing an inner yknow object is found, and it attempts to use that inner yknow object, and so on. If instead this process encounters an outer yknow object whose computation has a not-yet-specified result or a fully specified result value that isn't a @racket[just?] value, then an error is raised.
  
  This is a way of exercising one's prerogative as a user to replace a user-specified result with a fully specified one.
  
  This operation is idempotent and associative. If the outer yknow objects' fully specified results (if any) are all @racket[nothing?] values, or if they're are all @racket[just?] values containing inner yknow objects whose fully specified results (if any) are equivalent to each other, then this operation is also commutative.
}


@section[#:tag "yknow-predicate"]{Yknow Predicates}

@deftogether[(
  @defproc[(expressly-yknow-predicate-impl? [v any/c]) boolean?]
  @defthing[
    prop:expressly-yknow-predicate
    (struct-type-property/c expressly-yknow-predicate-impl?)
  ]
)]{
  Structure type property operations for procedures that can be invoked in a special way to obtain a @tech{yknow object} result. An instance of @racket[prop:expressly-yknow-predicate] should also be an instance of @racket[prop:expressly-knowable-predicate] and @racket[prop:procedure], likely using @racket[make-expressly-knowable-predicate-impl-for-yknow-predicate] and @racket[make-procedure-impl-for-knowable-predicate-with-arity-of-procedure] respectively. Furthermore, its ordinary procedure call result should be @racket[#f] and its knowable predicate result should be @racket[unknown?] when its yknow predicate result's @racket[yknow-value-knowable] is @racket[unknown?]; and otherwise, the ordinary procedure result should be equivalent to the @racket[known-value] result of the knowable predicate result and the @racket[yknow-value] of the yknow predicate result.
}

@defproc[
  (make-expressly-yknow-predicate-impl
    [get-accepts?-yknow (-> any/c (unconstrained-domain-> yknow?))])
  expressly-yknow-predicate-impl?
]{
  Given an implementation for @racket[get-accepts?-yknow], returns something a struct can use to implement the @racket[prop:expressly-yknow-predicate] interface.
}

@deftogether[(
  @defproc[
    ( (get-accepts?-yknow [f procedure?])
      [positional-arg any/c]
      ...
      [#:<kw> kw-arg any/c]
      ...)
    yknow?
  ]
  @defproc[
    (accepts?-yknow
      [f procedure?]
      [positional-arg any/c]
      ...
      [#:<kw> kw-arg any/c]
      ...)
    yknow?
  ]
)]{
  Given a procedure @racket[f], invokes it with the given positional and keyword arguments to obtain a @tech{yknow object} result. If @racket[f] is not a @racket[prop:expressly-yknow-predicate] instance, then the result is obtained by calling @racket[make-yknow-from-value-knowable] on the result of the @racket[accepts?-knowable] call.
}

@defproc[
  (make-expressly-knowable-predicate-impl-for-yknow-predicate)
  expressly-knowable-predicate-impl?
]{
  Returns a value that makes an appropriate @racket[prop:expressly-knowable-predicate] implementation for a structure type that implements @racket[prop:expressly-yknow-predicate].
}

@defproc[
  (makeshift-yknow-predicate
    [accepts?-yknow-impl (unconstrained-domain-> yknow?)])
  (unconstrained-domain-> (or/c #f any/c))
]{
  Returns an instance @racket[_f] of @racket[prop:procedure], @racket[prop:expressly-knowable-predicate], and @racket[prop:expressly-yknow-predicate] such that calling @racket[(get-accepts?-yknow _f)] returns @racket[accepts?-yknow-impl], calling @racket[(accepts?-knowable _f _positional-arg ... #:<kw> _kw-arg ...)] behaves just like calling @racket[(yknow-value-knowable (accepts?-yknow-impl _positional-arg ... #:<kw> _kw-arg ...))], and calling @racket[(_f _positional-arg ... #:<kw> _kw-arg ...)] behaves just like calling @racket[(yknow-value (accepts?-yknow-impl _positional-arg ... #:<kw> _kw-arg ...))].
}
