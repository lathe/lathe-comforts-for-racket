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


@section[#:tag "knowable-proper"]{Knowable Values Themselves}

@deftogether[(
  @defproc[(expressly-possibly-unknown-impl? [v any/c]) boolean?]
  @defthing[
    prop:expressly-possibly-unknown
    (struct-type-property/c expressly-possibly-unknown-impl?)
  ]
)]{
  Structure type property operations for @tech{knowable values} that represent anything short of a complete presence of information.
  
  Since not many operations necessarily preserve distinctions between @racket[unknown?] values, the main reason to define a new type abiding by this interface is for the hope of providing more helpful debug printing behavior.
}

@defproc[
  (make-expressly-possibly-unknown-impl
    [is-indeed? (-> any/c boolean?)])
  expressly-possibly-unknown-impl?
]{
  Given an implementation for @racket[unknown?], returns something a struct can use to implement the @racket[prop:expressly-possibly-unknown] interface.
}

@defproc[(unknown? [v any/c]) boolean?]{
  Returns whether the given value is a @tech{knowable value} and not (yet) one that represents a complete presence of information.
  
  It should be rare to need to check for this, since an @racket[unknown?] value may be replaced with a @racket[known?] one over time as libraries are upgraded. A program that processes a @tech{knowable value} and checks for this condition can be well-behaved if its @racket[known?] path provides strictly more information than its @tt{unknown?} path does.
}

@deftogether[(
  @defidform[example-unknown]
  @defform[#:link-target? #f (example-unknown)]
  @defform[#:kind "match expander" #:link-target? #f (example-unknown)]
  @defproc[(example-unknown? [v any/c]) boolean?]
)]{
  Struct-like operations which construct and deconstruct a @tech{knowable value} that represents a complete absence of information.
  
  It's unspecified whether two @tt{example-unknown} values are @racket[equal?].
  
  It should be rare to need to use @tt{example-unknown} as a @racket[match] expander or @tt{example-unknown?} as a predicate. If either of these obtains a successful result for a given value, then that result may be subject to change in the future as that value is replaced with a different representation of an @racket[unknown?] @tech{knowable value} or replaced with a @racket[known?] one. If the program returns results (or raises errors) that communicate this subject-to-change nature, then it can be well-behaved.
}

@defproc[(unknown) example-unknown?]{
  Returns a @tech{knowable value} that represents a complete absence of information. Specifically, this is equivalent to @racket[example-unknown], except that it isn't a @racket[match] expander.
}

@deftogether[(
  @defidform[known]
  @defform[#:link-target? #f (known value-expr)]
  @defform[#:kind "match expander" #:link-target? #f (known value-pat)]
  @defproc[(known? [v any/c]) boolean?]
  @defproc[(known-value [inst known?]) any/c]
)]{
  Struct-like operations which construct and deconstruct a @tech{knowable value} that represents the complete presence of information.
  
  Two @tt{known} values are @racket[equal?] if they contain @racket[equal?] elements.
}

@defproc[(knowable? [v any/c]) boolean?]{
  Returns whether the given value is a @tech{knowable value}.
}

@defproc[(knowable/c [c contract/c]) contract?]{
  Returns a contract that recognizes a @tech{knowable value} if it's @racket[unknown?] or if its @racket[known-value] is recognized by the given contract.
}

@defproc[
  (knowable-bind [kble knowable?] [func (-> any/c knowable?)])
  knowable?
]{
  Returns the given @tech{knowable value} if it's @racket[unknown?], and returns the result of calling @racket[func] on its @racket[known-value] if it's @racket[known?].
}

@defproc[
  (knowable-map [kble knowable?] [func (-> any/c any/c)])
  knowable?
]{
  Returns the given @tech{knowable value} if it's @racket[unknown?], and uses @racket[func] to update its @racket[known-value] if it's @racket[known?].
}

@defproc[
  (knowable-if [condition boolean?] [get-value (-> any/c)])
  knowable?
]{
  Creates a @tech{knowable value} that's @racket[known?] if and only if the given @racket[condition] is not @racket[#f]. The element is computed by calling the given function @racket[get-value] with no arguments.
}

@defproc[(knowable->falsable [kble knowable?]) (or/c #f any/c)]{
  Converts the given value from a @tech{knowable value} that may be @racket[unknown?] to an arbitrary value that may be @racket[#f]. The inputs @racket[(known #f)] and @racket[(unknown)] both result in @racket[#f], so some information may be lost this way.
  
  Note that if an upgrade of libraries changes the input from @racket[unknown?] to @racket[known?], then the result of this function may change from @racket[#f] to something else.
}

@defproc[(falsable->uninformative-knowable [fble (or/c #f any/c)]) knowable?]{
  Converts the given value from an arbitrary value that may be @racket[#f] to a @tech{knowable value} that may be @racket[unknown?]. There is no way for this to produce the result @racket[(known #f)], so this may not always be able to restore a @racket[knowable->falsable] result to its original form.
}


@section[#:tag "knowable-predicate"]{Knowable Predicates}

@deftogether[(
  @defproc[(expressly-knowable-predicate-impl? [v any/c]) boolean?]
  @defthing[
    prop:expressly-knowable-predicate
    (struct-type-property/c expressly-knowable-predicate-impl?)
  ]
)]{
  Structure type property operations for @tech{knowable values} that represent anything short of a complete presence of information.
  
  Since not many operations necessarily preserve distinctions between @racket[unknown?] values, the main reason to define a new type abiding by this interface is for the hope of providing more helpful debug printing behavior.
}

@defproc[
  (make-expressly-knowable-predicate-impl
    [get-accepts?-Knowable
      (-> any/c (unconstrained-domain-> knowable?))])
  expressly-knowable-predicate-impl?
]{
  Given a curried implementation for @racket[call-knowable], returns something a struct can use to implement the @racket[prop:expressly-knowable-predicate] interface.
}

@defproc[
  (call-knowable
    [f any/c]
    [positional-arg any/c]
    ...
    [#:<kw> kw-arg any/c]
    ...)
  knowable?
]{
  Given a value @racket[f] that must be a @racket[prop:expressly-knowable-predicate] instance or a @racket[procedure?], invokes it with the given positional and keyword arguments. If @racket[f] is not a @racket[prop:expressly-knowable-predicate] instance, then the result is obtained by calling @racket[falsable->uninformative-knowable] on the result of the @racket[procedure?] call.
}

@defproc[
  (make-procedure-impl-for-knowable-predicate)
  (unconstrained-domain-> any/c)
]{
  Returns a value that makes an appropriate @racket[prop:procedure] implementation for a structure type that implements @racket[prop:expressly-knowable-predicate]. Calling the procedure will have the same result as using @racket[call-knowable] followed by @racket[knowable->falsable].
  
  It will often be preferable to pass the result of @tt{make-procedure-impl-for-knowable-predicate} through @racket[procedure-reduce-arity]. Otherwise, the @racket[prop:procedure] instance's arity will be completely unconstrained. Alternatively, use @racket[make-procedure-impl-for-knowable-predicate-with-arity-of-procedure].
}

@defproc[
  (make-procedure-impl-for-knowable-predicate-with-arity-of-procedure
    [p procedure?])
  (unconstrained-domain-> any/c)
]{
  Returns a value that makes an appropriate @racket[prop:procedure] implementation for a structure type that implements @racket[prop:expressly-knowable-predicate]. Calling the procedure will have the same result as using @racket[call-knowable] followed by @racket[knowable->falsable].
  
  The @racket[prop:procedure] instance's arity will be the same as the arity of the given procedure @racket[p].
}

@defproc[
  (makeshift-knowable-predicate
    [accepts?-knowable (unconstrained-domain-> knowable?)])
  (unconstrained-domain-> any/c)
]{
  Returns an instance @racket[_f] of @racket[prop:procedure] and @racket[prop:expressly-knowable-predicate] such that calling @racket[(call-knowable _f _positional-arg ... #:<kw> _kw-arg ...)] behaves just like calling @racket[(accepts?-knowable _positional-arg ... #:<kw> _kw-arg ...)], while calling @racket[(_f _positional-arg ... #:<kw> _kw-arg ...)] behaves just like calling @racket[(knowable->falsable (accepts?-knowable _positional-arg ... #:<kw> _kw-arg ...))].
}
