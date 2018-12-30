#lang parendown scribble/manual

@; lathe-comforts/scribblings/lathe-comforts.scrbl
@;
@; Evergreen utilities.

@;   Copyright 2017-2018 The Lathe Authors
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


@(require #/for-label
  ; NOTE: Scribble docs don't seem to distinguish between things like
  ; module variables and local variables when they hyperlink variable
  ; occurrences in example code. In our examples here, we use `/` as a
  ; local variable, namely as the weak open paren of a call to
  ; Parendown's `pd`, so we exclude it from our `racket/base` import
  ; here.
  (except-in racket/base /))
@(require #/for-label #/only-in racket/contract/base -> any any/c)
@(require #/for-label #/only-in racket/match
  exn:misc:match? match match-lambda)
@(require #/for-label #/only-in syntax/parse expr id)
@(require #/for-label #/only-in syntax/parse/define
  define-simple-macro)

@; TODO: Once Parendown has documentation, make the references to `pd`
@; in these docs become hyperlinks like so.
@;(require #/for-label #/only-in parendown pd)

@(require #/for-label lathe-comforts)
@(require #/for-label lathe-comforts/trivial)

@(require #/only-in scribble/example examples make-eval-factory)

@(define example-eval
  (make-eval-factory
  #/list 'racket/base 'racket/match 'lathe-comforts 'parendown))


@title{Lathe Comforts}

Lathe Comforts for Racket is a collection of utilities that are handy for writing Racket code. This is a non-intrusive toolkit; in most cases it should only make certain Racket code easier to write, without substantially changing the architecture of the project it's used in.

Some of these utilities are designed with Parendown in mind. In some cases, Parendown's weak opening brackets make it easier to get by with higher-order functions instead of custom syntax. (Note that due to limitations of Scribble's Racket code formatter, we use Parendown's `pd` macro to achieve these weak parens, rather than using its custom reader syntax.)



@table-of-contents[]



@section[#:tag "evergreen"]{Evergreen Utilities for Binding Syntax and Pure FP}

@defmodule[lathe-comforts]


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
      An even number of syntax objects alternating between identifiers and expressions, proceeding as far as possible. This uses the head pattern @racket[(~seq (~seq _var:id _val:expr) ... (~peek-not (~seq __:id __:expr)))], and the user writes something like @racket[(w- _a 1 _b 2 (+ _a _b))] when they use this format.
    }
  ]
  
  In all cases, this binds two attributes of ellipsis depth 1, namely @racket[_var] and @racket[_val], and they carry the same number of matches.
  
  (See @racket[_], @racket[expr], and @racket[id].)
}

@defform[
  (define-simple-normalizing-binder (id pattern ...)
    (template ...))
]{
  Defines a syntax transformer named @racket[id] which "normalizes" its binding syntax. Its input is a form that uses the rather permissive @racket[binds] splicing syntax class, and its output specifically uses the @racket[([_var _val] ...)] binding format expected by most Racket binding syntaxes.
  
  Specifically, the generated macro is equivalent to the following, where @racket[pattern ...] and @racket[template ...] are expanded right away, and the rest of the ellipses are part of the generated macro:
  
  @racketblock[
    (define-simple-macro (id pattern ... _vars:binds _body:expr ...)
      (template ... ([_vars.var _vars.val] ...)
        _body ...))
  ]
  
  (See @racket[expr].)
  
  As an example, @racket[w-] and @racket[w-loop] are defined straightforwardly in terms of @racket[let]:
  
  @racketblock[
    (define-simple-normalizing-binder (w-)
      (let))
    (define-simple-normalizing-binder (w-loop _proc:id)
      (let _proc))
  ]
}


@subsection[#:tag "fp"]{Functional programming utilities}


@subsubsection[#:tag "bindings-and-recursion"]{Bindings and recursion}

@defproc[(pass [arg any/c] [func (-> any/c any)]) any]{
  Invokes the given procedure with the given argument value. In other words, @racket[(pass arg func)] is just like @racket[(func arg)] but in a different order.
  
  This utility can come in handy when experimenting with a new operation that returns procedures---for example, @racket[match-lambda]. Instead of going to the trouble to define another operation that acts as a let binding---in this case, @racket[match]---it's easy enough to use @tt{pass} and a weak bracket to accomplish basically the same programming style:
  
  @examples[
    ; Since we just introduced this with a colon, we suppress the
    ; "Examples:" label.
    #:label #f
    #:eval (example-eval)
    (match (list 1 2 3)
      [(list) #f]
      [(cons first rest) rest])
    (pd / pass (list 1 2 3) / match-lambda
      [(list) #f]
      [(cons first rest) rest])
  ]
}

@defform[(w- binds body-expr ...)]{
  Works just like a @racket[let] form with no @racket[_proc-id], but uses the @racket[binds] splicing syntax class for the syntax of its bindings, so parentheses can usually be omitted.
  
  @examples[
    #:eval (example-eval)
    (w- ([a 1] [b 2])
      (+ a b))
    (w- [a 1 b 2]
      (+ a b))
    (w- a 1 b 2
      (+ a b))
  ]
}

@defform[(fn arg-id ... body-expr)]{
  Creates a procedure with positional arguments @racket[arg-id ...] and body @racket[body-expr].
  
  This is only a frequently useful shorthand, not a full replacement of @racket[lambda]. Unlike @racket[lambda], @tt{fn} can only be used to create functions of fixed arity, with no keyword arguments, and the body may only consist of one expression (although this expression may be a @racket[begin] form of course). Hence, programs that use @racket[fn] may still need to use @racket[lambda] on occasion.
  
  @examples[
    #:eval (example-eval)
    (pd / hash-map (hash 'a 1 'b 2) / fn k v
      (format "(~s, ~s)" k v))
    (pd / build-list 5 / fn ~ / * 10 ~)
  ]
}

@defform[(w-loop proc-id binds body ...)]{
  Works just like a @racket[let] form with a @racket[_proc-id], but uses the @racket[binds] splicing syntax class for the syntax of its bindings, so parentheses can usually be omitted.
  
  This example reverses and squares the numbers in a list, using the @racket[_next] procedure to continue the loop:
  
  @examples[
    ; Since we just said "this example," we suppress the "Example:"
    ; label.
    #:label #f
    #:eval (example-eval)
    
    (pd / w-loop next original (list 1 2 3) result (list)
      (expect original (cons first rest) result
      / next rest / cons (* first first) result))
  ]
}

@defform[(loopfn proc-id arg-id ... body-expr)]{
  Creates a procedure with positional arguments @racket[arg-id ...] and body @racket[body-expr], which can refer to itself in the body using the name @racket[proc-id].
}


@subsubsection[#:tag "conditionals"]{Conditionals}

@deftogether[(
  @defform[(mat val-expr pat then-expr else-expr)]
  @defform[(expect val-expr pat else-expr then-expr)]
)]{
  Checks whether @racket[pat] matches the result of @racket[val-expr]. If it does, this evaluates @racket[then-expr] in tail position with the bindings introduced by @racket[pat]. Otherwise, this evaluates @racket[else-expr] without any new bindings.
  
  The only difference between @tt{mat} and @tt{expect} is the order of @racket[then-expr] and @racket[else-expr] in the form. When these are used with Parendown's weak opening brackets, they enable a programming style where run time error checking and other early exit conditions are kept toward the top of a procedure body, without affecting the indentation of the procedure's main logic.
  
  @examples[
    #:eval (example-eval)
    (pd / define (rev lst)
      (w-loop next lst lst result (list)
        
        ; If the list is empty, we're done.
        (mat lst (list) result
        
        ; Take apart the list, which must be a cons cell. If this
        ; doesn't work, raise an error.
        / expect lst (cons first rest)
          (error "Expected a list")
        
        ; Continue the loop, removing `first` from the input and
        ; adding it to the output.
        / next rest / cons first result)))
    (rev (list 1 2 3))
    (eval:error (rev 3))
  ]
}

@defform[
  (matfns pat then-expr elsefn-expr)
  #:contracts ([elsefn-expr (-> any/c any)])
]{
  Returns a procedure. The procedure takes a single argument value and checks whether it matches the @racket[match] pattern @racket[pat]. If it does, the procedure evaluates @racket[then-expr] in tail position with the bindings introduced by @racket[pat]. Otherwise, the procedure makes a tail call to the procedure resulting from @racket[elsefn-expr], passing in the same argument value.
}

@defform[(expectfn pat else-expr then-expr)]{
  Returns a procedure. The procedure takes a single argument value and checks whether it matches the @racket[match] pattern @racket[pat]. If it does, the procedure evaluates @racket[then-expr] in tail position with the bindings introduced by @racket[pat]. Otherwise, the procedure evaluates @racket[else-expr] in tail position without any new bindings.
}

@defform[(dissect val-expr pat then-expr)]{
  Checks whether @racket[pat] matches the result of @racket[val-expr]. If it does, this evaluates @racket[then-expr] in tail position with the bindings introduced by @racket[pat]. Otherwise, the @racket[exn:misc:match?] exception is raised.
  
  If you need a custom error message, use @racket[expect] with an expression that raises an exeption.
}

@defform[(dissectfn pat then-expr)]{
  Returns a procedure. The procedure takes a single argument value and checks whether it matches the @racket[match] pattern @racket[pat]. If it does, the procedure evaluates @racket[then-expr] in tail position with the bindings introduced by @racket[pat]. Otherwise, the @racket[exn:misc:match?] exception is raised.
  
  If you need a custom error message, use @racket[expectfn] with an expression that raises an exeption.
}



@section[#:tag "maybe"]{Maybe Values}

@defmodule[lathe-comforts/maybe]

Maybe values are a way to encode optional data. Using maybe values can simplify some interfaces that would otherwise use run time errors or special-cased sentinel values like @racket[#f].


@defstruct*[nothing ()]{
  A maybe value that does not contain a value.
  
  Every two @tt{nothing} values are @racket[equal?].
}

@defstruct*[just ([value any/c])]{
  A maybe value that does contain a value.
  
  Two @tt{just} values are @racket[equal?] if they contain @racket[equal?] values.
}

@defproc[(maybe? [x any/c]) boolean?]{
  Returns whether the given value is a maybe value. That is, it checks that the value is either a @racket[nothing] value or a @racket[just] value.
}

@defproc[(maybe/c [c chaperone-contract?]) chaperone-contract?]{
  Returns a chaperone contract that recognizes a maybe value where the contained value, if any, abides by the given chaperone contract.
}

@; TODO: Document `maybe-map`.



@section[#:tag "string"]{Strings}

@defmodule[lathe-comforts/string]


@defproc[(immutable-string? [v any/c]) boolean?]{
  Returns whether the given value is an immutable string.
  
  Equivalent to @racket[(and (string? v) (immutable? v))].
}



@section[#:tag "trivial"]{Trivial Values}

@defmodule[lathe-comforts/trivial]

Some values never really vary at all. Perhaps some library accepts an argument that it'll pass through, but the library's client has no need for its pass-through services this time. Perhaps some data structure can store annotations on certain nodes, but the client doesn't really care to annotate any of the nodes this time. In cases like these, it's useful to have a particular value that doesn't mean anything.

Racket programs sometimes use @racket[(void)] for this purpose, but that value is more commonly used as the return value of side-effecting operations which will never have a meaningful result to print at the top level. If a user exploring at the top level uses an operation that typically returns a pass-through value or label, but in this case it happens to return a trivial pass-through value or a trivial label, that's potentially interesting information for the user, since they may not have even known they were dealing with trivial data yet.

So Lathe Comforts provides a very simple structure type, @racket[trivial], to represent trivial values.


@defstruct*[trivial ()]{
  A trivial value.
  
  Every two @tt{trivial} values are @racket[equal?].
}



@section[#:tag "hash"]{Hash Tables}

@defmodule[lathe-comforts/hash]

Various utilities for Racket's hash tables.


@defproc[
  (make-similar-hash [example hash?] [assocs (listof pair?)])
  hash?
]{
  Returns a hash table with the same key-comparison procedure, key-holding strength, and mutability as @racket[example] but with entries populated from @racket[assocs] instead. If @racket[assocs] contains duplicate keys, the last entry in the list takes precedence.
}

@defproc[(hash-comparison-same? [a hash?] [b hash?]) boolean?]{
  Returns whether the two given hash tables have the same comparison procedure (@racket[equal?], @racket[eqv?], or @racket[eq?]).
}

@defproc[(hash-keys-same? [a hash?] [b hash?]) boolean?]{
  Returns whether the two given hash tables have the same set of keys according to their comparison procedure. If the two hash tables don't even have the same comparison procedure, the @racket[exn:fail:contract] exception is raised.
  
  If the thread performing this operation is terminated and the given hash tables use @racket[eqv?] or @racket[equal?] as their comparison procedure, all current and future operations on those hash tables may block indefinitely.
  
  If either of the given hash tables is modified partway through this operation, or if either one is an @racket[equal?]-based hash table whose keys have been mutated after insertion, the resulting behavior may be unpredictable.
}

@defproc[(hash-ref-maybe [hash hash?] [key any/c]) maybe?]{
  Looks up the given key in the given hash table. Returns a @racket[just?] of the value found or a @racket[nothing?] if no entry for that key exists.
  
  If the thread performing this operation is terminated and the given hash table uses @racket[eqv?] or @racket[equal?] as its comparison procedure, all current and future operations on that hash table may block indefinitely.
  
  If the given hash table is modified partway through this operation, or if it's an @racket[equal?]-based hash table whose keys have been mutated after insertion, the resulting behavior may be unpredictable.
}

@defproc[
  (hash-set-maybe [hash hash?] [key any/c] [maybe-value maybe?])
  hash?
]{
  Returns a hash table with the same key-comparison procedure, key-holding strength, and mutability as the given one, where the given key's mapping has been deleted (if @racket[maybe-value] is a @racket[nothing?]) or replaced (if it's a @racket[just?] of some value to use as the replacement).
  
  If the given hash table is an @racket[equal?]-based hash table whose keys have been mutated after insertion, the resulting behavior may be unpredictable.
  
  If the result is an @racket[equal?]-based hash table and one of its keys is mutated after insertion, it may have unpredictable behavior with other hash-table-related utilities.
}

@defproc[
  (hash-kv-map-sorted
    [key<? (-> any/c any/c boolean?)]
    [hash hash?]
    [func (-> any/c any/c any/c)])
  list?
]{
  Returns a list constructed by converting the given hash table to a list, sorting it in ascending order by comparing keys according to the given @racket[key<?] behavior, and then transforming each entry in ascending order by calling the given @racket[func] with the entry's key and value.
  
  The number of times @racket[key<?] is called is unspecified. The particular argument values it receives are unspecified, aside from the fact that they are all keys of @racket[hash].
  
  If the given hash table is modified while this operation is setting up (before it calls either of the given functions for the first time), the particular list of entries it processes may be unpredictable. Modifications after that will not affect the operation.
}

@defproc[
  (hash-kv-bind [hash hash?] [func (-> any/c any/c hash?)])
  hash?
]{
  Returns a hash table constructed by iterating over the given hash table @racket[hash]'s entries in an unspecified order, calling the given function @racket[func] with each entry's key and value, and collecting the entries of the resulting hash tables into a single result hash table.
  
  If multiple results of @racket[func] have the same key, the last one to be computed takes precedence.
  
  The resulting hash table has the same key-comparison procedure, key-holding strength, and mutability as @racket[hash]. Each result of @racket[func] may have its own combination of key-comparison procedure, key-holding strength, and mutability; these properties will be ignored.
  
  If @racket[hash] is modified while this operation is setting up (before it calls the given function for the first time), the particular list of entries this operation processes may be unpredictable. Modifications after that will not affect the operation.
  
  Likewise, if a result of @racket[func] is modified between the time it is returned and the time this operation either returns or calls @racket[func] again, then the particular list of entries this operation collects from that @racket[func] result may be unpredictable.
  
  If the overall result is an @racket[equal?]-based hash table and one of its keys is mutated after insertion, it may have unpredictable behavior with other hash-table-related utilities.
}

@defproc[
  (hash-kv-map-maybe [hash hash?] [func (-> any/c any/c maybe?)])
  hash?
]{
  Returns a hash table constructed by iterating over the given hash table's entries in an unspecified order, calling the given function with each entry's key and value, and collecting the results. If for some input entry, the function returns a @racket[nothing?], then there is no corresponding output entry. When the function returns a @racket[just?], then there is a corresponding output entry which maps the input entry's key to the the value of the @racket[just?].
  
  The resulting hash table has the same key-comparison procedure, key-holding strength, and mutability as the given one.
  
  If the given hash table is modified while this operation is setting up (before it calls the given function for the first time), the particular list of entries it processes may be unpredictable. Modifications after that will not affect the operation.
  
  If the result is an @racket[equal?]-based hash table and one of its keys is mutated after insertion, it may have unpredictable behavior with other hash-table-related utilities.
}

@defproc[
  (hash-kv-map [hash hash?] [func (-> any/c any/c any/c)])
  hash?
]{
  Returns a hash table with the same keys as the given one. The result is constructed by iterating over the given hash table's entries in an unspecified order and calling the given function with each entry's key and value to determine the corresponding result entry's mapped value.
  
  The resulting hash table has the same key-comparison procedure, key-holding strength, and mutability as the given one.
  
  If the given hash table is modified while this operation is setting up (before it calls the given function for the first time), the particular list of entries it processes may be unpredictable. Modifications after that will not affect the operation.
  
  If the result is an @racket[equal?]-based hash table and one of its keys is mutated after insertion, it may have unpredictable behavior with other hash-table-related utilities.
}

@defproc[
  (hash-kv-any [hash hash?] [func (-> any/c any/c boolean?)])
  boolean?
]{
  Iterates over the given hash table's entries in an unspecified order and calls the given function on each entry's key and value, stopping early if the function returns @racket[#t]. If the function does return @racket[#t], then the overall result is @racket[#t]; otherwise, it's @racket[#f].
  
  If the given hash table is modified partway through this operation, the resulting behavior may be unpredictable.
}

@defproc[
  (hash-kv-all [hash hash?] [func (-> any/c any/c boolean?)])
  boolean?
]{
  Iterates over the given hash table's entries in an unspecified order and calls the given function on each entry's key and value, stopping early if the function returns @racket[#f]. If the function does return @racket[#f], then the overall result is @racket[#f]; otherwise, it's @racket[#t].
  
  If the given hash table is modified partway through this operation, the resulting behavior may be unpredictable.
}

@defproc[
  (hash-kv-each [hash hash?] [body (-> any/c any/c any)])
  void?
]{
  Iterates over the given hash table's entries in an unspecified order and calls the given procedure on each entry's key and value. Ignores the procedure's results.
  
  If the given hash table is modified partway through this operation, the resulting behavior may be unpredictable.
}

@defproc[
  (hash-v-map-maybe [hash hash?] [func (-> any/c maybe?)])
  hash?
]{
  Returns a hash table constructed by iterating over the given hash table's entries in an unspecified order, calling the given function with each entry's mapped value, and collecting the results. If for some input entry, the function returns a @racket[nothing?], then there is no corresponding output entry. When the function returns a @racket[just?], then there is a corresponding output entry which maps the input entry's key to the the value of the @racket[just?].
  
  The resulting hash table has the same key-comparison procedure, key-holding strength, and mutability as the given one.
  
  If the given hash table is modified while this operation is setting up (before it calls the given function for the first time), the particular list of entries it processes may be unpredictable. Modifications after that will not affect the operation.
  
  If the result is an @racket[equal?]-based hash table and one of its keys is mutated after insertion, it may have unpredictable behavior with other hash-table-related utilities.
}

@defproc[(hash-v-map [hash hash?] [func (-> any/c any/c)]) hash?]{
  Returns a hash table with the same keys as the given one. The result is constructed by iterating over the given hash table's entries in an unspecified order and calling the given function with each entry's mapped value to determine the corresponding result entry's mapped value.
  
  The resulting hash table has the same key-comparison procedure, key-holding strength, and mutability as the given one.
  
  If the given hash table is modified while this operation is setting up (before it calls the given function for the first time), the particular list of entries it processes may be unpredictable. Modifications after that will not affect the operation.
}

@defproc[(hash-v-any [hash hash?] [func (-> any/c boolean?)]) boolean?]{
  Iterates over the given hash table's mapped values in an unspecified order and calls the given function on each one, stopping early if the function returns @racket[#t]. If the function does return @racket[#t], then the overall result is @racket[#t]; otherwise, it's @racket[#f].
  
  If the given hash table is modified partway through this operation, the resulting behavior may be unpredictable.
}

@defproc[(hash-v-all [hash hash?] [func (-> any/c boolean?)]) boolean?]{
  Iterates over the given hash table's mapped values in an unspecified order and calls the given function on each one, stopping early if the function returns @racket[#f]. If the function does return @racket[#f], then the overall result is @racket[#f]; otherwise, it's @racket[#t].
  
  If the given hash table is modified partway through this operation, the resulting behavior may be unpredictable.
}

@defproc[(hash-v-each [hash hash?] [func (-> any/c any)]) void?]{
  Iterates over the given hash table's mapped values in an unspecified order and calls the given procedure on each one. Ignores the procedure's results.
  
  If the given hash table is modified partway through this operation, the resulting behavior may be unpredictable.
}
