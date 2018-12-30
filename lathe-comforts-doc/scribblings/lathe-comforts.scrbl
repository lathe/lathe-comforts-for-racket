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


@(require #/for-label racket/base)
@(require #/for-label #/only-in racket/contract/base
  -> any any/c contract? listof or/c struct/c)
@(require #/for-label #/only-in racket/list append-map)
@(require #/for-label #/only-in racket/match
  exn:misc:match? match match-lambda)
@(require #/for-label #/only-in racket/math natural?)
@(require #/for-label #/only-in racket/struct
  make-constructor-style-printer)
@(require #/for-label #/only-in racket/struct-info
  extract-struct-info)
@(require #/for-label #/only-in syntax/parse expr id)
@(require #/for-label #/only-in syntax/parse/define
  define-simple-macro)

@(require #/for-label #/only-in parendown pd)

@(require #/for-label lathe-comforts)
@(require #/for-label lathe-comforts/contract)
@(require #/for-label lathe-comforts/trivial)
@(require #/for-label lathe-comforts/maybe)

@(require #/only-in scribble/example examples make-eval-factory)

@(define example-eval
  (make-eval-factory #/list
    'racket/base
    'racket/contract/base
    'racket/match
    'lathe-comforts
    'lathe-comforts/contract
    'parendown))


@title{Lathe Comforts}

Lathe Comforts for Racket is a collection of utilities that are handy for writing Racket code. This is a non-intrusive toolkit; in most cases it should only make certain Racket code easier to write, without substantially changing the architecture of the project it's used in.

Some of these utilities are designed with Parendown in mind. In some cases, Parendown's weak opening brackets make it easier to get by with higher-order functions instead of custom syntax. (Note that due to limitations of Scribble's Racket code formatter, we use Parendown's `pd` macro to achieve these weak parens, rather than using its custom reader syntax.)



@table-of-contents[]



@section[#:tag "evergreen"]{Evergreen Utilities for Binding Syntax and Pure FP}

@defmodule[lathe-comforts]


@subsection[#:tag "binding-syntax"]{Utilities for Binding Syntax}

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


@subsection[#:tag "fp"]{Utilities for Functional Programming}


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
    (pd _/ pass (list 1 2 3) _/ match-lambda
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
    (pd _/ hash-map (hash 'a 1 'b 2) _/ fn k v
      (format "(~s, ~s)" k v))
    (pd _/ build-list 5 _/ fn ~ _/ * 10 ~)
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
    
    (pd _/ w-loop next original (list 1 2 3) result (list)
      (expect original (cons first rest) result
      _/ next rest _/ cons (* first first) result))
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
    (pd _/ define (rev lst)
      (w-loop next lst lst result (list)
        
        ; If the list is empty, we're done.
        (mat lst (list) result
        
        ; Take apart the list, which must be a cons cell. If this
        ; doesn't work, raise an error.
        _/ expect lst (cons first rest)
          (error "Expected a list")
        
        ; Continue the loop, removing `first` from the input and
        ; adding it to the output.
        _/ next rest _/ cons first result)))
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



@section[#:tag "contract"]{Utilities for Contracts}

@defmodule[lathe-comforts/contract]

@defform[(fix/c id options ... contract-expr)]{
  A fixed-point syntax for contracts. Returns the result of running @racket[contract-expr] with a certain contract in scope as @racket[id]. The contract functionality of @racket[id] should be used only after @racket[contract-expr] has returned a contract, and it behaves just like that contract. This functionality is based on @racket[recursive-contract], and the @racket[options] given here supply the optional arguments of the @racket[recursive-contract] operation.
  
  @examples[
    #:eval (example-eval)
    (fix/c simple-s-expression/c
      (or/c symbol? (listof simple-s-expression/c)))
  ]
}



@section[#:tag "maybe"]{Maybe Values}

@defmodule[lathe-comforts/maybe]

Maybe values are a way to encode optional data. Using maybe values can simplify some interfaces that would otherwise use run time errors or special-cased sentinel values like @racket[#f].


@defstruct*[nothing ()]{
  A maybe value that does not contain an element.
  
  Every two @tt{nothing} values are @racket[equal?].
}

@defstruct*[just ([value any/c])]{
  A maybe value that contains an element.
  
  Two @tt{just} values are @racket[equal?] if they contain @racket[equal?] elements.
}

@defproc[(maybe? [x any/c]) boolean?]{
  Returns whether the given value is a maybe value. That is, it checks that the value is either a @racket[nothing?] value or a @racket[just?] value.
}

@defproc[(maybe/c [c contract?]) contract?]{
  Returns a contract that recognizes a maybe value where the element, if any, abides by the given contract.
}

@defproc[(maybe-bind [m maybe?] [func (-> any/c maybe?)]) maybe?]{
  Creates a maybe value by replacing the element of the given maybe value, if any, with zero or one elements according to the given function.
}

@defproc[(maybe-map [m maybe?] [func (-> any/c any/c)]) maybe?]{
  Creates a maybe value by replacing the element of the given maybe value, if any, with another according to the given function.
}



@section[#:tag "string"]{Utilities for Strings}

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



@section[#:tag "list"]{Utilities for Lists and Natural Numbers}

@defmodule[lathe-comforts/list]


@subsection[#:tag "list-nat"]{Utilities for Natural Numbers}

@defproc[(nat->maybe [n natural?]) (maybe/c natural?)]{
  Finds the predecessor of the given natural number, if any. If there is one, the result is a @racket[just?] of that value. If there isn't, the result is a @racket[nothing?].
}


@subsection[#:tag "list-list"]{Utilities for Lists}

@defproc[
  (list-foldl [state any/c] [lst list?] [func (-> any/c any/c any/c)])
  any/c
]{
  Returns the final state value obtained by initializing the state as the given value @racket[state] and updating the state by calling the given function @racket[func] with each element of the list @racket[lst] from first to last.
  
  The function @racket[func] is passed two values: The current state value and the list element to process. Its return value is used as the updated state value.
  
  Racket already provides a @racket[foldl] function, but this one arranges the function to be the last argument.
}

@defproc[
  (list-foldr [lst list?] [state any/c] [func (-> any/c any/c any/c)])
  any/c
]{
  Returns the final state value obtained by initializing the state as the given value @racket[state] and updating the state by calling the given function @racket[func] with each element of the list @racket[lst] from last to first.
  
  The function @racket[func] is passed two values: The list element to process and the current state value. Its return value is used as the updated state value.
  
  Racket already provides a @racket[foldr] function, but this one arranges the function to be the last argument, and it switches the order of the list and initial state arguments so the initial state is visually closer to the side of the list it interacts with.
}

@defproc[(list-bind [lst list?] [func (-> any/c list?)]) list?]{
  Creates a list by replacing every element of the given list with zero, one, or more elements according to the given function.
  
  The elements are processed from first to last.
  
  Racket already provides an @racket[append-map] function, but this one arranges the function to be the last argument.
}

@defproc[(list-map [lst list?] [func (-> any/c any/c)]) list?]{
  Creates a list by replacing every element of the given list with another according to the given function.
  
  The elements are processed from first to last.
  
  Racket already provides a @racket[map] function, but this one arranges the function to be the last argument.
}

@defproc[(list-any [lst list?] [func (-> any/c any)]) any]{
  Iterates over the given list's elements from first to last and calls the given function on each one, stopping early if the function returns a non-@racket[#f] value for any but the last element.
  
  If the list is empty, the overall result is @racket[#f]. Otherwise, if the function does return a non-@racket[#f] value for any but the last element, then that value is used as the overall result. Otherwise, the (possibly multiple) return values of calling the function with the last element are used as the result.
  
  Racket already provides an @racket[ormap] function, but this one arranges the function to be the last argument.
}

@defproc[(list-all [lst list?] [func (-> any/c any)]) any]{
  Iterates over the given list's elements from first to last and calls the given function on each one, stopping early if the function returns @racket[#f] for any but the last element.
  
  If the list is empty, the overall result is @racket[#t]. Otherwise, if the function does return @racket[#f] for any but the last element, then the overall result is @racket[#f]. Otherwise, the (possibly multiple) return values of calling the function with the last element are used as the result.
  
  Racket already provides an @racket[andmap] function, but this one arranges the function to be the last argument.
}

@defproc[(list-each [lst list?] [func (-> any/c any)]) void?]{
  Iterates over the given list's elements from first to last and calls the given procedure on each one. Ignores the procedure's results.
  
  Racket already provides a @racket[for-each] function, but this one arranges the function to be the last argument.
}

@defproc[
  (list-kv-map [lst list?] [func (-> natural? any/c any/c)])
  list?
]{
  Returns a list constructed by iterating over the given list's entries from first to last, calling the given function with each entry's index and value, and collecting the results.
}

@defproc[
  (list-kv-any [lst list?] [func (-> natural? any/c any)])
  any
]{
  Iterates over the given list's entries from first to last and calls the given function on each entry's index and value, stopping early if the function returns a non-@racket[#f] value for any but the last element.
  
  If the list is empty, the overall result is @racket[#f]. Otherwise, if the function does return a non-@racket[#f] value for any but the last entry, then that value is used as the overall result. Otherwise, the (possibly multiple) return values of calling the function with the last entry's index and value are used as the result.
}

@defproc[
  (list-kv-all [lst list?] [func (-> natural? any/c any)])
  any
]{
  Iterates over the given list's entries from first to last and calls the given function on each entry's index and value, stopping early if the function returns @racket[#f] for any but the last element.
  
  If the list is empty, the overall result is @racket[#t]. Otherwise, if the function does return @racket[#f] for any but the last entry, then the overall result is @racket[#f]. Otherwise, the (possibly multiple) return values of calling the function with the last entry's index and value are used as the result.
}

@defproc[
  (list-kv-each [lst list?] [func (-> natural? any/c any)])
  void?
]{
  Iterates over the given list's entries from first to last and calls the given procedure on each entry's index and value. Ignores the procedure's results.
}

@defproc[
  (list-zip-map [a list?] [b list?] [func (-> any/c any/c any/c)])
  list?
]{
  Creates a list by replacing every pair of elements of the given two lists (which must be of the same length) with a single element according to the given function.
  
  The elements are processed from first to last.
  
  Racket already provides a @racket[map] function, but this one arranges the function to be the last argument.
}

@defproc[
  (list-zip-any [a list?] [b list?] [func (-> any/c any/c any)])
  any
]{
  Iterates over the given two lists (which must be of the same length) and calls the given function on each pair of elements from first to last, stopping early if the function returns a non-@racket[#f] value for any but the last pair of elements.
  
  If the lists are empty, the overall result is @racket[#f]. Otherwise, if the function does return a non-@racket[#f] value for any but the last pair of elements, then that value is used as the overall result. Otherwise, the (possibly multiple) return values of calling the function with the last pair of elements are used as the result.
  
  Racket already provides an @racket[ormap] function, but this one arranges the function to be the last argument.
}

@defproc[
  (list-zip-all [a list?] [b list?] [func (-> any/c any/c any)])
  any
]{
  Iterates over the given two lists (which must be of the same length) and calls the given function on each pair of elements from first to last, stopping early if the function returns @racket[#f] for any but the last pair of elements.
  
  If the lists are empty, the overall result is @racket[#t]. Otherwise, if the function does return @racket[#f] for any but the last pair of elements, then the overall result is @racket[#f]. Otherwise, the (possibly multiple) return values of calling the function with the last pair of elements are used as the result.
  
  Racket already provides an @racket[andmap] function, but this one arranges the function to be the last argument.
}

@defproc[
  (list-zip-each [a list?] [b list?] [func (-> any/c any/c any)])
  void?
]{
  Iterates over the given two lists (which must be of the same length) and calls the given procedure on each pair of elements from first to last. Ignores the procedure's results.
  
  Racket already provides a @racket[for-each] function, but this one arranges the function to be the last argument.
}


@subsection[#:tag "list-together"]{Utilities for Natural Numbers and Lists Together}

@defproc*[(
  [(list-length<=nat? [lst list?] [n natural?]) boolean?]
  [(nat<=list-length? [n natural?] [lst list?]) boolean?]
  [(list-length=nat? [lst list?] [n natural?]) boolean?]
  [(list-length<nat? [lst list?] [n natural?]) boolean?]
  [(nat<list-length? [n natural?] [lst list?]) boolean?]
)]{
  These procedures compare a given natural number with the length of a given list. This can be more efficient than comparing the number to @racket[(length lst)] when that length is larger than the number.
}



@section[#:tag "hash"]{Utilities for Hash Tables}

@defmodule[lathe-comforts/hash]

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



@section[#:tag "struct"]{Utilities for Structs}

@defmodule[lathe-comforts/struct]

@defform[(struct-easy (name-id slot-id ...) rest ...)]{
  This is a convenience layer over @racket[struct], which has some features to automate the implementation of the struct's print behavior and @racket[equal?] behavior.
  
  The interface is rather unstable at this point. It's not even clear at this point which pieces of the behavior here are by design and which are temporary kludges.
  
  @; TODO: See if we can make it more stable.
  
  Here are some non-normative examples of how it can be used:
  
  The definition @racket[(@#,tt{struct-easy} (_my-data _field-one _field-two))] behaves the same way as @racket[(struct _my-data (_field-one _field-two))] except that it also implements a @racket[gen:custom-write] behavior based on @racket[make-constructor-style-printer].
  
  In the @racket[rest] section, the first occurrence of the @racket[#:other] keyword begins passing the rest of the subforms through to the @racket[struct] form. To illustrate, this means the definition @racket[(@#,tt{struct-easy} (_my-data _field-one _field-two) #:other ...)] behaves the same way as @racket[(struct _my-data (_field-one _field-two) ...)], aside from that custom write behavior.
  
  If @racket[#:equal] appears in the @racket[rest] section (before any occurrence of  @racket[#:other]), then an implementation of @racket[gen:equal+hash] will be generated so that the resulting structure type's instances are @racket[equal?] if their corresponding fields are @racket[equal?].
  
  If a list of the form @racket[(#:guard-easy _body-expr ...)] appears in the @racket[rest] section (before any occurrence of  @racket[#:other]), then the resulting structure type runs each @racket[_body-expr] when an instance is constructed. In those expressions, local variables corresponding to each field (e.g. @racket[_field-one] and @racket[_field-two]) are bound to the values that instance is being constructed with. The results of each @racket[_body-expr] are ignored; they're expected to raise exceptions if the field values are unacceptable.
  
  If @racket[#:write] followed by an expression appears, then whenever an instance of this structure type would have its @racket[gen:custom-write] behavior invoked, it runs that expression with local variables in scope corresponding to each field name (e.g. @racket[_field-one] and @racket[_field-two]), each bound to the instance's respective field value. The expression is expected to return a procedure that takes the instance value itself and returns a list of values to be printed. (Since these are redundant, typically the expression will either ignore the field variables or ignore the instance value.) The resulting list of values is printed according to the behavior of @racket[make-constructor-style-printer].
  
  If @racket[#:error-message-phrase] followed by an expression appears, then that expression will be evaluated as the structure type is defined, and its result will be used in certain error messages of the generated @racket[gen:custom-write] and @racket[gen:equal+hash] implementations, particularly the errors that occur when these methods are called with a value that isn't actually an instance of this structure type. The errors are of the form "Expected <this> to be <phrase>", and the default "<phrase>" is "an instance of the <name> structure type". For certain structure types, it may make sense to use @racket[#:error-message-phrase] to change this to a more pithy phrase, like "an RGB color" rather than "an instance of the rgb-color structure type." (However, is it even possible for clients of @racket[struct-easy] to observe these errors in their programs? This feature might not make any visible difference.)
  
  @; TODO: See if it does. That is to say, see if
  @; `#:error-message-phrase` makes any difference.
}

@defform[(struct-predicate struct-name-id)]{
  Expands to the predicate identifier associated with the given structure type name.
  
  For instance, if a struct is defined as @racket[(struct _my-data (_field-one _field-two))], then traditionally we can recognize instances of the struct using @racket[(_my-data? _x)], and now we can also recognize them using @racket[((@#,tt{struct-predicate} _my-data) _x)].
  
  This comes in handy mostly when defining other syntax transformers that deal with structure type names. Sometimes it allows those syntax transformers to be written using simple syntax templates, saving the trouble of making manual calls to @racket[syntax-local-value] and @racket[extract-struct-info].
}

@defform[(struct-accessor-by-name struct-name-id field-name-id)]{
  Expands to the struct field accessor identifier associated with the given structure type name and field name.
  
  For instance, if a struct is defined as @racket[(struct _my-data (_field-one _field-two))], then traditionally we can extract the first field using @racket[(_my-data-field-one _x)], and now we can also extract it using @racket[((@#,tt{struct-accessor-by-name} _my-data _field-one) _x)].
  
  This comes in handy mostly when defining other syntax transformers that deal with structure type names. Sometimes it allows those syntax transformers to be written using simple syntax templates, saving the trouble of making manual calls to @racket[syntax-local-value] and @racket[extract-struct-info].
}

@defform[(istruct/c name-id field/c-expr ...)]{
  Returns a contract that recognizes an instance of structure type @racket[name-id] where the fields abide by the respective @racket[field/c-expr] coontracts.
  
  Unlike @racket[struct/c], this works even when @racket[name-id] is an immutable struct and the @racket[field/c-expr] values contain one or more impersonator contracts.
  
  However, this comes at the price of some quirks. This operation works by reconstructing the struct altogether when a higher-order projection is taken. This means the projection of this struct isn't necessarily @racket[eq?], @racket[equal?], or @racket[impersonator-of?] to the original value. In fact, the projection becomes an instance of the structure type @racket[name-id], even when the original value is an instance of a distinct structure subtype of @racket[name-id].
}
