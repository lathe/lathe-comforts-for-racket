#lang parendown scribble/manual

@; lathe-comforts/scribblings/lathe-comforts.scrbl
@;
@; Evergreen utilities.

@;   Copyright 2017-2022, 2025 The Lathe Authors
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

Some of these utilities are designed with Parendown in mind. In some cases, Parendown's weak opening brackets make it easier to get by with higher-order functions instead of custom syntax. (Note that due to limitations of Scribble's Racket code formatter, we use Parendown's @racket[pd] macro to achieve these weak parens, rather than using its custom reader syntax.)



@table-of-contents[]



@section[#:tag "evergreen"]{Evergreen Utilities for Binding Syntax and Pure FP}

@defmodule[lathe-comforts]


@subsection[#:tag "syntax-definition"]{Utilities for Macro Definition}

@(define @enforces-autopticity[]
  @list{
    This syntax must be called with @tech{autopticity}. An occurrence of a cons cell that's part of the call syntax must have a set of scopes that's equal to or a superset of the set of scopes on the entire call, as though the call has created a local binding of what a cons cell means in these positions. This helps ensure that even though Racket expressions are often made of cons cells, an expression inserted into one of these positions by a macro's syntax template will not have its cons cells misinterpreted.
  })

@(define @also-enforces-autopticity[]
  @list{
    This syntax itself must also be called with @tech{autopticity}. An occurrence of a cons cell that's part of the call syntax must have a set of scopes that's equal to or a superset of the set of scopes on the entire call, as though the call has created a local binding of what a cons cell means in these positions. This helps ensure that even though Racket expressions are often made of cons cells, an expression inserted into one of these positions by a macro's syntax template will not have its cons cells misinterpreted.
  })

@(define @definition-that-ensures-autopticity[]
  @list{
    Defining a syntax transformer this way helps to ensure @tech{autopticity} of each of its call sites. Note that each of the @racket[_pattern]s is still the usual sort of @racket[syntax-parse] pattern, and it won't ensure autopticity without the use of helpers like the @racket[~autoptic-list] pattern expander.
    
    @also-enforces-autopticity[]
  })

@defform[
  (define-syntax-parse-rule/autoptic (id pattern ...)
    pattern-directive ...
    template)
]{
  Defines a syntax transformer named @racket[id] similarly to @racket[define-syntax-parse-rule], except that it first verifies that that the literal list @racket[(id pattern ...)] at the call site has scopes that are a subset of the scopes on that list's first cons cell.
  
  @definition-that-ensures-autopticity[]
}

@defform[
  (define-syntax-parse-rule/autoptic/loc (id pattern ...)
    pattern-directive ...
    template)
]{
  Defines a syntax transformer named @racket[id] similarly to @racket[define-syntax-parse-rule], except that it first verifies that that all the tails of the literal list @racket[(id pattern ...)] at the call site have scopes that are a subset of the scopes on that list's first cons cell, and it restores the list's source location on the transformed code as though using @racket[syntax/loc].
  
  @definition-that-ensures-autopticity[]
}


@subsection[#:tag "binding-syntax"]{Utilities for Binding Syntax}

@defproc[
  #:kind "splicing syntax class"
  (autoptic-binds-to [surrounding-stx syntax?])
  @#,tech[#:doc syntax-doc]{splicing syntax class}
]{
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
    (define-syntax-parse-rule/autoptic/loc
      (id pattern ... _vars _body:expr ...)
      #:declare _vars (autoptic-binds-to this-syntax)
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
  
  @definition-that-ensures-autopticity[]
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

@defform[(w- local-binds body-expr ...)]{
  Works just like a @racket[let] form with no @racket[_proc-id], but uses the @racket[autoptic-binds-to] splicing syntax class for the syntax of its bindings, so parentheses can usually be omitted.
  
  @enforces-autopticity[]
  
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
  
  @enforces-autopticity[]
  
  This is only a frequently useful shorthand, not a full replacement of @racket[lambda]. Unlike @racket[lambda], @tt{fn} can only be used to create functions of fixed arity, with no keyword arguments, and the body may only consist of one expression (although this expression may be a @racket[begin] form of course). Hence, programs that use @racket[fn] may still need to use @racket[lambda] on occasion.
  
  @examples[
    #:eval (example-eval)
    (pd _/ hash-map (hash 'a 1 'b 2) _/ fn k v
      (format "(~s, ~s)" k v))
    (pd _/ build-list 5 _/ fn ~ _/ * 10 ~)
  ]
}

@defform[(w-loop proc-id local-binds body ...)]{
  Works just like a @racket[let] form with a @racket[_proc-id], but uses the @racket[autoptic-binds-to] splicing syntax class for the syntax of its bindings, so parentheses can usually be omitted.
  
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
  
  @enforces-autopticity[]
}

@defform[(loopfn proc-id arg-id ... body-expr)]{
  Creates a procedure with positional arguments @racket[arg-id ...] and body @racket[body-expr], which can refer to itself in the body using the name @racket[proc-id].
  
  @enforces-autopticity[]
}


@subsubsection[#:tag "conditionals"]{Conditionals}

@deftogether[(
  @defform[(mat val-expr pat then-expr else-expr)]
  @defform[(expect val-expr pat else-expr then-expr)]
)]{
  Checks whether @racket[pat] matches the result of @racket[val-expr]. If it does, this evaluates @racket[then-expr] in tail position with the bindings introduced by @racket[pat]. Otherwise, this evaluates @racket[else-expr] without any new bindings.
  
  @enforces-autopticity[]
  
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
  
  @enforces-autopticity[]
}

@defform[(expectfn pat else-expr then-expr)]{
  Returns a procedure. The procedure takes a single argument value and checks whether it matches the @racket[match] pattern @racket[pat]. If it does, the procedure evaluates @racket[then-expr] in tail position with the bindings introduced by @racket[pat]. Otherwise, the procedure evaluates @racket[else-expr] in tail position without any new bindings.
  
  @enforces-autopticity[]
}

@deftogether[(
  @defform[(dissect val-expr pat then-expr)]
  @defform[(dissect/derived orig val-expr pat then-expr)]
)]{
  Checks whether @racket[pat] matches the result of @racket[val-expr]. If it does, this evaluates @racket[then-expr] in tail position with the bindings introduced by @racket[pat]. Otherwise, the @racket[exn:misc:match?] exception is raised.
  
  @enforces-autopticity[]
  
  The @tt{dissect/derived} variant reports errors in terms of @racket[orig].
  
  If you need a custom error message, use @racket[expect] with an expression that raises an exeption.
}

@deftogether[(
  @defform[(dissectfn pat then-expr)]
  @defform[(dissectfn/derived orig pat then-expr)]
)]{
  Returns a procedure. The procedure takes a single argument value and checks whether it matches the @racket[match] pattern @racket[pat]. If it does, the procedure evaluates @racket[then-expr] in tail position with the bindings introduced by @racket[pat]. Otherwise, the @racket[exn:misc:match?] exception is raised.
  
  @enforces-autopticity[]
  
  The @tt{dissectfn/derived} variant reports errors in terms of @racket[orig].
  
  If you need a custom error message, use @racket[expectfn] with an expression that raises an exeption.
}



@section[#:tag "contract"]{Utilities for Contracts}

@defmodule[lathe-comforts/contract]

@defproc[(obstinacy? [v any/c]) boolean?]{
  Returns whether the given value is a @deftech{contract obstinacy}, which is an enumeration that distinguishes between impersonator contracts, chaperone contracts, and flat contracts.
  
  Contract obstinacy can be thought of as a scale with two ends. Impersonator contracts are the most @deftech{obstinate} since they can go to any length to impose behavior on the values that pass through, while flat contracts are the most @deftech{reticent} since they don't pass any more judgment on a value after they've ferried it through.
  
  By default, Racket contracts are usually as obstinate as can be; @racket[contract?] refers generally to impersonator contracts. Only certain contracts exercise reticence.
}

@deftogether[(
  @defidform[impersonator-obstinacy]
  @defform[#:link-target? #f (impersonator-obstinacy)]
  @defform[
    #:kind "match expander"
    #:link-target? #f
    (impersonator-obstinacy)
  ]
  @defproc[(impersonator-obstinacy? [v any/c]) boolean?]
)]{
  Struct-like operations which construct and deconstruct a @tech{contract obstinacy} that represents impersonator contracts.
  
  Every two @tt{impersonator-obstinacy} values are @racket[equal?].
}

@deftogether[(
  @defidform[chaperone-obstinacy]
  @defform[#:link-target? #f (chaperone-obstinacy)]
  @defform[
    #:kind "match expander"
    #:link-target? #f
    (chaperone-obstinacy)
  ]
  @defproc[(chaperone-obstinacy? [v any/c]) boolean?]
)]{
  Struct-like operations which construct and deconstruct a @tech{contract obstinacy} that represents chaperone contracts.
  
  Every two @tt{chaperone-obstinacy} values are @racket[equal?].
}

@deftogether[(
  @defidform[flat-obstinacy]
  @defform[#:link-target? #f (flat-obstinacy)]
  @defform[
    #:kind "match expander"
    #:link-target? #f
    (flat-obstinacy)
  ]
  @defproc[(flat-obstinacy? [v any/c]) boolean?]
)]{
  Struct-like operations which construct and deconstruct a @tech{contract obstinacy} that represents flat contracts.
  
  Every two @tt{flat-obstinacy} values are @racket[equal?].
}

@defproc[(obstinacy-contract/c [ob obstinacy?]) flat-contract?]{
  Returns a flat contract that recognizes contracts that are at least as @tech{reticent} as the given @tech{contract obstinacy}. For instance, @racket[(obstinacy-contract/c (chaperone-obstinacy))] returns a flat contract that's equivalent to (perhaps even identical to) @racket[chaperone-contract?].
}

@defproc[(obstinacy-get-make-contract [ob obstinacy?]) procedure?]{
  Given a @tech{contract obstinacy}, returns the analogous @racket[make-contract], @racket[make-chaperone-contract], or @racket[make-flat-contract] procedure.
}

@defproc[
  (obstinacy-get-coerce-contract-for-id [ob obstinacy?] [id symbol?])
  (-> any/c (obstinacy-contract/c ob))
]{
  Returns a procedure that takes a single value and coerces it to a contract that's at least as @tech{reticent} as the given @tech{contract obstinacy}. This is done using @racket[coerce-contract], @racket[coerce-chaperone-contract], or @racket[coerce-flat-contract].
  
  If the value to be converted is not one of the coercible values, the coercion procedure signals an error with @racket[id] in its error message.
}

@defproc[(value-name-for-contract [v any/c]) any/c]{
  Gets the @racket[contract-name] of the given value if it's a contract, and merely returns the value otherwise. This can be handy when defining a new contract where the @racket[contract-name] may embed arbitrary values, but embedded contracts in particular should be easy to read.
}

@defform[
  (let/c [var-id val-expr] ... body-expr)
  #:contracts ([body-expr contract?])
]{
  Evaluates each @racket[val-expr], evaluates the @racket[body-expr] with those values in scope under respective @racket[var-id] variables, and renames the resulting contract to @racket[`(let/c [var-id ,(value-name-for-contract _val)] ... ,body-expr)], where each @racket[_val] is the result of a @racket[val-expr].
  
  This can come in handy when composiing relatively large contracts that use the same value in multiple places. It keeps the name more concise than it usually would be.
}

@defform*[
  [
    (fix/c self-id options ... contract-expr)
    (fix/c (self-id [arg-id arg-expr] ...) options ... contract-expr)]
  #:contracts ([contract-expr contract?])
]{
  A fixed-point syntax for contracts. Returns the result of running @racket[contract-expr] with a certain contract or contract-returning function in scope as @racket[self-id], and with each given @racket[arg-expr]'s result in scope as the corresponding @racket[arg-id].
  
  In the unparenthesized case, @racket[self-id] is bound to a contract. The contract functionality of @racket[self-id] should be used only after @racket[contract-expr] has returned a contract, and it behaves just like that contract.
  
  In the parenthesized case, @racket[self-id] is bound to a contract-returning function that takes one argument for each @racket[arg-id]. The contract functionality of the result of @racket[self-id] should be used only after @racket[contract-expr] has returned a contract, and it works by evaluating @racket[contract-expr] again with @racket[arg-id] bound to each function argument.
  
  In both cases, the contract obtained from @racket[self-id] is delayed so that it can be used without causing an infinite loop. This functionality is based on @racket[recursive-contract], and the given @racket[options] supply the optional arguments of the @racket[recursive-contract] operation.
  
  @examples[
    #:eval (example-eval)
    (fix/c simple-s-expression/c
      (or/c symbol? (listof simple-s-expression/c)))
  ]
}

@defform[
  (by-own-method/c maybe-obstinacy pat body-expr)
  #:grammar
  [
    (maybe-obstinacy
      (code:line)
      (code:line #:obstinacy obstinacy-expr))]
  #:contracts
  (
    [obstinacy-expr obstinacy?]
    [body-expr (obstinacy-contract/c obstinacy-expr)])
]{
  A syntax for contracts that depend on the value they apply to. Returns a contract that tries to match the value against the match pattern @racket[pat], and if successful, executes the @racket[body-expr] (with all the bound variables from @racket[pat] in scope) and behaves according to that contract. If the match is not successful, then the value is not considered to meet this contract.
  
  A @tech{contract obstinacy} may be given as @racket[obstinacy-expr], which defaults to @racket[(impersonator-obstinacy)]. The overall result is a contract of that contract obsinacy (e.g. a chaperone contract if using @racket[(chaperone-obstinacy)], or any contract at all if using @racket[(impersonator-obstinacy)]). The result of @racket[body-expr] must be a contract of that contract obstinacy as well.
  
  The name of the returned contract includes @racket[pat] and @racket[body-expr] verbatim. If they contain references to variables defined elsewhere, @racket[let/c] may be useful to ensure those variable bindings are apparent in the overall contract name.
}

@defproc[(equal/c [example any/c]) flat-contract?]{
  Returns a contract that recognizes a value if the given value is @racket[equal?] to it.
}

@defproc[(flat-contract-accepting/c [v any/c]) flat-contract?]{
  Returns a flat contract that recognizes any flat contract that recognizes the given value.
}



@section[#:tag "maybe"]{Maybe Values}

@defmodule[lathe-comforts/maybe]

@deftech{Maybe values} are a way to encode optional data. Using maybe values can simplify some interfaces that would otherwise use run time errors or special-cased sentinel values like @racket[#f].


@deftogether[(
  @defidform[nothing]
  @defform[#:link-target? #f (nothing)]
  @defform[#:kind "match expander" #:link-target? #f (nothing)]
  @defproc[(nothing? [v any/c]) boolean?]
)]{
  Struct-like operations which construct and deconstruct a @tech{maybe value} that does not contain an element.
  
  Every two @tt{nothing} values are @racket[equal?].
}

@deftogether[(
  @defidform[just]
  @defform[#:link-target? #f (just value-expr)]
  @defform[#:kind "match expander" #:link-target? #f (just value-pat)]
  @defproc[(just? [v any/c]) boolean?]
  @defproc[(just-value [inst just?]) any/c]
)]{
  Struct-like operations which construct and deconstruct a @tech{maybe value} that contains an element.
  
  Two @tt{just} values are @racket[equal?] if they contain @racket[equal?] elements.
}

@defproc[(maybe? [v any/c]) boolean?]{
  Returns whether the given value is a @tech{maybe value}. That is, it checks that the value is either a @racket[nothing?] value or a @racket[just?] value.
}

@defproc[(maybe/c [c contract?]) contract?]{
  Returns a contract that recognizes a @tech{maybe value} where the element, if any, abides by the given contract.
}

@defproc[(maybe-bind [m maybe?] [func (-> any/c maybe?)]) maybe?]{
  Creates a @tech{maybe value} by replacing the element of the given maybe value, if any, with zero or one elements according to the given function.
}

@defproc[(maybe-map [m maybe?] [func (-> any/c any/c)]) maybe?]{
  Creates a @tech{maybe value} by replacing the element of the given maybe value, if any, with another according to the given function.
}

@defproc[(maybe-if [condition any/c] [get-value (-> any/c)]) maybe?]{
  Creates a @tech{maybe value} that has an element if and only if the given @racket[condition] is not @racket[#f]. The element is computed by calling the given function @racket[get-value] with no arguments.
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

So Lathe Comforts provides a very simple structure type, @racket[trivial], to represent @deftech{trivial values}.


@deftogether[(
  @defidform[trivial]
  @defform[#:link-target? #f (trivial)]
  @defform[#:kind "match expander" #:link-target? #f (trivial)]
  @defproc[(trivial? [v any/c]) boolean?]
)]{
  Struct-like operations which construct and deconstruct a @tech{trivial value}.
  
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

@defproc[(prefab-struct? [v any/c]) boolean?]{
  Returns whether the given value is an instance of a prefab struct type.
}

@defproc[(immutable-prefab-struct? [v any/c]) boolean?]{
  Returns whether the given value is an instance of a prefab struct type and has no mutable fields.
}

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
  
  If @racket[#:error-message-phrase] followed by an expression appears, then that expression will be evaluated as the structure type is defined, and its result will be used in certain error messages of the generated @racket[gen:custom-write] and @racket[gen:equal+hash] implementations, particularly the errors that occur when these methods are called with a value that isn't actually an instance of this structure type. The errors are of the form "expected <this> to be <phrase>", and the default "<phrase>" is "an instance of the <name> structure type". For certain structure types, it may make sense to use @racket[#:error-message-phrase] to change this to a more pithy phrase, like "an RGB color" rather than "an instance of the rgb-color structure type." (However, is it even possible for clients of @racket[struct-easy] to observe these errors in their programs? This feature might not make any visible difference.)
  
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

@defform[
  (istruct/c name-id field/c-expr ...)
  #:contracts ([field/c-expr contract?])
]{
  Returns a contract that recognizes an instance of structure type @racket[name-id] where the fields abide by the respective @racket[field/c-expr] contracts.
  
  Unlike @racket[struct/c] (but like @racket[match/c]), this works even when @racket[name-id] is an immutable structure type name and the @racket[field/c-expr] contracts contain one or more impersonator contracts.
  
  However, this comes at the price of some quirks. This operation works by reconstructing the struct altogether when a higher-order projection is taken. This means the projection of this struct isn't necessarily @racket[eq?], @racket[equal?], or @racket[impersonator-of?] to the original value. In fact, the projection becomes an instance of the structure type @racket[name-id], even when the original value is an instance of a distinct structure subtype of @racket[name-id].
}

@defproc[(tupler? [v any/c]) boolean?]{
  Returns whether the given value is a tupler.
  
  A tupler is an object that carries the constructor, predicate, and positional accessor functionality of an immutable tagged tuple type that's distinct from the type manipulated by any other tupler. Essentially, a tupler carries most of the functionality an immutable @racket[struct] definition creates except for its structure type descriptor. A tupler can only be created by @racket[define-syntax-and-value-imitation-simple-struct].
}

@defproc[(tupler-length [t tupler?]) natural?]{
  Returns the number of projections the given tupler has. This is the number of projections can be accessed from a tuple value and the number of projection values that must be supplied to the tupler's constructor.
}

@defproc[(tupler/c [length/c flat-contract?]) flat-contract?]{
  Returns a flat contract that recognizes a tupler whose length abides by the given flat contract.
}

@defproc[((tupler-pred?-fn [t tupler?]) [v any/c]) boolean?]{
  Returns whether the given value is recognized by the given tupler as one of its tuple values.
}

@defproc[
  (
    (tupler-ref-fn [t tupler?])
    [v (tupler-pred?-fn t)]
    [i (and/c natural? (</c (tupler-length t)))])
  any/c
]{
  Returns the projection at the given index in the given tuple value as observed by the given tupler.
}

@defproc[
  (tupler-proj-fns [t tupler?])
  (listof (-> (tupler-pred?-fn t) any/c))
]{
  Returns a list of projection procedures based on the given tupler.
}

@; NOTE: Even though we've expressed the contract in a way that
@; enforces arity, it's a somewhat convoluted contract to look at, so
@; we don't use it in the documentation here.
@defproc[
  ((tupler-make-fn [t tupler?]) [proj any/c] ...)
  (tupler-pred?-fn t)
]{
  Constructs a tuple value using the given tupler. There should be as many @racket[proj] arguments as the tupler's length.
}

@; NOTE: These are unsafe because they can create misbehaving tuplers,
@; but if we exported them, this would be their documentation.
@;{
@; TODO: See if we can change the contract of `make-fn` to enforce its
@; arity.
@defproc[
  (tupler-from-pred-and-ref-and-make
    [length natural?]
    [pred?-fn (-> any/c boolean?)]
    [ref-fn (-> pred?-fn (and/c natural? (</c length)) any/c)]
    [make-fn (-> any/c ... pred?-fn)])
  tupler?
]{
  Returns a tupler which has the given length and uses the given predicate, positional-index-based accessor procedure, and constructor procedure.
  
  The given procedures should satisfy certain laws. For instance, any value which results from @racket[make-fn] should satisfy @racket[pred?-fn], and the values seen by @racket[ref-fn] should be the same ones that were given to @racket[make-fn]. The procedures shouldn't perform side effects except for the generation of new object identities for the purposes of @racket[eq?]. The values created by @racket[make-fn] shouldn't be accepted by the @racket[pred?-fn] of any other tupler. The procedures shouldn't have important qualities other than their behavior; things like their object names, their @racket[eq?] identities, and the particular @racket[prop:procedure]-implementing structure types they're instances of may or may not be passed through to the results of operations like @racket[tupler-make-fn].
  
  Since this doesn't verify that the given procedures are law-abiding and since other ways of creating tuplers are law-abiding automatically, this operation is considered unsafe.
}

@; TODO: See if we can change the contract of `make-fn` to enforce its
@; arity.
@defproc[
  (tupler-from-pred-and-projs-and-make
    [pred?-fn (-> any/c boolean?)]
    [proj-fns (listof (-> pred?-fn any/c))]
    [make-fn (-> any/c ... pred?-fn)])
  tupler?
]{
  Returns a tupler which uses the given predicate, projection procedures, and constructor procedure.
  
  The given procedures should satisfy certain laws. For instance, any value which results from @racket[make-fn] should satisfy @racket[pred?-fn], and the values seen by @racket[proj-fns] should be the same ones that were given to @racket[make-fn]. The procedures shouldn't perform side effects except for the generation of new object identities for the purposes of @racket[eq?]. The values created by @racket[make-fn] shouldn't be accepted by the @racket[pred?-fn] of any other tupler. The procedures shouldn't have important qualities other than their behavior; things like their object names, their @racket[eq?] identities, and the particular @racket[prop:procedure]-implementing structure types they're instances of may or may not be passed through to the results of operations like @racket[tupler-make-fn].
  
  Since this doesn't verify that the given procedures are law-abiding and since other ways of creating tuplers are law-abiding automatically, this operation is considered unsafe.
}
}

@; TODO: Uncomment this once we need to export it.
@;{
@defproc[
  (tupler-for-simple-make-struct-type
    [length natural?]
    [reflection-name symbol?]
    [inspector (or/c inspector? #f 'prefab)]
    [props (listof (cons/c struct-type-property? any/c))])
  tupler?
]{
  Creates a new immutable structure type with the given number of fields, reflection name, inspector, and list of structure type property bindings.
  
  The reflection name is used for certain reflective operations.
  
  If the inspector is the value @racket['prefab], this accesses a prefab structure type instead of creating a new one, and there must be no structure type property bindings in the list.
  
  If the inspector is the value @racket[#f], the resulting structure type is transparent.
  
  Note that unlike @racket[make-struct-type], this has no support for creating structure types that have supertypes, subtypes, guard procedures, mutable fields, or automatic fields. For the most part, all these features except supertypes and subtypes can be simulated: Mutable fields can be simulated with immutable fields that contain mutable boxes, while guard procedures and automatic fields can be simulated by defining another procedure to call instead of calling this tupler's constructor directly.
  
  To have more complete control over the structure type created, use @racket[make-struct-type]. However, that will not produce a tupler value.
}
}

@deftogether[(
  @defform[
    (define-imitation-simple-struct
      (inst?-id inst-field-id ...)
      inst-id
      reflection-name-expr inspector-expr option ...)
  ]
  @defform[
    (define-syntax-and-value-imitation-simple-struct
      (inst?-id inst-field-id ...)
      inst-id
      tupler-id
      reflection-name-expr inspector-expr option ...)
    #:grammar
    [
      (option
        (#:prop prop-expr prop-val-expr)
        (#:gen gen:name method-definition ...)
        (@#,racket[auto-write])
        (@#,racket[auto-equal]))]
    #:contracts
    (
      [inspector-expr (or/c inspector? #f 'prefab)]
      [reflection-name-expr symbol?]
      [prop-expr struct-type-property?])
  ]
)]{
  Creates a new structure type, and defines struct-like operations which construct and deconstruct an actual structure value of that type.
  
  The variable @racket[inst?-id] is defined to be a predicate that detects these structures.
  
  Each variable @racket[inst-field-id] is defined to be a procedure that accesses the corresponding positional field of any given one of these structures.
  
  The variable @racket[inst-id] is defined to be a match expander that can be used to construct or match these structures.
  
  The variable @racket[tupler-id] (when using @tt{define-syntax-and-value-imitation-simple-struct}) is defined to be a first-class tupler object that can be used to construct or match these structures.
  
  The structure type is created with the given reflection name, the given inspector, a number of fields equal to the number of @racket[inst-field-id] variables specified, and structure type property bindings determined by the given @racket[option] entries.
  
  The reflection name is used for certain reflective operations.
  
  If the inspector is the value @racket['prefab], this accesses a prefab structure type instead of creating a new one, and there must be no @racket[option] entries specified.
  
  An @racket[option] of the form @racket[(#:prop prop-expr prop-val-expr)] is like providing the @racket[struct] option @racket[#:property prop-expr prop-val-expr]. It specifies a binding of the @racket[prop-expr] structure type property to the value @racket[prop-val-expr].
  
  An @racket[option] of the form @racket[(#:gen gen:name method-definition ...)] is like providing the @racket[struct] option @racket[#:methods gen:name [method-definition ...]]. The @racket[gen:name] must be an identifier with a transformer binding that specifies a generic interface as defined by @racket[define-generics]. The @racket[method-definition] forms define the implementation of this interface's methods for this new structure type just as they do in @racket[struct].
  
  An @racket[option] of the form @racket[(auto-write)] specifies implementations of @racket[gen:custom-write] and @racket[prop:custom-print-quotable] which use @racket[make-constructor-style-printer] to display the value.
  
  An @racket[option] of the form @racket[(auto-equal)] specifies an implementation of @racket[gen:equal+hash] which treats any two of these structures as @racket[equal?] if their fields are @racket[equal?].
  
  Note that unlike @racket[struct], this has no support for creating structure types that have supertypes, subtypes, guard procedures, mutable fields, or automatic fields. For the most part, all these features except supertypes and subtypes can be simulated: Mutable fields can be simulated with immutable fields that contain mutable boxes, while guard procedures and automatic fields can be simulated by defining another procedure to call instead of calling the defined constructor directly.
  
  To have more complete control over the structure type created, use @racket[struct]. However, that will not produce a tupler value.
}

@defform[#:kind "structure type property expander" (auto-write)]{
  A syntax which is only useful as an option to @racket[define-imitation-simple-struct] and @racket[define-syntax-and-value-imitation-simple-struct]. In that context, it specifies that the created structure type should have implementations of @racket[gen:custom-write] and @racket[prop:custom-print-quotable] which use @racket[make-constructor-style-printer] to display a structure value.
}

@defform[#:kind "structure type property expander" (auto-equal)]{
  A syntax which is only useful as an option to @racket[define-imitation-simple-struct] and @racket[define-syntax-and-value-imitation-simple-struct]. In that context, it specifies that the created structure type should have an implementation of @racket[gen:equal+hash] which treats any two of the structure values as @racket[equal?] if their fields are @racket[equal?].
}

@defform[
  (define-imitation-simple-generics
    inst?-id inst-impl?-id
    (#:method method-id () ... (#:this) () ...)
    ...
    prop:inst-id build-inst-impl-id
    prop-name-expr inst-impl-reflection-name-expr supers-expr)
  #:contracts
  (
    [prop-name-expr symbol?]
    [inst-impl-reflection-name-expr symbol?]
    [supers-expr
      (listof (cons/c struct-type-property? (-> _inst-impl? any/c)))])
]{
  Creates a new structure type property, creates a new structure type for use as the type of implementations of that property, and defines generic-interface-like operations for using these things.
  
  The variable @racket[inst?-id] is defined to be a predicate which determines whether the given value implements the property. Usually a value is considered to implement the property if its structure type has an implementation associated with the property. A structure type descriptor is special; it's considered to implement the property if the structure type it's a descriptor for has an implementation associated with the property.
  
  The variable @racket[inst-impl?-id] is defined to be a predicate which recognizes values of the structure type created for representing implementations of this property.
  
  Each variable @racket[method-id] is defined to be a procedure of the indicated number of arguments. Each @racket[()] occurrence represents one argument, and the occurrence of @racket[(#:this)] represents one argument which receives special treatment. The argument in the @racket[(#:this)] position is expected to be a value which implements the property. Its implementation for the property determines the rest of the procedure's behavior.
  
  The variable @racket[prop:inst-id] is defined to a structure type property descriptor. When a structure type has an implementation associated with the descriptor @racket[prop:inst-id], that implementation is also associated with the property the rest of these operations interact with. (In fact, for now this is the descriptor of the property itself.)
  
  The variable @racket[build-inst-impl-id] is defined to be a procedure which constructs a value which represents an implementation of this property from N arguments, where N is the number of @racket[#:method] clauses in this definition. Each argument must be an implementation to use for the corresponding method.
  
  The symbol resulting from @racket[prop-name-expr] is used for certain reflective operations on the structure type property descriptor.
  
  The symbol resulting from @racket[inst-impl-reflection-name-expr] is used for certain reflective operations on instances of the structure type of implementations.
  
  The list resulting from @racket[supers-expr] has the same meaning as the @racket[_supers] argument to @racket[make-struct-type-property]. It's an association list mapping a structure type property to a function which transform the implementation of this property into the implementation of that one. When a structure type has an implementation of this property, it has an implementation of all of those as well, generated by the given functions.
  
  When composing a @racket[_supers] argument to pass in, keep in mind that the values representing the implementation of this property are very opaque. Usually the only way to use them is by incorporating them into the creation of another structure type. Most @racket[_supers] arguments will simply use constant values or call the generic methods, ignoring the implementation value they receive altogether.
  
  Note that unlike @racket[define-generics], this has no support for creating generic interface information that can be used with the @racket[struct] @racket[#:method] syntax; it has no support for giving methods optional arguments, keyword arguments, rest arguments, or non-dispatching fast paths; it has no support for letting instances implement some methods but not others; and it does not define a contract combinator.
  
  Some of these drawbacks are tricky to work around, but not all of them. Methods which take complex argument lists or which have fast paths can be defined as wrapper procedures. The idea of an unimplemented method can be approximated by adding a second method that returns a boolean saying whether the first is implemented.
  
  The @racket[#:method] syntax is not especially feasible. It's possible to use it by writing a separate @racket[define-generics] interface with a @racket[#:derive-property] option that populates this property. However, users should be discouraged from calling that wrapper interface's methods directly or using its contract combinator, since those things will look at the @racket[define-generics] interface, not this property it's intended to populate. Instead of using the @racket[#:method] syntax, it's recommended to define various procedures and macros that make calls to @racket[build-inst-impl-id] with the desired combinations of functionality.
  
  Even the contract combinator can be simulated in a more big-picture sense: Higher-order contracts for structure type properties will tend to need to replace a property's implementation with an instrumented version. Not all structure types that implement a properly will do so in a way that's easily decoupled from their other functionality, much less swapped out for another whole implementation. That means the ability to swap something out is essentially another method that structure type can implement, and it can be designed as such.
  
  When writing contracts for values whose design isn't easy to change, another approach is to intercept those values at the boundaries of a program and convert them into a custom structure type whose design is easier to control.
}



@section[#:tag "match"]{Utilities for Match Expanders}

@defmodule[lathe-comforts/match]

@defform[
  (define-match-expander-from-match-and-make
    new-name-id match-name-id make-id-name-id make-list-name-id)
]{
  Defines @racket[new-name-id] as a syntax that expands into a call to @racket[match-name-id] if it's used a match expander, @racket[make-id-name-id] if it's used as an standalone identifier in an expression context, and @racket[make-list-name-id] if it's used at the beginning of a list in an expression context.
}

@defform[
  (define-match-expander-attenuated
    new-name-id old-name-id [arg-id arg/c-expr] ... guard-expr)
  #:contracts ([arg/c-expr contract?])
]{
  Defines @racket[new-name-id] as a syntax that acts as a match expander which takes one subform for each @racket[arg-id] and expands into a pattern using @racket[old-name-id] with the same number of subforms.
  
  When the syntax defined this way used as an expression syntax with one subexpression for each @racket[arg/c-expr], it first executes each subexpression, projects them each through the corresponding @racket[arg/c-expr] contract, and evaluates @racket[guard-expr] with @racket[arg-id] bound to those projections. If the result of @racket[guard-expr] is @racket[#f], a precondition representing to this guard fails, raising an @racket[exn:fail:contract] exception. Otherwise, this returns the result of @racket[(old-name-id arg-id ...)].
  
  When the syntax defined this way is used as an identifier, it returns a function that performs the expression syntax behavior when it's called.
  
  When the syntax defined this way is used as a match expander with one pattern for each @racket[arg-id], it first tries to match the same arguments according to @racket[(old-name-id arg-id ...)], and it fails if that pattern fails. Then it tries to check each of the @racket[arg/c-expr] contracts' @racket[contract-first-order-passes?] behavior against the respective argument value, and it fails if any of those fails. Then it projects the arguments through those contracts, and it attempts to check the @racket[guard-expr] with those projections, failing if the @racket[guard-expr] returns @racket[#f]. If it hasn't failed yet, it proceeds to match those projections according to the patterns given at the call site.
  
  @; TODO: This description seems a bit too terse. It might be good to give examples.
}

@defform[
  (match/c name-id arg/c-expr ...)
  #:contracts ([arg/c-expr contract?])
]{
  Returns a contract that recognizes a value if it matches a pattern of the form @racket[(name-id _arg-id ...)], where each @racket[_arg-id] is an identifier, and only as long as each value bound to a @racket[_arg-id] this way abides by the respective @racket[arg/c-expr] contract.
  
  The value's projection is computed by taking the projections of each of the arguments and then executing @racket[(name-id _arg-id ...)], where this time each @racket[_arg-id] is an identifier already bound to the argument's projection value. For some match patterns, this may cause substantial changes to the value when projected by this contract: If @racket[name-id] is @racket[vector], it changes immutable vectors to mutable ones. If @racket[name-id] is a structure type name, it changes instances of subtypes of @racket[name-id] into instances of @racket[name-id] itself.
  
  Unlike @racket[struct/c] (but like @racket[istruct/c]), this works even when @racket[name-id] is an immutable structure type name and the @racket[arg/c-expr] contracts contain one or more impersonator contracts.
}



@include-section["lathe-comforts/syntax.scrbl"]



@include-section["lathe-comforts/own-contract.scrbl"]
