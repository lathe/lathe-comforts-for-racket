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

@; TODO: Once Parendown has documentation, make sure the references to
@; `pd` in these docs become hyperlinks.
(require #/for-label #/only-in parendown pd)

@(require #/for-label lathe-comforts)

@(require #/only-in scribble/example examples make-eval-factory)

@(define example-eval
  (make-eval-factory
  #/list 'racket/base 'racket/match 'lathe-comforts 'parendown))


@title{Lathe Comforts}

@defmodule[lathe-comforts]

Lathe Comforts for Racket is a collection of utilities that are handy for writing Racket code. This is a non-intrusive toolkit; in most cases it should only make certain Racket code easier to write, without substantially changing the architecture of the project it's used in.

Some of these utilities are designed with Parendown in mind. In some cases, Parendown's weak opening brackets make it easier to get by with higher-order functions instead of custom syntax. (Note that due to limitations of Scribble's Racket code formatter, we use Parendown's `pd` macro to achieve these weak parens, rather than using its custom reader syntax.)



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
