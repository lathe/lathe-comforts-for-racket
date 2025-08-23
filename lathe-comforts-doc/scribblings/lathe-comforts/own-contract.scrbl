#lang parendown/slash scribble/manual

@; lathe-comforts/scribblings/lathe-comforts/own-contract.scrbl
@;
@; A framework for defining contracts close by to the functions
@; they're for, with configuration options for suppressing external
@; contracts and/or activating certain internal contracts.

@;   Copyright 2022, 2024-2025 The Lathe Authors
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


@(define @enforces-autopticity[]
  @list{
    This syntax must be called with @tech{autopticity}. An occurrence of a cons cell, empty list, or keyword that's part of the call syntax must have a set of scopes that's equal to or a superset of the set of scopes on the entire call, as though the call has created a local binding of what a cons cell means in these positions. This helps ensure that even though Racket expressions are often made of cons cells, an expression inserted into one of these positions by a macro's syntax template will not have its cons cells misinterpreted.
  })


@title[#:tag "own-contract"]{
  A Framework for Declaring Contracts Alongside Functions
}

@defmodule[lathe-comforts/own-contract]

The @tt{lathe-comforts/own-contract} module defines a small and unstable DSL for managing the use of contracts in a module. The combination of @racket[define/own-contract] and @racket[own-contract-out] works like specifying a contract in a @racket[contract-out] form, but it allows the contract to be written at the function definition site in the style of @racket[define/contract]. To use the DSL, it's necessary to use @racket[define-own-contract-policies] first.

@defform[
  (define-own-contract-policies option ...)
  #:grammar
  [
    (option
      (code:line #:antecedent-land stx)
      (code:line #:make-signature-contract-id
        make-signature-contract-id-expr)
      (code:line #:suppressing-external-contracts?
        suppressing-external-contracts?-expr)
      (code:line #:activating-external-contracts?
        activating-external-contracts?-expr))]
  #:contracts
  (
    [make-signature-contract-id-expr
      (or/c #f (-> identifier? identifier?))]
    [suppressing-external-contracts?-expr boolean?]
    [activating-external-contracts?-expr boolean?])
]{
  Sets up the @racketmodname[lathe-comforts/own-contract] DSL for the current module. Certain policies can be specified that adjust the behavior of the DSL.
  
  @enforces-autopticity[]
  
  @specsubform[(code:line #:antecedent-land antecedent-land)]{
    The various syntactic forms of the @racketmodname[lathe-comforts/own-contract] DSL communicate by way of anaphoric bindings. This option causes the @tt{define-own-contract-policies} form to define these bindings so that they have a lexical context derived from the lexical context of the syntax of @racket[antecedent-land].
    
    Everything else about @racket[antecedent-land] is ignored; it is not evaluated as an expression.
  }
  
  @specsubform[
    (code:line #:make-signature-contract-id
      make-signature-contract-id-expr)
    
    #:contracts
    (
      [ make-signature-contract-id
        (or/c #f (-> identifier? identifier?))])
  ]{
    Evaluates @racket[make-signature-contract-id-expr] at phase level 1 relative to the surrounding context and uses the result to determine what variables the DSL uses to keep track of a binding's associated contract.
    
    If the result is a procedure, the procedure will be called each time the DSL determines the variable that an associated contract will be found in. It will be passed the identifier of the value binding, and its result will be taken as the identifier of the associated contract binding.
    
    If the result is @racket[#f], a procedure is used that adds a unique but consistent scope to the identifiers it receives. Essentially, the default policy value is @racket[(make-syntax-introducer)].
  }
  
  @specsubform[
    (code:line #:suppressing-external-contracts?
      suppressing-external-contaracts?-expr)
    #:contracts ([suppressing-external-contaracts?-expr boolean?])
  ]{
    Evaluates @racket[suppressing-external-contaracts?-expr] at phase level 1 relative to the surrounding context and uses the result to determine whether the DSL suppresses contracts on external uses of a variable. The default is @racket[#f], which causes the DSL to enforce contracts on external uses.
    
    Note that currently, any use via a module import is considered an external use, even if the importing module is part of the same codebase as the module that's being imported. This DSL may be extended or modified in the future with more options for treating such uses as internal.
  }
  
  @specsubform[
    (code:line #:activating-internal-contracts?
      activating-internal-contaracts?-expr)
    #:contracts ([activating-internal-contaracts?-expr boolean?])
  ]{
    Evaluates @racket[activating-internal-contaracts?-expr] at phase level 1 relative to the surrounding context and uses the result to determine whether the DSL attempts to enforce contracts on external uses of a variable. The default is @racket[#f], which causes the DSL to ignore contracts for internal uses.
    
    Note that currently, only contracts ascribed using @racket[define/own-contract] can be activated this way. Contracts ascribed using @racket[ascribe-own-contract] don't affect the internal binding of the variable they annotate, so direct uses of that variable will continue to behave in ways that aren't protected by the contract.
    
    Currently, unlike most ways of imposing a contract, this enforcement doesn't establish a boundary between two parties. The surrounding module is blamed for any violations of the contract. See @racket[invariant-assertion]. Like @racket[invariant-assertion], and unlike @racket[define/contract], the contract is enforced for recursive function calls.
  }
}

@defform[
  (own-contract-out maybe-antecedent-land id ...)
  #:grammar
  [
    (maybe-antecedent-land
      (code:line)
      (code:line #:antecedent-land stx))]
]{
  A @racket[_provide-spec] for use with @racket[provide]. Each @racket[id] is provided from the module, protected with its associated @racket[define/own-contract] or @racket[ascribe-own-contract] contract as though using @racket[contract-out].
  
  Like @racket[contract-out], currently this only supports providing identifiers at the same phase level as the @racket[provide] form. It won't work when nested inside @racket[for-syntax].
  
  The @racketmodname[lathe-comforts/own-contract] DSL must be set up for the current module using @racket[define-own-contract-policies] before calling this.
  
  @enforces-autopticity[]
  
  @specsubform[(code:line #:antecedent-land antecedent-land)]{
    The various syntactic forms of the @racketmodname[lathe-comforts/own-contract] DSL communicate by way of anaphoric bindings. This option causes the @tt{own-contract-out} form to retrieve these bindings using a lexical context derived from the lexical context of the syntax of @racket[antecedent-land].
    
    Everything else about @racket[antecedent-land] is ignored; it is not evaluated as an expression.
  }
}

@defform*[
  
  [
    (define/own-contract id
      contract-expr
      maybe-antecedent-land
      init-value-expr)
    (define/own-contract (head args)
      contract-expr
      maybe-antecedent-land
      body ...+)]
  
  #:grammar
  [
    (maybe-antecedent-land
      (code:line)
      (code:line #:antecedent-land stx))
    (head id (head args))
    (args
      (code:line arg ...)
      (code:line arg ... @#,racketparenfont{.} rest-id))
    (arg
      arg-id
      [arg-id default-expr]
      (code:line keyword arg-id)
      (code:line keyword [arg-id default-expr]))]
  
  #:contracts ([contract-expr contract?])
]{
  Works like @racket[define] or @racket[define/contract], except that it associates the given @racket[contract-expr] with @racket[id] for the purposes of @racket[own-contract-out]. The @racket[contract-expr] expression is evaluated right away if the @racket[#:activating-internal-contracts?] policy is in effect, or otherwise lazily when it's needed by @racket[own-contract-out].
  
  The @racketmodname[lathe-comforts/own-contract] DSL must be set up for the current module using @racket[define-own-contract-policies] before calling this.
  
  If the @racket[#:activating-internal-contracts?] policy is in effect (though it is not in effect by default), uses of @racket[id] are protected by the given contract even within the current module. Currently, unlike most ways of imposing a contract, this enforcement doesn't establish a boundary between two parties. The surrounding module is blamed for any violations of the contract. See @racket[invariant-assertion]. Like @racket[invariant-assertion], and unlike @racket[define/contract], the contract is enforced for recursive function calls.
  
  @enforces-autopticity[]
  
  @specsubform[(code:line #:antecedent-land antecedent-land)]{
    The various syntactic forms of the @racketmodname[lathe-comforts/own-contract] DSL communicate by way of anaphoric bindings. This option causes the @tt{define/own-contract} form to retrieve these bindings using a lexical context derived from the lexical context of the syntax of @racket[antecedent-land].
    
    Everything else about @racket[antecedent-land] is ignored; it is not evaluated as an expression.
  }
}

@defform[
  (ascribe-own-contract id contract-expr maybe-antecedent-land)
  
  #:grammar
  [
    (maybe-antecedent-land
      (code:line)
      (code:line #:antecedent-land stx))]
  
  #:contracts ([contract-expr contract?])
]{
  Associates the given @racket[contract-expr] with the given @racket[id] for the purposes of @racket[own-contract-out]. The @racket[contract-expr] expression is evaluated lazily when needed by @racket[own-contract-out].
  
  The @racketmodname[lathe-comforts/own-contract] DSL must be set up for the current module using @racket[define-own-contract-policies] before calling this.
  
  Note that currently, even if the @racket[#:activating-internal-contracts?] policy is in effect, code that uses @tt{ascribe-own-contract} won't have its internal contracts activated. Contracts ascribed using @tt{ascribe-own-contract} don't affect the internal binding of the variable they annotate, so direct uses of that variable will continue to behave in ways that aren't protected by the contract.
  
  @enforces-autopticity[]
  
  @specsubform[(code:line #:antecedent-land antecedent-land)]{
    The various syntactic forms of the @racketmodname[lathe-comforts/own-contract] DSL communicate by way of anaphoric bindings. This option causes the @tt{ascribe-own-contract} form to retrieve these bindings using a lexical context derived from the lexical context of the syntax of @racket[antecedent-land].
    
    Everything else about @racket[antecedent-land] is ignored; it is not evaluated as an expression.
  }
}
