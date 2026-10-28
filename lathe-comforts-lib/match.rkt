#lang parendown racket/base

; lathe-comforts/match
;
; Utilities for match expanders.

;   Copyright 2019-2020, 2022 The Lathe Authors
;
;   Licensed under the Apache License, Version 2.0 (the "License");
;   you may not use this file except in compliance with the License.
;   You may obtain a copy of the License at
;
;       http://www.apache.org/licenses/LICENSE-2.0
;
;   Unless required by applicable law or agreed to in writing,
;   software distributed under the License is distributed on an
;   "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
;   either express or implied. See the License for the specific
;   language governing permissions and limitations under the License.


(require lathe-comforts/private/shim)
(init-shim)

(require #/for-syntax #/only-in lathe-comforts fn)
(require #/for-syntax #/only-in lathe-comforts/syntax ~autoptic-list)

(require #/only-in lathe-comforts
  dissect dissectfn expect expectfn fn mat w-)


(provide
  define-match-expander-from-match-and-make
  define-match-expander-attenuated
  match/c)



(define-syntax-parser define-match-expander-from-match-and-make #/
  {~autoptic-list
    (_ new-name:id match-name:id make-id-name:id make-list-name:id)}
  (quasisyntax/loc this-syntax
    (define-match-expander new-name
      #,
      (syntax/loc this-syntax
        (... #/syntax-parser #/ {~autoptic-list (_ arg ...)}
          (quasisyntax/loc this-syntax (match-name arg ...))))
      #,
      (syntax/loc this-syntax
        (... #/syntax-parser
          [_:id (quasisyntax/loc this-syntax make-id-name)]
          [ {~autoptic-list (_ arg ...)}
            (quasisyntax/loc this-syntax
              (make-list-name arg ...))])))))


(define-syntax-parser define-match-expander-attenuated #/
  {~autoptic-list
    (_ new-name:id old-name:id
      {~autoptic-list
        [
          arg
          {~var arg/c
            (expr/c #'contract? #:name "an argument contract")}]}
      ...
      guard-expr:expr)}
  #:with (arg-pat ...) (generate-temporaries #'(arg ...))
  #:with (contracted-arg ...) (generate-temporaries #'(arg ...))
  #:with (arg/c-result ...) (generate-temporaries #'(arg/c ...))
  #`
  (begin
    
    (define arg/c-result arg/c.c)
    ...
    #,
    (syntax/loc this-syntax
      (define (passes-guard? arg ...)
        guard-expr))
    
    #,
    (quasisyntax/loc this-syntax
      (define-match-expander new-name
        #,
        (quasisyntax/loc this-syntax
          (syntax-parser #/
            ; TODO: We should really use a syntax class for match
            ; patterns rather than `expr` here, but it doesn't look
            ; like one exists yet.
            {~autoptic-list (_ {~var arg-pat expr} ...)}
            
            #:with contracted-guard
            (wrap-expr/c
              #'(->i ([arg arg/c-result] ...) [_ any/c])
              #'(fn arg ... #/passes-guard? arg ...)
              ; NOTE: The `#:positive` and `#:negative`
              ; arguments here are the usual values but swapped.
              #:positive 'from-macro
              #:negative 'use-site
              #:context this-syntax)
            
            {~@ #:with contracted-arg
              (wrap-expr/c #'arg/c-result #'arg
                #:context this-syntax)}
            ...
            
            #'
            (app
              (expectfn (old-name arg ...) #f
                (and
                  (contract-first-order-passes? arg/c-result arg)
                  ...
                #/let ([arg contracted-arg] ...)
                #/and (contracted-guard arg ...)
                #/list arg ...))
              (list arg-pat ...))))
        #,
        (quasisyntax/loc this-syntax
          (fn stx
            (with-syntax
              (
                [contracted-function
                  (wrap-expr/c
                    #'(->i ([arg arg/c-result] ...)
                        #:pre (arg ...) (passes-guard? arg ...)
                        any)
                    #'(fn arg ... #/old-name arg ...)
                    ; NOTE: The `#:positive` and `#:negative`
                    ; arguments here are the usual values but swapped.
                    #:positive 'from-macro
                    #:negative 'use-site
                    #:context stx)])
            #/syntax-parse stx
              
              ; NOTE: We allow the expander to expand into an
              ; expression that creates a construction-only procedure
              ; version of itself when it's used directly as an
              ; identifier. That way, we have leeway to upgrade
              ; constructor-like functions into match expanders that
              ; are defined this way.
              ;
              ; NOTE: We have the procedure report contract errors in
              ; terms of the place this expansion has been performed.
              ; If we defined a single procedure and returned that one
              ; every time, it would have to report its contract
              ; errors in terms of the place
              ; `define-match-expander-attenuated` was called, which
              ; would be less specific.
              ;
              [ _:id
                #'(procedure-rename contracted-function 'new-name)]
              
              [ {~autoptic-list (_ arg ...)}
                #'(contracted-function arg ...)])))))))


(define (match/c-impl foo-name foo->maybe-list list->foo args)
  (w- args
    (for/list ([arg (in-list args)])
      (dissect arg (list arg/c arg-blame-message)
      #/list (coerce-contract 'match/c arg/c) arg-blame-message))
  #/w- arg/cs
    (for/list ([arg (in-list args)])
      (dissect arg (list arg/c arg-blame-message)
        arg/c))
  #/w- name
    (list* 'match/c foo-name
    #/for/list ([arg/c (in-list arg/cs)])
      (contract-name arg/c))
  #/w- is-flat-contract?
    (for/and ([arg/c (in-list arg/cs)])
      (flat-contract? arg/c))
  #/
    (if is-flat-contract?
      make-flat-contract
      make-contract)
    
    #:name name
    
    #:first-order
    (fn v
      (w- v-list (foo->maybe-list v)
      #/and v-list
      #/for/and
        ([arg/c (in-list arg/cs)] [v-arg (in-list v-list)])
        (contract-first-order-passes? arg/c v-arg)))
    
    #:late-neg-projection
    (fn blame
      (w- arg-projections
        (for/list ([arg (in-list args)])
          (dissect arg (list arg/c arg-blame-message)
          #/(get/build-late-neg-projection arg/c)
            (blame-add-context blame arg-blame-message)))
      #/fn v missing-party
        (w- v-list (foo->maybe-list v)
        #/mat v-list #f
          (raise-blame-error blame #:missing-party missing-party v
            '(expected: "~e" given: "~e")
            name v)
        #/if is-flat-contract?
          (begin
            (for
              (
                [arg-late-neg-projection (in-list arg-projections)]
                [v-arg (in-list v-list)])
              (arg-late-neg-projection v-arg missing-party))
            v)
        #/list->foo
          (for/list
            (
              [arg-late-neg-projection (in-list arg-projections)]
              [v-arg (in-list v-list)])
            (arg-late-neg-projection v-arg missing-party)))))))

(define-syntax-parser match/c #/
  {~autoptic-list
    (_ name:id
      ; TODO: See how well this `#:name` helps. It seems pretty vague.
      {~var arg/c (expr/c #'contract? #:name "one of the arguments")}
      ...)}
  #:with (arg ...) (generate-temporaries #'(arg/c ...))
  #`
  (match/c-impl
    'name
    (fn v
      (expect v #,(syntax/loc this-syntax (name arg ...)) #f
      #/list arg ...))
    (dissectfn (list arg ...)
      (name arg ...))
    (list
      #,@
      (for/list
        (
          [i (in-naturals)]
          [arg/c (in-list (syntax->list #'(arg/c.c ...)))])
        #`(list #,arg/c #,(format "position ~a of" i))))))
