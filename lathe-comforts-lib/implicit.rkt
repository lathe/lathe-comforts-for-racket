#lang parendown racket/base

; lathe-comforts/implicit
;
; An extensible system of lexically scoped dispatchers.

;   Copyright 2020 The Lathe Authors
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


(require #/for-syntax racket/base)
(require #/for-syntax #/only-in racket/syntax syntax-local-eval)

(require #/for-syntax #/only-in lathe-comforts/struct
  auto-equal auto-write define-imitation-simple-struct)
(require #/for-syntax #/only-in syntax/parse expr syntax-parse)

(require #/for-syntax #/only-in lathe-comforts dissect expect fn w-)
(require #/for-syntax #/only-in lathe-comforts/hash hash-ref-maybe)
(require #/for-syntax #/only-in lathe-comforts/maybe just)


(require #/only-in racket/contract/base -> any/c contract-out or/c)
(require #/only-in syntax/parse/define define-simple-macro)

(require #/only-in lathe-comforts expect w-)
(require #/only-in lathe-comforts/maybe maybe?)


(provide
  define-empty-aux-env
  let-implicit
  quote-implicit)
; TODO NOW: Figure out how to uncomment this. We might need a few
; intermediate submodules.
#;
(provide #/contract-out
  [syntax-local-implicit-value-maybe
    (-> (or/c #f syntax?) any/c maybe?)])


(begin-for-syntax
  (define-imitation-simple-struct (aux-env? aux-hash) aux-env
    'aux-env (current-inspector) (auto-write) (auto-equal)))

(define-syntax (define-empty-aux-env stx)
  (syntax-protect
  #/syntax-parse stx #/ (_)
    #`(define-syntax
        #,(syntax-local-introduce #/datum->syntax #f '#%aux-env)
        (aux-env #/hash))))

(define-for-syntax (let-implicit-fn var val env)
  (dissect env (aux-env hash)
  ; TODO: Also track sets of scopes.
  #/aux-env #/hash-set hash var val))

(define-for-syntax (syntax-local-aux-env stx)
  (syntax-local-value #/datum->syntax stx '#%aux-env))

(define-syntax (let-implicit stx)
  (syntax-protect
  #/syntax-parse stx #/ (_ var-expr:expr val:expr body:expr)
  #/w- aux-env (syntax-local-aux-env stx)
  #/w- var (syntax-local-eval #'var-expr)
    #`(let-syntax
        (
          [
            #,(datum->syntax stx '#%aux-env)
            (let-implicit-fn var-expr val ('#,(fn aux-env)))])
        body)))

(define-for-syntax (syntax-local-implicit-value-maybe stx var)
  (w- env (syntax-local-aux-env stx)
  #/expect env (aux-env hash)
    (raise-arguments-error 'syntax-local-implicit-value-maybe
      "encountered an unexpected value as the auxiliary environment"
      "aux-env" env)
  #/hash-ref-maybe hash var))

(define-syntax (quote-implicit stx)
  (syntax-protect
  #/syntax-parse stx #/ (_ var-expr:expr)
  #/w- aux-env (syntax-local-aux-env stx)
  #/w- var (syntax-local-eval #'var-expr)
  #/expect (syntax-local-implicit-value-maybe stx var) (just val)
    (raise-syntax-error #f
      "unbound implicit variable"
      stx
      #'var-expr)
    #`'#,val))


; TODO NOW: Figure out some more tests to do on these. Everything
; seems to be passing with flying colors.

; TODO NOW: Change `let-implicit` so it can bind more than one thing
; at a time. Consider also making an easy way for its right-hand-sides
; to refer to existing implicit bindings (but first, we should see if
; we can do it the long way using
; `syntax-local-implicit-value-maybe`). Together, these will make it
; possible for `let-implicit` to express permutations of the implicit
; bindings.

; TODO: Consider having `eval-implicit`, like `quote-implicit` but
; without the quotation. Maybe we should call it `begin-implicit` or
; something just to avoid the `eval` connotations.

; TODO NOW: Add other features to realize a more complete,
; type-class-like vision, such as the ability to install local
; bindings that execute Turing-complete predicates when looking up a
; variable to find a candidate binding list. If there are two or more
; candidates in the nearest level of scope, that's an error.

; TODO NOW: Move these to the lathe-comforts-test package, and make
; them proper RackUnit unit tests.

(define-empty-aux-env)
(begin-for-syntax (displayln (syntax-local-aux-env #'())))

(w- x 1
  (let-implicit 4 "correct"
    (w- x 2
      (quote-implicit 4))))

(let-implicit 4 "cor"
  (let-syntax ([four (syntax-id-rules () [_ (quote-implicit 4)])])
    (let-implicit 4 "rect"
      (string-append four (quote-implicit 4)))))

(define-simple-macro
  (using-implicit-4-as-an-implementation-detail body:expr)
  (let-implicit 4 "inner"
    body))

(let-implicit 4 "correct"
  (using-implicit-4-as-an-implementation-detail
    (quote-implicit 4)))

