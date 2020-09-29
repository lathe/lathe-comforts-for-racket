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
(require #/for-syntax #/only-in racket/hash hash-union)
(require #/for-syntax #/only-in racket/syntax syntax-local-eval)
(require #/for-syntax #/only-in syntax/id-table
  bound-id-table-ref bound-id-table-set make-immutable-bound-id-table)
(require #/for-syntax #/only-in syntax/parse ~and expr syntax-parse)

(require #/for-syntax #/only-in lathe-comforts
  dissect expect fn mat w-)
(require #/for-syntax #/only-in lathe-comforts/list
  list-foldl list-map)
(require #/for-syntax #/only-in lathe-comforts/maybe just)


(require #/only-in racket/contract/base -> any/c contract-out or/c)
(require #/only-in syntax/parse/define define-simple-macro)

(require #/only-in lathe-comforts expect w-)
(require #/only-in lathe-comforts/maybe just maybe?)


(provide #/contract-out
  [syntax-local-implicit-value-maybe
    (-> (or/c #f syntax?) any/c maybe?)]
  [syntax-local-implicit-value
    (-> (or/c #f syntax?) any/c any/c)])
(provide
  define-empty-aux-env
  let-implicits-with-scopes
  let-implicits
  let-implicit
  quote-implicit
  local-implicit)


(module private/part-1 racket/base
  
  (require #/only-in lathe-comforts/struct
    auto-equal auto-write define-imitation-simple-struct)
  
  (require #/only-in lathe-comforts dissect expect fn w-)
  (require #/only-in lathe-comforts/hash hash-ref-maybe)
  
  
  (provide #/all-defined-out)
  
  
  (define-imitation-simple-struct (aux-env? aux-hash) aux-env
    'aux-env (current-inspector) (auto-write) (auto-equal))
  
  (define introduce-aux-env-scope
    (make-interned-syntax-introducer
      (string->uninterned-symbol "aux-env-ns")))
  
  (define (aux-env-id stx)
    (introduce-aux-env-scope #/datum->syntax stx '#%aux-env))
  
  (define (syntax-local-aux-env who stx)
    (w- env
      (syntax-local-value (aux-env-id stx) #/fn
        (raise-arguments-error who
          "expected an auxiliary environment to be available"))
    #/expect env (aux-env hash)
      (raise-arguments-error who
        "encountered an unexpected value as the auxiliary environment"
        "aux-env" env)
      env))
  
  (define (syntax-local-implicit-value-maybe stx var)
    (dissect
      (syntax-local-aux-env 'syntax-local-implicit-value-maybe stx)
      (aux-env hash)
    #/hash-ref-maybe hash var))
  
  (define (syntax-local-implicit-value stx var)
    (dissect (syntax-local-aux-env 'syntax-local-implicit-value stx)
      (aux-env hash)
    #/hash-ref hash var #/fn
      (raise-arguments-error 'syntax-local-implicit-value
        (format "unbound implicit variable: ~a" var))))
)
(require #/for-syntax 'private/part-1)
(require 'private/part-1)


(define-syntax (define-empty-aux-env stx)
  (syntax-protect
  #/syntax-parse stx #/ (_)
    #`(define-syntax #,(syntax-local-introduce #/aux-env-id #f)
        (aux-env #/hash))))

(define-for-syntax (let-implicits-fn bindings env)
  (dissect env (aux-env shadowed)
  #/w- bindings
    (list-foldl (hash) bindings #/fn bindings binding
      (dissect binding (list var val)
      #/hash-union bindings (hash var val) #:combine/key
      #/fn var existing new
        ; TODO: See if this should be a `raise-syntax-error`.
        (raise-arguments-error 'let-implicits-with-scopes
          "duplicate implicit variable"
          "var" var)))
  #/aux-env #/hash-union shadowed bindings #:combine #/fn shadowed new
    new))

(define-syntax (let-implicits-with-scopes stx)
  (syntax-protect
  #/syntax-parse stx #/
    (_ ([(~and () scopes) var-expr:expr val:expr] ...) body:expr)
    
    #:with (var-expr-result ...) (generate-temporaries #'(scopes ...))
    #:with (val-result ...) (generate-temporaries #'(scopes ...))
    
    #:with (scope-id ...)
    (list-map (syntax->list #'(scopes ...)) #/fn scope
      (datum->syntax scope '-))
    
  #/dissect
    (list-foldl
      (list (make-immutable-bound-id-table) (list))
      (syntax->list #'((scope-id var-expr-result val-result) ...))
    #/fn state stx
      (dissect state (list seen-table rev-seen-list)
      #/syntax-parse stx #/ (scope-id var-expr-result val-result)
      #/w- seen-rev-entries
        (bound-id-table-ref seen-table #'scope-id (list))
      #/list
        (bound-id-table-set seen-table #'scope-id
          (cons #'(var-expr-result val-result) seen-rev-entries))
        (mat seen-rev-entries (list)
          (cons #'scope-id rev-seen-list)
          rev-seen-list)))
    (list seen-table rev-seen-list)
  #/w- seen-list (reverse rev-seen-list)
  #/with-syntax
    (
      [
        (
          (
            unique-aux-env-id
            ((unique-var-expr-result unique-val-result) ...)
            unique-quoted-aux-env)
          ...)
        (list-map seen-list #/fn scope-id
          (w- aux-env
            ; TODO: Let the `who` argument depend on the macro call
            ; that expands to this one.
            (syntax-local-aux-env 'let-implicits-with-scopes scope-id)
          #/w- seen-rev-entries
            (bound-id-table-ref seen-table scope-id)
          #/list
            (aux-env-id scope-id)
            (reverse seen-rev-entries)
            #`('#,(fn aux-env))))])
    #`(let-syntaxes
        (
          [
            (unique-aux-env-id ...)
            (let ()
              (begin
                (define var-expr-result var-expr)
                (define val-result val))
              ...
              (values
                (let-implicits-fn
                  (list
                    (list unique-var-expr-result unique-val-result)
                    ...)
                  unique-quoted-aux-env)
                ...))])
        body)))

(define-simple-macro
  (let-implicits ([var-expr:expr val:expr] ...) body:expr)
  
  #:with (scopes ...)
  (list-map (syntax->list #'(var-expr ...)) #/fn var-expr
    (datum->syntax var-expr '()))
  
  (let-implicits-with-scopes ([scopes var-expr val] ...)
    body))

(define-simple-macro (let-implicit var-expr:expr val:expr body:expr)
  (let-implicits ([var-expr val])
    body))

; NOTE: In case `let-implicits-with-scopes` is a bit too much to
; understand at once, here's a simple standalone implementation of
; `let-implicit`. The additional logic in `let-implicits-with-scopes`
; has to do with allowing multiple implicit variables to be bound at
; once and respecting the bindings' individual scope sets.
#|
(define-for-syntax (let-implicit-fn var val env)
  (dissect env (aux-env hash)
  #/aux-env #/hash-set hash var val))

(define-syntax (let-implicit stx)
  (syntax-protect
  #/syntax-parse stx #/ (_ var-expr:expr val:expr body:expr)
  #/w- aux-env (syntax-local-aux-env 'let-implicit #'var-expr)
    #`(let-syntax
        (
          [
            #,(aux-env-id stx)
            (let-implicit-fn var-expr val ('#,(fn aux-env)))])
        body)))
|#

(define-syntax (quote-implicit stx)
  (syntax-protect
  #/syntax-parse stx #/ (_ var-expr:expr)
  #/w- aux-env (syntax-local-aux-env 'quote-implicit #'var-expr)
  #/w- var (syntax-local-eval #'var-expr)
  #/expect (syntax-local-implicit-value-maybe stx var) (just val)
    (raise-syntax-error #f
      (format "unbound implicit variable: ~a" var)
      stx
      #'var-expr)
    #`'#,val))

(define (local-implicit-fn stx var)
  (expect (syntax-local-implicit-value-maybe stx var) (just val)
    (raise-arguments-error 'local-implicit
      (format "unbound implicit variable: ~a" var))
    val))

; NOTE: This offers an easy way for the right-hand-sides of the
; `let-implicit...` operations to to refer to existing implicit
; bindings. The expression `(local-implicit _)` is effectively
; shorthand for
; `(just-value #/syntax-local-implicit-value-maybe #'() _)` or
; `(syntax-local-implicit-value #'() _)`.
(define-syntax (local-implicit stx)
  (syntax-protect
  #/syntax-parse stx #/ (_ var-expr:expr)
    #`(local-implicit-fn #'#,(datum->syntax #'var-expr '()) var-expr)))


; TODO: Consider having `eval-implicit`, like `quote-implicit` but
; without the quotation. Maybe we should call it `begin-implicit` or
; something just to avoid the `eval` connotations.

; TODO NOW: Add other features to realize a more complete,
; type-class-like vision, such as the ability to install local
; bindings that execute Turing-complete predicates when looking up a
; variable to find a candidate binding list. If there are two or more
; candidates in the nearest level of scope, that's an error.
