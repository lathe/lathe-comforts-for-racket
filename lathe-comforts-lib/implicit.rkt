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
  [syntax-local-implicit-equals-transformer-part-maybe
    (-> (or/c #f syntax?) any/c maybe?)]
  [syntax-local-implicit-equals-transformer-part
    (-> (or/c #f syntax?) any/c any/c)])
(provide
  define-empty-aux-env
  let-implicit-equals-bindings-with-scopes
  let-implicit-equals-bindings
  let-implicit-equals-transformer-bindings-with-scopes
  let-implicit-equals-transformer-bindings
  let-implicit-equals-transformer-binding
  quote-implicit-equals-transformer-part
  local-implicit-equals-transformer-part
  local-implicit-equals-run-time-parts-as-list)


(module private/part-1 racket/base
  
  (require #/only-in lathe-comforts/struct
    auto-equal auto-write define-imitation-simple-struct)
  
  (require #/only-in lathe-comforts dissect dissectfn expect fn w-)
  (require #/only-in lathe-comforts/hash hash-ref-maybe)
  (require #/only-in lathe-comforts/maybe maybe-map)
  
  
  (provide #/all-defined-out)
  
  
  (define-imitation-simple-struct (aux-env? aux-hash) aux-env
    'aux-env (current-inspector) (auto-write) (auto-equal))
  
  (define-imitation-simple-struct
    (aux-env-equals-entry?
      aux-env-equals-entry-transformer-part
      aux-env-equals-entry-run-time-part-ids)
    aux-env-equals-entry
    'aux-env-equals-entry (current-inspector)
    (auto-write)
    (auto-equal))
  
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
  
  (define (syntax-local-implicit-equals-entry-maybe who stx var)
    (dissect (syntax-local-aux-env who stx) (aux-env hash)
    #/hash-ref-maybe hash var))
  
  (define (syntax-local-implicit-equals-entry who stx var)
    (dissect (syntax-local-aux-env who stx) (aux-env hash)
    #/hash-ref hash var #/fn
      (raise-arguments-error who
        (format "unbound implicit `equals?` variable: ~a" var))))
  
  (define
    (syntax-local-implicit-equals-transformer-part-maybe stx var)
    (maybe-map
      (syntax-local-implicit-equals-entry-maybe
        'syntax-local-implicit-equals-transformer-part-maybe
        stx
        var)
    #/dissectfn
      (aux-env-equals-entry transformer-part run-time-part-ids)
      transformer-part))
  
  (define (syntax-local-implicit-equals-transformer-part stx var)
    (dissect
      (syntax-local-implicit-equals-entry
        'syntax-local-implicit-equals-transformer-part
        stx
        var)
      (aux-env-equals-entry transformer-part run-time-part-ids)
      transformer-part))
  
  (define
    (syntax-local-implicit-equals-run-time-part-ids-maybe stx var)
    (maybe-map
      (syntax-local-implicit-equals-entry-maybe
        'syntax-local-implicit-equals-run-time-part-ids-maybe
        stx
        var)
    #/dissectfn
      (aux-env-equals-entry transformer-part run-time-part-ids)
      run-time-part-ids))
  
  (define (syntax-local-implicit-equals-run-time-part-ids stx var)
    (dissect
      (syntax-local-implicit-equals-entry
        'syntax-local-implicit-equals-run-time-part-ids
        stx
        var)
      (aux-env-equals-entry transformer-part run-time-part-ids)
      run-time-part-ids))
)
(require #/for-syntax 'private/part-1)
(require 'private/part-1)


(define-syntax (define-empty-aux-env stx)
  (syntax-protect
  #/syntax-parse stx #/ (_)
    #`(define-syntax #,(syntax-local-introduce #/aux-env-id #f)
        (aux-env #/hash))))

(define-for-syntax
  (let-implicit-equals-transformer-bindings-fn bindings env)
  (dissect env (aux-env shadowed)
  #/w- bindings
    (list-foldl (hash) bindings #/fn bindings binding
      (dissect binding (list var transformer-part run-time-part-ids)
      #/hash-union bindings
        (hash var
          (aux-env-equals-entry transformer-part run-time-part-ids))
        #:combine/key
      #/fn var existing new
        ; TODO: See if this should be a `raise-syntax-error`.
        (raise-arguments-error
          'let-implicit-equals-transformer-bindings-with-scopes
          "duplicate implicit `equals?` variable"
          "var" var)))
  #/aux-env #/hash-union shadowed bindings #:combine #/fn shadowed new
    new))

(define-syntax (let-implicit-equals-bindings-with-scopes stx)
  (syntax-protect
  #/syntax-parse stx #/
    (_
      (
        [
          (~and () scopes)
          var-expr:expr
          transformer-part:expr
          run-time-part:expr
          ...]
        ...)
      body:expr)
    
    #:with (var-expr-result ...)
    (generate-temporaries #'(var-expr ...))
    
    #:with (transformer-part-result ...)
    (generate-temporaries #'(transformer-part ...))
    
    #:with ((run-time-part-result ...) ...)
    (list-map (syntax->list #'((run-time-part ...) ...))
    #/fn run-time-parts
      (generate-temporaries run-time-parts))
    
    #:with (scope-id ...)
    (list-map (syntax->list #'(scopes ...)) #/fn scope
      (datum->syntax scope '-))
    
  #/dissect
    (list-foldl
      (list (make-immutable-bound-id-table) (list))
      (syntax->list
        #'(
            (
              scope-id
              var-expr-result
              transformer-part-result
              (run-time-part-result ...))
            ...))
    #/fn state stx
      (dissect state (list seen-table rev-seen-list)
      #/syntax-parse stx #/
        (
          scope-id
          var-expr-result
          transformer-part-result
          (run-time-part-result ...))
      #/w- seen-rev-entries
        (bound-id-table-ref seen-table #'scope-id (list))
      #/list
        (bound-id-table-set seen-table #'scope-id
          (cons
            #'(
                var-expr-result
                transformer-part-result
                (run-time-part-result ...))
            seen-rev-entries))
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
            (
              (
                unique-var-expr-result
                unique-transformer-part-result
                (unique-run-time-part-result ...))
              ...)
            unique-quoted-aux-env)
          ...)
        (list-map seen-list #/fn scope-id
          (w- aux-env
            ; TODO: Let the `who` argument depend on the macro call
            ; that expands to this one.
            (syntax-local-aux-env
              'let-implicit-equals-transformer-bindings-with-scopes
              scope-id)
          #/w- seen-rev-entries
            (bound-id-table-ref seen-table scope-id)
          #/list
            (aux-env-id scope-id)
            (reverse seen-rev-entries)
            #`('#,(fn aux-env))))])
    #`(let ([run-time-part-result run-time-part] ... ...)
      #/let-syntaxes
        (
          [
            (unique-aux-env-id ...)
            (let ()
              (begin
                (define var-expr-result var-expr)
                (define transformer-part-result transformer-part))
              ...
              (values
                (let-implicit-equals-transformer-bindings-fn
                  (list
                    (list
                      unique-var-expr-result
                      unique-transformer-part-result
                      (list #'unique-run-time-part-result ...))
                    ...)
                  unique-quoted-aux-env)
                ...))])
        body)))

(define-simple-macro
  (let-implicit-equals-bindings
    ([var-expr:expr transformer-part:expr run-time-part:expr ...] ...)
    body:expr)
  
  #:with (scopes ...)
  (list-map (syntax->list #'(var-expr ...)) #/fn var-expr
    (datum->syntax var-expr '()))
  
  (let-implicit-equals-bindings-with-scopes
    ([scopes var-expr transformer-part run-time-part ...] ...)
    body))

(define-simple-macro
  (let-implicit-equals-transformer-bindings-with-scopes
    ([(~and () scopes) var-expr:expr val:expr] ...)
    body:expr)
  (let-implicit-equals-bindings-with-scopes
    ([scopes var-expr val] ...)
    body))

(define-simple-macro
  (let-implicit-equals-transformer-bindings
    ([var-expr:expr val:expr] ...)
    body:expr)
  
  #:with (scopes ...)
  (list-map (syntax->list #'(var-expr ...)) #/fn var-expr
    (datum->syntax var-expr '()))
  
  (let-implicit-equals-transformer-bindings-with-scopes
    ([scopes var-expr val] ...)
    body))

(define-simple-macro
  (let-implicit-equals-transformer-binding
    var-expr:expr val:expr body:expr)
  (let-implicit-equals-transformer-bindings ([var-expr val])
    body))

; NOTE: In case `let-implicit-equals-transformer-bindings-with-scopes`
; is a bit too much to understand at once, here's a simple standalone
; implementation of `let-implicit-equals-transformer-binding`. The
; additional logic in
; `let-implicit-equals-transformer-bindings-with-scopes` has to do
; with allowing multiple implicit variables to be bound at once and
; respecting the bindings' individual scope sets.
#|
(define-for-syntax
  (let-implicit-equals-transformer-binding-fn var val env)
  (dissect env (aux-env hash)
  #/aux-env #/hash-set hash var #/aux-env-equals-entry val #/list))

(define-syntax (let-implicit-equals-transformer-binding stx)
  (syntax-protect
  #/syntax-parse stx #/ (_ var-expr:expr val:expr body:expr)
  #/w- aux-env
    (syntax-local-aux-env 'let-implicit-equals-transformer-binding
      #'var-expr)
    #`(let-syntax
        (
          [
            #,(aux-env-id stx)
            (let-implicit-equals-transformer-binding-fn var-expr val
              ('#,(fn aux-env)))])
        body)))
|#

(define-syntax (quote-implicit-equals-transformer-part stx)
  (syntax-protect
  #/syntax-parse stx #/ (_ var-expr:expr)
  #/w- aux-env
    (syntax-local-aux-env 'quote-implicit-equals-transformer-part
      #'var-expr)
  #/w- var (syntax-local-eval #'var-expr)
  #/expect
    (syntax-local-implicit-equals-transformer-part-maybe stx var)
    (just transformer-part)
    (raise-syntax-error #f
      (format "unbound implicit `equals?` variable: ~a" var)
      stx
      #'var-expr)
    #`'#,transformer-part))

(define (local-implicit-equals-transformer-part-fn stx var)
  (expect
    (syntax-local-implicit-equals-transformer-part-maybe stx var)
    (just transformer-part)
    (raise-arguments-error 'local-implicit-equals-transformer-part
      (format "unbound implicit `equals?` variable: ~a" var))
    transformer-part))

; NOTE: This offers an easy way for the right-hand-sides of the
; `let-implicit-equals-transformer-binding...` operations to to refer
; to existing implicit `equals?` transformer binding. The expression
; `(local-implicit-equals-transformer-part _)` is effectively
; shorthand for
; `(just-value #/syntax-local-implicit-equals-transformer-part-maybe #'() _)`
; or `(syntax-local-implicit-equals-transformer-part #'() _)`.
(define-syntax (local-implicit-equals-transformer-part stx)
  (syntax-protect
  #/syntax-parse stx #/ (_ var-expr:expr)
    #`(local-implicit-equals-transformer-part-fn
        #'#,(datum->syntax #'var-expr '())
        var-expr)))

(define-syntax (local-implicit-equals-run-time-parts-as-list stx)
  (syntax-protect
  #/syntax-parse stx #/ (_ var-expr:expr)
  #/w- aux-env
    (syntax-local-aux-env
      'local-implicit-equals-run-time-parts-as-list
      #'var-expr)
  #/w- var (syntax-local-eval #'var-expr)
  #/expect
    (syntax-local-implicit-equals-run-time-part-ids-maybe stx var)
    (just run-time-part-ids)
    (raise-syntax-error #f
      (format "unbound implicit `equals?` variable: ~a" var)
      stx
      #'var-expr)
    #`(list #,@run-time-part-ids)))

(define (local-implicit-equals-run-time-part-ids-fn stx var)
  (expect
    (syntax-local-implicit-equals-run-time-part-ids-maybe stx var)
    (just run-time-part-ids)
    (raise-arguments-error 'local-implicit-equals-run-time-part-ids
      (format "unbound implicit `equals?` variable: ~a" var))
    run-time-part-ids))

; TODO: Make a variant of `let-implicit-equals-bindings-with-scopes`
; that takes a phase-1 expression and uses it to generate the list of
; run-time part expressions, so that we can use it with
; `local-implicit-equals-run-time-part-ids` to bind an implicit
; `equals?` binding under a different name.
;
; NOTE: The run-time parts of implicit `equals?` bindings are mostly
; just to show we can associate an implicit binding with run-time
; content. Other families of implicit bindings (rather than the
; `equals?` family we have now) will likely make more interesting use
; of run-time parts. (TODO: Once we have those families, update this
; comment.)
;
(define-syntax (local-implicit-equals-run-time-part-ids stx)
  (syntax-protect
  #/syntax-parse stx #/ (_ var-expr:expr)
    #`(local-implicit-equals-run-time-part-ids-fn
        #'#,(datum->syntax #'var-expr '())
        var-expr)))


; TODO NOW: Add other features to realize a more complete,
; type-class-like vision, such as the ability to install local
; bindings that execute Turing-complete predicates when looking up a
; variable to find a candidate binding list. If there are two or more
; candidates in the nearest level of scope, that's an error.
