#lang parendown racket/base

; lathe-comforts/contract
;
; Utilities for contracts.

;   Copyright 2017-2018 The Lathe Authors
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
(require #/for-syntax #/only-in syntax/parse
  expr expr/c id syntax-parse)

; NOTE: The Racket documentation says `get/build-late-neg-projection`
; is in `racket/contract/combinator`, but it isn't. It's in
; `racket/contract/base`. Since it's also in `racket/contract` and the
; documentation correctly says it is, we require it from there.
(require #/only-in racket/contract get/build-late-neg-projection)
(require #/only-in racket/contract/base
  -> and/c any/c chaperone-contract? contract? contract-name
  contract-out flat-contract? list-contract? recursive-contract
  rename-contract)
(require #/only-in racket/contract/combinator
  blame-add-context blame-swap coerce-contract
  contract-first-order-passes? contract-equivalent? contract-stronger?
  make-chaperone-contract make-contract make-flat-contract
  raise-blame-error)
(require #/only-in syntax/parse/define define-simple-macro)

(require #/only-in lathe-comforts dissectfn expect expectfn fn w-)

(provide #/contract-out
  [value-name-for-contract (-> any/c any/c)])
(provide
  let/c
  fix/c
  by-own-method/c)
(provide #/contract-out
  [equal/c (-> any/c flat-contract?)]
  [swap/c (-> contract? contract?)]
  [flat-contract-accepting/c (-> any/c flat-contract?)])


(define (value-name-for-contract v)
  (if (contract? v)
    (contract-name v)
    v))


(define-syntax (let/c stx)
  (syntax-protect #/syntax-parse stx #/ (_ [var:id val] ... body)
    
    #:declare body (expr/c #'contract? #:name "body result")
    
    #'(let ([var val] ...)
        (rename-contract body.c
          `(let/c [var ,(value-name-for-contract var)] ...
             body)))))


; NOTE: This takes the same options `recursive-contract` does, and it
; passes them along unmodified.
(define-syntax (fix/c stx)
  (syntax-protect #/syntax-parse stx
    [
      (fix/c var:id options ... contract)
      
      #:declare contract
      (expr/c #'contract? #:name "contract argument")
      
      #'(let ()
          (define var
            (w- var
              (rename-contract (recursive-contract var options ...)
                'var)
            #/rename-contract contract.c
              `(fix/c var options ... contract)))
          var)]
    [
      (fix/c (var:id [arg-var:id arg-val:expr] ...) options ...
        contract)
      
      #:declare contract
      (expr/c #'contract? #:name "contract argument")
      
      #'(let ()
          (define var
            (lambda (arg-var ...)
              (w- var
                (lambda (arg-var ...)
                  (rename-contract
                    (recursive-contract (var arg-var ...) options ...)
                    `(var ,(contract-name-or-value arg-var) ...)))
              #/rename-contract contract.c
                `(fix/c
                  (var [arg-var ,(contract-name-or-value arg-var)] ...)
                  options ...
                  contract))))
          (var arg-val ...))]))


(define (by-own-method/c-impl pat-expr body-expr body)
  (w- name `(by-own-method/c ,pat-expr ,body-expr)
  #/w- first-order
    (fn v
      (expect (body v) (list c) #f
      #/contract-first-order-passes? c v))
  #/make-contract #:name name #:first-order first-order
    
    #:late-neg-projection
    (fn blame
      (fn v missing-party
        (expect (body v) (list c)
          (raise-blame-error blame #:missing-party missing-party v
            '(expected: "~e" given: "~e")
            name v)
        #/
          (
            (get/build-late-neg-projection c)
            (blame-add-context blame "the body of"))
          v missing-party)))))

(define-syntax (by-own-method/c stx)
  (syntax-protect #/syntax-parse stx #/ (_ pat:expr body)
    #:declare body (expr/c #'contract? #:name "body result")
    #'(by-own-method/c-impl 'pat 'body
        (expectfn pat (list)
          (list body.c)))))


(define (equal/c example)
  (rename-contract (fn v #/equal? example v)
    `(equal/c ,(value-name-for-contract example))))


(define (swap/c c)
  (w- c (coerce-contract 'swap/c c)
  #/w- name `(swap/c ,(contract-name c))
  #/w- c-late-neg-projection (get/build-late-neg-projection c)
  #/
    (if (flat-contract? c) make-flat-contract
      (if (chaperone-contract? c) make-chaperone-contract
        make-contract))
    #:is-list-contract? (list-contract? c)
    #:name name
    #:stronger (fn self other #/contract-stronger? c other)
    #:equivalent (fn self other #/contract-equivalent? c other)
    
    #:first-order (fn v #/contract-first-order-passes? c v)
    
    #:late-neg-projection
    (fn blame #/c-late-neg-projection #/blame-swap blame)))


(define (flat-contract-accepting/c v)
  (rename-contract
    (and/c flat-contract? (fn c #/contract-first-order-passes? c v))
    `(flat-contract-accepting/c ,(value-name-for-contract v))))
