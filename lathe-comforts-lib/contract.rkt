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
  -> any/c contract? contract-name contract-out flat-contract?
  recursive-contract rename-contract)
(require #/only-in racket/contract/combinator
  blame-add-context contract-first-order-passes? make-contract
  raise-blame-error)
(require #/only-in syntax/parse/define define-simple-macro)

(require #/only-in lathe-comforts dissectfn expect expectfn fn w-)

(provide let/c fix/c by-own-method/c)
(provide #/contract-out
  [equal/c (-> any/c flat-contract?)])


(define (let/c-impl var-exprs body-expr vals body)
  (define result
    (body #/for/list ([var-expr var-exprs] [val vals])
      (rename-contract val var-expr)))
  (rename-contract result
    `(let/c
       ,@(for/list ([var-expr var-exprs] [val vals])
           `([,var-expr ,(contract-name val)]))
       ,(contract-name result))))

(define-syntax (let/c stx)
  (syntax-protect #/syntax-parse stx #/ (_ [var:id val] ... body)
    
    #:declare val
    (expr/c #'contract? #:name "one of the val arguments")
    
    #:declare body (expr/c #'contract? #:name "body result")
    
    #'(let/c-impl (list 'var ...) 'body (list val.c ...)
      #/dissectfn (list var ...)
        body.c)))


; NOTE: This takes the same options `recursive-contract` does, and it
; passes them along unmodified.
(define-simple-macro (fix/c var:id options ... contract)
  #:declare contract (expr/c #'contract? #:name "contract argument")
  (let ()
    (define var
      (w- var
        (rename-contract (recursive-contract var options ...) 'var)
      #/w- contract-result contract.c
        (rename-contract contract-result
          `(fix/c var ,(contract-name contract-result)))))
    var))


(define
  (by-own-method/c-impl pat-expr body-expr body)
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
  (rename-contract (fn v #/equal? example v) `(equal/c ,example)))
