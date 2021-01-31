#lang parendown racket/base

; lathe-comforts/contract
;
; Utilities for contracts.

;   Copyright 2017-2020 The Lathe Authors
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
  expr expr/c id ~optional ~seq syntax-parse)

; NOTE: The Racket documentation says `get/build-late-neg-projection`
; is in `racket/contract/combinator`, but it isn't. It's in
; `racket/contract/base`. Since it's also in `racket/contract` and the
; documentation correctly says it is, we require it from there.
(require #/only-in racket/contract get/build-late-neg-projection)
(require #/only-in racket/contract/base
  -> ->i and/c any/c chaperone-contract? contract? contract-name
  contract-out flat-contract? recursive-contract rename-contract)
(require #/only-in racket/contract/combinator
  blame-add-context coerce-chaperone-contract coerce-contract
  coerce-flat-contract contract-first-order-passes?
  make-chaperone-contract make-contract make-flat-contract
  raise-blame-error)
(require #/only-in syntax/parse/define define-simple-macro)

(require #/only-in lathe-comforts
  dissect dissectfn expect expectfn fn mat w-)
(require #/only-in lathe-comforts/struct
  auto-equal auto-write define-imitation-simple-struct)

(provide
  impersonator-obstinacy)
(provide #/contract-out
  [impersonator-obstinacy? (-> any/c boolean?)])
(provide
  chaperone-obstinacy)
(provide #/contract-out
  [chaperone-obstinacy? (-> any/c boolean?)])
(provide
  flat-obstinacy)
(provide #/contract-out
  [flat-obstinacy? (-> any/c boolean?)]
  [obstinacy? (-> any/c boolean?)]
  [obstinacy-contract/c (-> obstinacy? flat-contract?)]
  [obstinacy-get-make-contract (-> obstinacy? procedure?)]
  [obstinacy-get-coerce-contract-for-id
    (->i ([ob obstinacy?] [id symbol?])
      [_ (ob) (-> any/c #/obstinacy-contract/c ob)])]
  [value-name-for-contract (-> any/c any/c)])
(provide
  let/c
  fix/c
  by-own-method/c)
(provide #/contract-out
  [equal/c (-> any/c flat-contract?)]
  [flat-contract-accepting/c (-> any/c flat-contract?)])


(define-imitation-simple-struct
  (impersonator-obstinacy?)
  impersonator-obstinacy
  'impersonator-obstinacy (current-inspector)
  (auto-write)
  (auto-equal))
(define-imitation-simple-struct
  (chaperone-obstinacy?)
  chaperone-obstinacy
  'chaperone-obstinacy (current-inspector)
  (auto-write)
  (auto-equal))
(define-imitation-simple-struct
  (flat-obstinacy?)
  flat-obstinacy
  'flat-obstinacy (current-inspector)
  (auto-write)
  (auto-equal))

(define (obstinacy? v)
  (or
    (impersonator-obstinacy? v)
    (chaperone-obstinacy? v)
    (flat-obstinacy? v)))

(define (obstinacy-contract/c ob)
  (mat ob (impersonator-obstinacy) contract?
  #/mat ob (chaperone-obstinacy) chaperone-contract?
  #/dissect ob (flat-obstinacy) flat-contract?))

; TODO: See if we should use `begin-suggest-inline` for this. Would it
; help at all?
(define (obstinacy-get-make-contract ob)
  (mat ob (impersonator-obstinacy) make-contract
  #/mat ob (chaperone-obstinacy) make-chaperone-contract
  #/dissect ob (flat-obstinacy) make-flat-contract))

(define (obstinacy-get-coerce-contract-for-id ob id)
  (mat ob (impersonator-obstinacy) (fn c #/coerce-contract id c)
  #/mat ob (chaperone-obstinacy)
    (fn c #/coerce-chaperone-contract id c)
  #/dissect ob (flat-obstinacy) (fn c #/coerce-flat-contract id c)))

; TODO: See if we should export this. It may just be an implementation
; detail of `obstinacy-late-contract-projector`.
(define
  (obstinacy-project-late ob project-v-and-late-party v late-party)
  (w- next-v (project-v-and-late-party v late-party)
  #/mat ob (impersonator-obstinacy) next-v
  #/mat ob (chaperone-obstinacy) next-v
  #/dissect ob (flat-obstinacy) v))

; TODO: See if we should export this. It doesn't seem as universally
; useful as `obstinacy-project-late`, since usually the contract `c`
; is known before `missing-party` is.
(define
  (obstinacy-late-contract-projector ob coerce blame missing-party)
  (fn c v context
    (w- c-proj
      (
        (get/build-late-neg-projection #/coerce c)
        (blame-add-context blame context))
    #/obstinacy-project-late ob c-proj v missing-party)))


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


(define (by-own-method/c-impl ob name body)
  (w- coerce
    (obstinacy-get-coerce-contract-for-id ob 'by-own-method/c)
  #/w- first-order
    (fn v
      (expect (body v) (list c) #f
      #/contract-first-order-passes? c v))
  #/ (obstinacy-get-make-contract ob)
    
    #:name name
    #:first-order first-order
    
    #:late-neg-projection
    (fn blame
      (fn v missing-party
        (w- project-late-contract
          (obstinacy-late-contract-projector
            ob coerce blame missing-party)
        #/expect (body v) (list c)
          (raise-blame-error blame #:missing-party missing-party v
            '(expected: "~e" given: "~e")
            name v)
        #/project-late-contract c v "the body of")))))

(define-syntax (by-own-method/c stx)
  (syntax-protect #/syntax-parse stx #/
    (_
      (~optional (~seq #:obstinacy ob)
        #:defaults ([ob #'(impersonator-obstinacy)]))
      pat:expr
      body)
    
    #:declare ob
    (expr/c #'obstinacy? #:name "obstinacy argument")
    
    #:declare body
    (expr/c #'(obstinacy-contract/c ob-c) #:name "body result")
    
    #'(w- ob-c ob.c
      #/by-own-method/c-impl ob-c '(by-own-method/c pat body)
        (expectfn pat (list)
          (list body.c)))))


(define (equal/c example)
  (rename-contract (fn v #/equal? example v)
    `(equal/c ,(value-name-for-contract example))))


(define (flat-contract-accepting/c v)
  (rename-contract
    (and/c flat-contract? (fn c #/contract-first-order-passes? c v))
    `(flat-contract-accepting/c ,(value-name-for-contract v))))
