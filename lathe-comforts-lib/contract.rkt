#lang parendown racket/base

; lathe-comforts/contract
;
; Utilities for contracts.

;   Copyright 2017-2022, 2025 The Lathe Authors
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


(require #/for-syntax #/only-in lathe-comforts/syntax
  ~autoptic ~autoptic-list)

(require lathe-comforts/private/shim)
(init-shim)

(require #/only-in lathe-comforts
  dissect dissectfn expect expectfn fn mat w-)
(require #/only-in lathe-comforts/struct
  auto-equal auto-write define-imitation-simple-struct)

(provide
  impersonator-obstinacy)
(provide #/own-contract-out
  impersonator-obstinacy?)
(provide
  chaperone-obstinacy)
(provide #/own-contract-out
  chaperone-obstinacy?)
(provide
  flat-obstinacy)
(provide #/own-contract-out
  flat-obstinacy?
  obstinacy?
  obstinacy-contract/c
  obstinacy-get-make-contract
  obstinacy-get-coerce-contract-for-id
  value-name-for-contract)
(provide
  let/c
  fix/c
  by-own-method/c)
(provide #/own-contract-out
  equal/c
  flat-contract-accepting/c)


(define-imitation-simple-struct
  (impersonator-obstinacy?)
  impersonator-obstinacy
  'impersonator-obstinacy (current-inspector)
  (auto-write)
  (auto-equal))
(ascribe-own-contract impersonator-obstinacy? (-> any/c boolean?))
(define-imitation-simple-struct
  (chaperone-obstinacy?)
  chaperone-obstinacy
  'chaperone-obstinacy (current-inspector)
  (auto-write)
  (auto-equal))
(ascribe-own-contract chaperone-obstinacy? (-> any/c boolean?))
(define-imitation-simple-struct
  (flat-obstinacy?)
  flat-obstinacy
  'flat-obstinacy (current-inspector)
  (auto-write)
  (auto-equal))
(ascribe-own-contract flat-obstinacy? (-> any/c boolean?))

(define/own-contract (obstinacy? v)
  (-> any/c boolean?)
  (or
    (impersonator-obstinacy? v)
    (chaperone-obstinacy? v)
    (flat-obstinacy? v)))

(define/own-contract (obstinacy-contract/c ob)
  (-> obstinacy? flat-contract?)
  (mat ob (impersonator-obstinacy) contract?
  #/mat ob (chaperone-obstinacy) chaperone-contract?
  #/dissect ob (flat-obstinacy) flat-contract?))

; TODO: See if we should use `begin-suggest-inline` for this. Would it
; help at all?
(define/own-contract (obstinacy-get-make-contract ob)
  (-> obstinacy? procedure?)
  (mat ob (impersonator-obstinacy) make-contract
  #/mat ob (chaperone-obstinacy) make-chaperone-contract
  #/dissect ob (flat-obstinacy) make-flat-contract))

(define/own-contract (obstinacy-get-coerce-contract-for-id ob id)
  (->i ([ob obstinacy?] [id symbol?])
    [_ (ob) (-> any/c #/obstinacy-contract/c ob)])
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


(define/own-contract (value-name-for-contract v)
  (-> any/c any/c)
  (if (contract? v)
    (contract-name v)
    v))


(define-syntax-parse-rule/autoptic
  (let/c
    {~autoptic-list
      [ var:id
        {~var val (expr/c #'contract #:name "binding value")}]}
    ...
    {~var body (expr/c #'contract? #:name "body result")})
  (let ([var val] ...)
    (rename-contract body.c
      `(let/c [var ,(value-name-for-contract var)] ...
          body))))


; TODO: See if we'll use this, probably in an upgraded `fix/c`.
(define
  (make-recursive-contract
    obstinacy extra-delay? list? contract-promise)
  (w- p contract-promise
  #/mat obstinacy (flat-obstinacy)
    (if extra-delay?
      (if list?
        (recursive-contract (force p)
          #:flat #:extra-delay #:list-contract?)
        (recursive-contract (force p) #:flat #:extra-delay))
      (if list?
        (recursive-contract (force p) #:flat #:list-contract?)
        (recursive-contract (force p) #:flat)))
  #/mat obstinacy (chaperone-obstinacy)
    (if list?
      (recursive-contract (force p) #:chaperone #:list-contract?)
      (recursive-contract (force p) #:chaperone))
  #/dissect obstinacy (impersonator-obstinacy)
    (if list?
      (recursive-contract (force p) #:impersonator #:list-contract?)
      (recursive-contract (force p) #:impersonator))))

; TODO: Let this pass in the extra options of
; `make-recursive-contract`.
;
(define-syntax-parser fix/c
  [
    {~autoptic-list
      (fix/c var:id
        {~var contract
          (expr/c #'contract? #:name "contract argument")})}
    #'(let ()
        (define var
          (w- var (rename-contract (recursive-contract var) 'var)
          #/rename-contract contract.c `(fix/c var contract)))
        var)]
  [
    {~autoptic-list
      (fix/c
        {~autoptic-list
          (var:id {~autoptic-list [arg-var:id arg-val:expr]} ...)}
        {~var contract
          (expr/c #'contract? #:name "contract argument")})}
    #'(let ()
        (define var
          (lambda (arg-var ...)
            (w- var
              (lambda (arg-var ...)
                (rename-contract
                  (recursive-contract #/var arg-var ...)
                  `(var ,(contract-name-or-value arg-var) ...)))
            #/rename-contract contract.c
              `(fix/c
                  (var
                    [arg-var ,(contract-name-or-value arg-var)]
                    ...)
                  contract))))
        (var arg-val ...))])


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

(define-syntax-parse-rule/autoptic
  (by-own-method/c
    (~optional
      (~seq {~autoptic #:obstinacy}
        {~var ob (expr/c #'obstinacy? #:name "obstinacy argument")})
      #:defaults ([ob.c #'(impersonator-obstinacy)]))
    pat:expr
    {~var body
      (expr/c #'(obstinacy-contract/c ob-c) #:name "body result")})
  (w- ob-c ob.c
  #/by-own-method/c-impl ob-c '(by-own-method/c pat body)
    (expectfn pat (list)
      (list body.c))))


(define/own-contract (equal/c example)
  (-> any/c flat-contract?)
  (rename-contract (fn v #/equal? example v)
    `(equal/c ,(value-name-for-contract example))))

(define/own-contract (equal-always/c example)
  (-> any/c flat-contract?)
  (rename-contract (fn v #/equal-always? example v)
    `(equal-always/c ,(value-name-for-contract example))))


(define/own-contract (flat-contract-accepting/c v)
  (-> any/c flat-contract?)
  (rename-contract
    (and/c flat-contract? (fn c #/contract-first-order-passes? c v))
    `(flat-contract-accepting/c ,(value-name-for-contract v))))
