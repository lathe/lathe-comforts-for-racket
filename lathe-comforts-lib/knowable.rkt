#lang parendown/slash racket/base

; lathe-comforts/knowable
;
; A pair of types, `unknown` and `known`, intended for values
; that may or may not have been established yet in the contemporary
; state of the library ecosystem.

;   Copyright 2024 The Lathe Authors
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

(require lathe-comforts)
(require lathe-comforts/list)
(require lathe-comforts/match)
(require lathe-comforts/struct)


(provide /own-contract-out
  expressly-possibly-unknown-impl?
  prop:expressly-possibly-unknown
  make-expressly-possibly-unknown-impl
  unknown?
  example-unknown?)
(provide
  example-unknown)
(provide /own-contract-out
  unknown
  known?
  known-value)
(provide
  known)
(provide /own-contract-out
  knowable?
  knowable/c
  knowable-bind
  knowable-map
  knowable-if
  knowable-zip*
  knowable->falsable
  falsable->uninformative-knowable
  boolean-and-knowable-promise-zip*
  boolean-and-knowable-thunk-zip*
  boolean-or-knowable-thunk-zip*
  expressly-knowable-predicate-impl?
  prop:expressly-knowable-predicate
  make-expressly-knowable-predicate-impl
  get-accepts?-knowable
  accepts?-knowable
  make-procedure-impl-for-knowable-predicate
  make-procedure-impl-for-knowable-predicate-with-arity-of-procedure
  makeshift-knowable-predicate)


(define-imitation-simple-generics
  expressly-possibly-unknown? expressly-possibly-unknown-impl?
  (#:method expressly-possibly-unknown-is-indeed? (#:this))
  prop:expressly-possibly-unknown make-expressly-possibly-unknown-impl
  'expressly-possibly-unknown 'expressly-possibly-unknown-impl (list))
(ascribe-own-contract expressly-possibly-unknown-impl?
  (-> any/c boolean?))
(ascribe-own-contract prop:expressly-possibly-unknown
  (struct-type-property/c expressly-possibly-unknown-impl?))
(ascribe-own-contract make-expressly-possibly-unknown-impl
  (-> (-> any/c boolean?) expressly-possibly-unknown-impl?))


(define/own-contract (unknown? v)
  (-> any/c boolean?)
  (and
    (expressly-possibly-unknown? v)
    (expressly-possibly-unknown-is-indeed? v)))

(define-imitation-simple-struct (example-unknown?) example-unknown
  'unknown (current-inspector) (auto-write)
  (#:prop prop:expressly-possibly-unknown
    (make-expressly-possibly-unknown-impl /fn self
      #t)))
(ascribe-own-contract example-unknown? (-> any/c boolean?))

(define/own-contract (unknown)
  (-> example-unknown?)
  (example-unknown))

(define-imitation-simple-struct (known? known-value) known
  'known (current-inspector) (auto-write) (auto-equal))
(ascribe-own-contract known? (-> any/c boolean?))
(ascribe-own-contract known-value (-> known? any/c))

(define/own-contract (knowable? v)
  (-> any/c boolean?)
  (or (known? v) (unknown? v)))

; TODO: Give the resulting contract a better name, check that it has
; good `contract-stronger?` behavior, etc.
(define/own-contract (knowable/c c)
  (-> contract? contract?)
  (w- c (coerce-contract 'knowable/c c)
  /rename-contract (or/c unknown? /match/c known c)
    `(knowable/c ,(contract-name c))))

(define/own-contract (knowable-bind kble func)
  (-> knowable? (-> any/c knowable?) knowable?)
  (expect kble (known value) kble
  /func value))

(define/own-contract (knowable-map kble func)
  (-> knowable? (-> any/c any/c) knowable?)
  (knowable-bind kble /fn value
  /known /func value))

(define/own-contract (knowable-if condition get-value)
  (-> boolean? (-> any/c) knowable?)
  (if condition
    (known /get-value)
    (unknown)))

(define/own-contract (knowable-zip* knowable-list)
  (-> (listof knowable?) (knowable/c list?))
  (expect knowable-list (cons knowable knowable-list) (known /list)
  /knowable-bind knowable /fn element
  /knowable-map (knowable-zip* knowable-list) /fn element-list
    (cons element element-list)))

(define/own-contract (knowable->falsable kble)
  (-> knowable? any/c)
  (mat kble (known value)
    value
    #f))

(define/own-contract (falsable->uninformative-knowable fble)
  (-> any/c knowable?)
  (knowable-if fble /fn fble))

(define/own-contract (boolean-and-knowable-promise-zip* kp-list)
  (-> (listof (promise/c (knowable/c boolean?)))
    (promise/c (knowable/c boolean?)))
  (delay
    (if
      (list-any kp-list /fn kp
        (mat (force kp) (known #f) #t #f))
      (known #f)
    /knowable-if (list-all kp-list /fn kp /known? /force kp) /fn #t)))

(define/own-contract (boolean-and-knowable-thunk-zip* kble-thunk-list)
  (-> (listof (-> (knowable/c boolean?))) (knowable/c boolean?))
  (force /boolean-and-knowable-promise-zip*
    (list-map kble-thunk-list /fn kble-thunk /delay /kble-thunk)))

(define/own-contract (boolean-or-knowable-thunk-zip* kble-thunk-list)
  (-> (listof (-> (knowable/c boolean?))) (knowable/c boolean?))
  (w- boolean-knowable-not
    (fn k
      (knowable-map k /fn result /not result))
  /boolean-knowable-not /boolean-and-knowable-thunk-zip*
    (list-map kble-thunk-list /fn kble-thunk
      (fn /boolean-knowable-not /kble-thunk))))


(define-imitation-simple-generics
  expressly-knowable-predicate? expressly-knowable-predicate-impl?
  (#:method expressly-knowable-predicate-get-accepts?-knowable
    (#:this))
  prop:expressly-knowable-predicate
  make-expressly-knowable-predicate-impl
  'expressly-knowable-predicate
  'expressly-knowable-predicate-impl (list))
(ascribe-own-contract expressly-knowable-predicate-impl?
  (-> any/c boolean?))
(ascribe-own-contract prop:expressly-knowable-predicate
  (struct-type-property/c expressly-knowable-predicate-impl?))
(ascribe-own-contract make-expressly-knowable-predicate-impl
  (-> (-> any/c (unconstrained-domain-> knowable?))
    expressly-knowable-predicate-impl?))

(define/own-contract (get-accepts?-knowable f)
  (-> procedure? (unconstrained-domain-> knowable?))
  (if (expressly-knowable-predicate? f)
    (expressly-knowable-predicate-get-accepts?-knowable f)
  /let-values ([(required-kws allowed-kws) (procedure-keywords p)])
  /procedure-reduce-keyword-arity-mask
    (make-keyword-procedure
      (lambda (ks vs . positional-args)
        (falsable->uninformative-knowable
          (keyword-apply f ks vs positional-args)))
      (lambda positional-args
        (falsable->uninformative-knowable /apply f positional-args)))
    (procedure-arity-mask f)
    required-kws
    allowed-kws))

(define (accepts?-knowable/get-accepts?-knowable f)
  (unless (procedure? f)
    (raise-arguments-error 'accepts?-knowable
      "expected the called value to be a procedure"
      "f" f))
  (get-accepts?-knowable f))

(define/own-contract accepts?-knowable
  (unconstrained-domain-> knowable?)
  (procedure-reduce-arity
    (make-keyword-procedure
      (lambda (ks vs f . positional-args)
        (w- accepts?-knowable
          (accepts?-knowable/get-accepts?-knowable f)
        /keyword-apply accepts?-knowable ks vs positional-args))
      (lambda (f . positional-args)
        (w- accepts?-knowable
          (accepts?-knowable/get-accepts?-knowable f)
        /apply accepts?-knowable positional-args)))
    (arity-at-least 1)))

; Returns a value that makes an appropriate `prop:procedure`
; implementation for a structure type that implements
; `prop:expressly-knowable-predicate`. It will often be preferable to
; pass this result through `procedure-reduce-arity`.
;
(define/own-contract (make-procedure-impl-for-knowable-predicate)
  (-> (unconstrained-domain-> any/c))
  (compose knowable->falsable accepts?-knowable))

(define/own-contract
  (make-procedure-impl-for-knowable-predicate-with-arity-of-procedure
    p)
  (-> procedure? (unconstrained-domain-> any/c))
  (define-values (required-kws allowed-kws) (procedure-keywords p))
  (procedure-reduce-keyword-arity-mask
    (make-procedure-impl-for-knowable-predicate)
    (arithmetic-shift (procedure-arity-mask p) 1)
    required-kws
    allowed-kws))

(define makeshift-knowable-predicate-inspector (current-inspector))

(define/own-contract (makeshift-knowable-predicate accepts?-knowable)
  (-> (unconstrained-domain-> knowable?)
    (unconstrained-domain-> any/c))
  (define-imitation-simple-struct
    (makeshift-knowable-predicate?
      makeshift-knowable-predicate-get-accepts?-knowable)
    makeshift-knowable-predicate
    'makeshift-knowable-predicate
    makeshift-knowable-predicate-inspector
    (auto-write)
    (#:prop prop:procedure
      (make-procedure-impl-for-knowable-predicate-with-arity-of-procedure
        accepts?-knowable))
    (#:prop prop:expressly-knowable-predicate
      (make-expressly-knowable-predicate-impl
        (dissectfn (makeshift-knowable-predicate accepts?-knowable)
          accepts?-knowable))))
  (makeshift-knowable-predicate accepts?-knowable))
