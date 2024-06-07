#lang parendown/slash racket/base

; lathe-comforts/private/smoosh
;
; An object system that allows for partial information.

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

(require /only-in lathe-comforts
  dissect dissectfn expect fn mat w- w-loop)
(require /only-in lathe-comforts/hash
  hash-ref-maybe hash-set-maybe hash-v-map make-similar-hash)
(require /only-in lathe-comforts/list
  list-all list-any list-foldl list-length=nat? list-map list-zip-map)
(require /only-in lathe-comforts/match match/c)
(require /only-in lathe-comforts/maybe
  just just? just-value maybe? maybe-bind maybe/c maybe-if maybe-map
  nothing nothing?)
(require /only-in lathe-comforts/string immutable-string?)
(require /only-in lathe-comforts/struct
  auto-equal auto-write define-imitation-simple-generics
  define-imitation-simple-struct immutable-prefab-struct?
  mutable-prefab-struct?)
(require /only-in lathe-comforts/trivial trivial trivial?)


(provide /own-contract-out
  expressly-possibly-unknown-impl?
  prop:expressly-possibly-unknown
  make-expressly-possibly-unknown-impl
  unknown?
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
  knowable->falsable
  falsable->uninformative-knowable
  expressly-knowable-predicate-impl?
  prop:expressly-knowable-predicate
  make-expressly-knowable-predicate-impl
  call-knowable
  make-procedure-impl-for-knowable-predicate
  make-procedure-impl-for-knowable-predicate-with-arity-of-procedure
  makeshift-knowable-predicate
  glossesque-sys?
  glossesque-sys-impl?
  glossesque-sys-glossesque-union-of-zero
  glossesque-sys-glossesque-skm-union-of-two-knowable
  glossesque-sys-glossesque-ref-maybe-knowable
  glossesque-sys-rider-and-glossesque-update-maybe-knowable
  glossesque-sys-glossesque-empty?
  glossesque-sys-glossesque-count
  glossesque-sys-glossesque-iteration-sequence
  prop:glossesque-sys
  glossesque-sys-glossesque-set-maybe-knowable
  ; TODO: When we document this, make sure we document its keyword
  ; arguments.
  make-glossesque-sys-impl
  glossesque-sys-map-glossesque
  glossesque-sys-map-key
  tagged-glossesque-sys?
  tagged-glossesque-sys-get-inhabitant?
  tagged-glossesque-sys-get-glossesque-sys)
(provide
  tagged-glossesque-sys)
(provide /own-contract-out
  custom-gloss-key-report?
  custom-gloss-key-report-impl?
  custom-gloss-key-report-get-==-tagged-glossesque-sys-knowable
  custom-gloss-key-report-get-path-related-tagged-glossesque-sys-knowable
  prop:custom-gloss-key-report
  make-custom-gloss-key-report-impl
  uninformative-custom-gloss-key-report
  uninformative-custom-gloss-key-reports
  custom-gloss-key-report-map
  custom-gloss-key-reports-map
  custom-gloss-key-report-zip*-map
  custom-gloss-key-reports-zip*-map
  constant-custom-gloss-key-report
  constant-custom-gloss-key-reports
  sequence-first
  path-related-wrapper?
  path-related-wrapper-value)
(provide
  path-related-wrapper)
(provide /own-contract-out
  info-wrapper?
  info-wrapper-value)
(provide
  info-wrapper)
(provide /own-contract-out
  expressly-custom-gloss-key-dynamic-type-impl?
  prop:expressly-custom-gloss-key-dynamic-type
  make-expressly-custom-gloss-key-dynamic-type-impl
  dynamic-type-get-custom-gloss-key-reports
  get-dynamic-type-with-default-bindings
  knowable-zip*
  maybe-min-zip*
  promise-zip*-map
  knowable-promise-zip*-map
  boolean-and-knowable-promise-zip*
  boolean-and-knowable-thunk-zip*
  boolean-or-knowable-thunk-zip*
  maybe-min-knowable-promise-zip*-map
  sequence-zip*-map
  knowable-promise-or
  gloss?
  list-map-foldl-knowable
  rider-and-hash-update-maybe-knowable
  hash-skv-map-maybe-knowable
  hash-skm-union-of-two-knowable
  rider-and-maybe-update-knowable
  count-and-rider-and-maybe-update-maybe-knowable
  maybe-or-sm-union-of-two-knowable
  maybe-left-count-sm-union-of-two-knowable
  gloss-union-of-zero
  gloss-skm-union-of-two-knowable
  gloss-iteration-sequence
  gloss-ref-maybe-knowable
  gloss-count
  gloss-empty?
  rider-and-gloss-update-maybe-knowable
  make-gloss-glossesque-sys
  gloss-set-maybe-knowable
  uninformative-dynamic-type
  dynamic-type-var-for-any-dynamic-type?)
(provide
  dynamic-type-var-for-any-dynamic-type)
(provide /own-contract-out
  expressly-has-dynamic-type-impl?
  prop:expressly-has-dynamic-type
  make-expressly-has-dynamic-type-impl
  get-dynamic-type
  smoosh-report?
  smoosh-report-impl?
  smoosh-report-join-knowable-promise-maybe-knowable-promise
  smoosh-report-meet-knowable-promise-maybe-knowable-promise
  smoosh-report-==-knowable-promise-maybe-knowable-promise
  smoosh-report-path-related-knowable-promise-maybe-knowable-promise
  prop:smoosh-report
  make-smoosh-report-impl
  uninformative-smoosh-report
  uninformative-smoosh-reports
  smoosh-and-comparison-of-two-report?
  smoosh-and-comparison-of-two-report-impl?
  smoosh-and-comparison-of-two-report-<=?-knowable-promise
  smoosh-and-comparison-of-two-report->=?-knowable-promise
  smoosh-and-comparison-of-two-report-get-smoosh-report
  prop:smoosh-and-comparison-of-two-report
  make-smoosh-and-comparison-of-two-report-impl
  uninformative-smoosh-and-comparison-of-two-report
  uninformative-smoosh-and-comparison-of-two-reports
  expressly-smooshable-dynamic-type-impl?
  prop:expressly-smooshable-dynamic-type
  make-expressly-smooshable-dynamic-type-impl
  dynamic-type-get-smoosh-of-zero-reports
  dynamic-type-get-smoosh-of-one-reports
  dynamic-type-get-smoosh-and-comparison-of-two-reports
  dynamic-type-get-smoosh-and-comparison-of-two-reports-via-second
  uninformative-hash-code
  smoosh-equal-hash-code-support-report?
  smoosh-equal-hash-code-support-report-impl?
  smoosh-equal-hash-code-support-report-==-hash-code-promise
  smoosh-equal-hash-code-support-report-path-related-hash-code-promise
  prop:smoosh-equal-hash-code-support-report
  make-smoosh-equal-hash-code-support-report-impl
  uninformative-smoosh-equal-hash-code-support-report
  uninformative-smoosh-equal-hash-code-support-reports
  expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type-impl?
  prop:expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type
  make-expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type-impl
  dynamic-type-get-smoosh-equal-hash-code-support-reports
  smoosh-report-map
  smoosh-reports-map
  smoosh-report-zip*-map
  smoosh-reports-zip*-map
  smoosh-and-comparison-of-two-report-map
  smoosh-and-comparison-of-two-reports-map
  smoosh-and-comparison-of-two-report-zip*-map
  smoosh-and-comparison-of-two-reports-zip*-map
  smoosh-equal-hash-code-support-report-map
  smoosh-equal-hash-code-support-reports-map
  smoosh-equal-hash-code-support-report-zip*-map
  smoosh-equal-hash-code-support-reports-zip*-map
  false-smoosh-and-comparison-of-two-reports
  constant-smoosh-report
  constant-smoosh-reports
  promise-map
  constant-smoosh-and-comparison-of-two-report
  constant-smoosh-and-comparison-of-two-reports
  constant-smoosh-equal-hash-code-support-report
  constant-smoosh-equal-hash-code-support-reports
  equal-always-wrapper?
  equal-always-wrapper-value)
(provide
  equal-always-wrapper)
(provide /own-contract-out
  indistinct-wrapper?
  indistinct-wrapper-value)
(provide
  indistinct-wrapper)
(provide /own-contract-out
  smoosh-and-comparison-of-two-report-join
  smoosh-and-comparison-of-two-reports-join
  smoosh-reports-with-hesitation-at-discrepancies
  smoosh-and-comparison-of-two-reports-with-hesitation-at-discrepancies
  list-rev-append
  list-rem-first-maybe-knowable
  assoc-list-skm-union-of-two-knowable
  assoc-list-ref-maybe-knowable
  rider-and-assoc-list-update-maybe-knowable
  counted-glossesque-sys
  equality-check-atom-glossesque-sys
  equal-always-atom-glossesque-sys
  equality-check-indistinct-atom-glossesque-sys
  indistinct-glossesque-sys
  equal-always-indistinct-atom-glossesque-sys
  chaperone=-atom-glossesque-sys
  chaperone=-indistinct-atom-glossesque-sys
  eq-atom-glossesque-sys
  eq-indistinct-atom-glossesque-sys
  equal-always-from-list-injection-glossesque-sys
  equal-always-indistinct-from-list-injection-glossesque-sys
  chaperone=-copiable-glossesque-sys
  chaperone=-indistinct-copiable-glossesque-sys
  normalized-glossesque-sys
  terminal-glossesque-sys
  make-expressly-smooshable-dynamic-type-impl-from-equal-always-list-isomorphism
  make-expressly-smooshable-dynamic-type-impl-from-chaperone-of-list-isomorphism
  make-expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type-impl-from-list-injection
  make-expressly-custom-gloss-key-dynamic-type-impl-from-list-injection
  make-expressly-smooshable-bundle-property-from-list-isomorphism
  make-expressly-smooshable-dynamic-type-impl-for-equal-always-atom
  make-expressly-smooshable-dynamic-type-impl-for-chaperone-of-atom
  make-expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type-impl-for-atom
  make-expressly-smooshable-bundle-property-for-atom
  non-nan-number-glossesque-sys
  nan-number?
  non-nan-number?
  non-nan-extflonum?
  nan-extflonum?
  dynamic-type-case-by-cases
  gloss-ref
  gloss-set
  make-gloss
  gloss-keys
  any-dynamic-type?)
(provide
  any-dynamic-type)


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

(define/own-contract (unknown)
  (-> unknown?)
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

(define/own-contract (knowable-if condition then)
  (-> boolean? (-> any/c) knowable?)
  (if condition
    (known /then)
    (unknown)))

(define/own-contract (knowable->falsable kble)
  (-> knowable? any/c)
  (mat kble (known value)
    value
    #f))

(define/own-contract (falsable->uninformative-knowable fble)
  (-> any/c knowable?)
  (knowable-if fble /fn fble))


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
  (->
    (-> expressly-knowable-predicate?
      (unconstrained-domain-> knowable?))
    expressly-knowable-predicate-impl?))

(define/own-contract call-knowable
  (unconstrained-domain-> knowable?)
  (procedure-reduce-arity
    (make-keyword-procedure
      (lambda (ks vs f . positional-args)
        (if (expressly-knowable-predicate? f)
          (w- accepts?-knowable
            (expressly-knowable-predicate-get-accepts?-knowable f)
          /keyword-apply accepts?-knowable ks vs positional-args)
        /if (procedure? f)
          (falsable->uninformative-knowable
            (keyword-apply f ks vs positional-args))
          (raise-arguments-error 'call-knowable
            "expected the called value to be a prop:expressly-knowable-predicate instance or a procedure"
            "f" f)))
      (lambda (f . positional-args)
        (if (expressly-knowable-predicate? f)
          (w- accepts?-knowable
            (expressly-knowable-predicate-get-accepts?-knowable f)
          /apply accepts?-knowable positional-args)
        /if (procedure? f)
          (falsable->uninformative-knowable /apply f positional-args)
          (raise-arguments-error 'call-knowable
            "expected the called value to be a prop:expressly-knowable-predicate instance or a procedure"
            "f" f))))
    (arity-at-least 1)))

; Returns a value that makes an appropriate `prop:procedure`
; implementation for a structure type that implements
; `prop:expressly-knowable-predicate`. It will often be preferable to
; pass this result through `procedure-reduce-arity`.
;
(define/own-contract (make-procedure-impl-for-knowable-predicate)
  (-> (unconstrained-domain-> any/c))
  (compose knowable->falsable call-knowable))

(define/own-contract
  (make-procedure-impl-for-knowable-predicate-with-arity-of-procedure p)
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
      makeshift-knowable-predicate-get-accepts?-direct)
    makeshift-knowable-predicate
    'makeshift-knowable-predicate
    makeshift-knowable-predicate-inspector
    (auto-write)
    (auto-equal)
    (#:prop prop:procedure
      (make-procedure-impl-for-knowable-predicate-with-arity-of-procedure
        accepts?-knowable))
    (#:prop prop:expressly-knowable-predicate
      (make-expressly-knowable-predicate-impl
        (dissectfn (makeshift-knowable-predicate accepts?-knowable)
          accepts?-knowable))))
  (makeshift-knowable-predicate accepts?-knowable))


(define-imitation-simple-generics
  glossesque-sys? glossesque-sys-impl?
  (#:method glossesque-sys-glossesque-union-of-zero (#:this))
  (#:method glossesque-sys-glossesque-skm-union-of-two-knowable
    (#:this)
    ()
    ()
    ()
    ())
  (#:method
    glossesque-sys-glossesque-ref-maybe-knowable (#:this) () ())
  (#:method glossesque-sys-rider-and-glossesque-update-maybe-knowable
    (#:this)
    ()
    ()
    ())
  (#:method glossesque-sys-glossesque-empty? (#:this) ())
  (#:method glossesque-sys-glossesque-count (#:this) ())
  (#:method glossesque-sys-glossesque-iteration-sequence (#:this) ())
  prop:glossesque-sys
  make-glossesque-sys-impl-from-various-unkeyworded
  'glossesque-sys 'glossesque-sys-impl (list))
(ascribe-own-contract glossesque-sys? (-> any/c boolean?))
(ascribe-own-contract glossesque-sys-impl? (-> any/c boolean?))
(ascribe-own-contract glossesque-sys-glossesque-union-of-zero
  (-> glossesque-sys? any/c))
(ascribe-own-contract
  glossesque-sys-glossesque-skm-union-of-two-knowable
  (-> glossesque-sys? any/c any/c any/c
    (-> any/c any/c maybe? maybe? (knowable/c (list/c any/c maybe?)))
    (knowable/c (list/c any/c any/c))))
(ascribe-own-contract glossesque-sys-glossesque-ref-maybe-knowable
  (-> glossesque-sys? any/c any/c (knowable/c maybe?)))
(ascribe-own-contract
  glossesque-sys-rider-and-glossesque-update-maybe-knowable
  (-> glossesque-sys? (list/c any/c any/c) any/c
    (-> (list/c any/c maybe?) (knowable/c (list/c any/c maybe?)))
    (knowable/c (list/c any/c any/c))))
(ascribe-own-contract glossesque-sys-glossesque-empty?
  (-> glossesque-sys? any/c boolean?))
(ascribe-own-contract glossesque-sys-glossesque-count
  (-> glossesque-sys? any/c natural?))
(ascribe-own-contract glossesque-sys-glossesque-iteration-sequence
  (-> glossesque-sys? any/c (sequence/c any/c any/c)))
(ascribe-own-contract prop:glossesque-sys
  (struct-type-property/c glossesque-sys-impl?))

(define/own-contract
  (glossesque-sys-glossesque-set-maybe-knowable gs g k m)
  (-> glossesque-sys? any/c any/c maybe? knowable?)
  (knowable-map
    (glossesque-sys-rider-and-glossesque-update-maybe-knowable
      gs (list (trivial) g) k
      (dissectfn (list (trivial) old-m) (list (trivial) m)))
  /dissectfn (list (trivial) g)
    g))

(define/own-contract
  (make-glossesque-sys-impl
    #:glossesque-union-of-zero glossesque-union-of-zero
    
    #:glossesque-skm-union-of-two-knowable
    glossesque-skm-union-of-two-knowable
    
    #:glossesque-ref-maybe-knowable glossesque-ref-maybe-knowable
    
    #:rider-and-glossesque-update-maybe-knowable
    rider-and-glossesque-update-maybe-knowable
    
    #:glossesque-empty? glossesque-empty?
    #:glossesque-count glossesque-count
    #:glossesque-iteration-sequence glossesque-iteration-sequence)
  (->
    #:glossesque-union-of-zero (-> glossesque-sys? any/c)
    
    #:glossesque-skm-union-of-two-knowable
    (-> glossesque-sys? any/c any/c any/c
      (-> any/c any/c maybe? maybe?
        (knowable/c (list/c any/c maybe?)))
      (knowable/c (list/c any/c any/c)))
    
    #:glossesque-ref-maybe-knowable
    (-> glossesque-sys? any/c any/c (knowable/c maybe?))
    
    #:rider-and-glossesque-update-maybe-knowable
    (-> glossesque-sys? (list/c any/c any/c) any/c
      (-> (list/c any/c maybe?) (knowable/c (list/c any/c maybe?)))
      (knowable/c (list/c any/c any/c)))
    
    #:glossesque-empty? (-> glossesque-sys? any/c boolean?)
    #:glossesque-count (-> glossesque-sys? any/c natural?)
    
    #:glossesque-iteration-sequence
    (-> glossesque-sys? any/c (sequence/c any/c any/c))
    
    glossesque-sys-impl?)
  (make-glossesque-sys-impl-from-various-unkeyworded
    glossesque-union-of-zero
    glossesque-skm-union-of-two-knowable
    glossesque-ref-maybe-knowable
    rider-and-glossesque-update-maybe-knowable
    glossesque-empty?
    glossesque-count
    glossesque-iteration-sequence))

(define-imitation-simple-struct
  (mapped-glossesque-glossesque-sys?
    mapped-glossesque-glossesque-sys-granted-glossesque
    mapped-glossesque-glossesque-sys-on-glossesque
    mapped-glossesque-glossesque-sys-original)
  mapped-glossesque-glossesque-sys
  'mapped-glossesque-glossesque-sys (current-inspector) (auto-write)
  
  (#:prop prop:glossesque-sys /make-glossesque-sys-impl
    
    #:glossesque-union-of-zero
    (dissectfn (mapped-glossesque-glossesque-sys >g< <g> original)
      (<g> /glossesque-sys-glossesque-union-of-zero original))
    
    #:glossesque-skm-union-of-two-knowable
    (fn gs state a b skm-union-knowable
      (dissect gs (mapped-glossesque-glossesque-sys >g< <g> original)
      /knowable-map
        (glossesque-sys-glossesque-skm-union-of-two-knowable
          original state (>g< a) (>g< b) skm-union-knowable)
      /fn result
        (<g> result)))
    
    #:glossesque-ref-maybe-knowable
    (fn gs g k
      (dissect gs (mapped-glossesque-glossesque-sys >g< <g> original)
      /w- g (>g< g)
      /glossesque-sys-glossesque-ref-maybe-knowable original g k))
    
    #:rider-and-glossesque-update-maybe-knowable
    (fn gs rider-and-g k on-rider-and-m-knowable
      (dissect gs (mapped-glossesque-glossesque-sys >g< <g> original)
      /dissect rider-and-g (list rider g)
      /w- rider-and-g (list rider />g< g)
      /knowable-map
        (glossesque-sys-rider-and-glossesque-update-maybe-knowable
          original rider-and-g k on-rider-and-m-knowable)
      /dissectfn (list rider g)
        (list rider /<g> g)))
    
    #:glossesque-empty?
    (fn gs g
      (dissect gs (mapped-glossesque-glossesque-sys >g< <g> original)
      /glossesque-sys-glossesque-empty? original />g< g))
    
    #:glossesque-count
    (fn gs g
      (dissect gs (mapped-glossesque-glossesque-sys >g< <g> original)
      /glossesque-sys-glossesque-count original />g< g))
    
    #:glossesque-iteration-sequence
    (fn gs g
      (dissect gs (mapped-glossesque-glossesque-sys >g< <g> original)
      /glossesque-sys-glossesque-iteration-sequence original />g< g))
    
    )
  
  )

(define/own-contract
  (glossesque-sys-map-glossesque gs
    #:granted-glossesque [granted-glossesque (fn g g)]
    #:on-glossesque [on-glossesque (fn g g)])
  (->*
    (glossesque-sys?)
    (
      #:granted-glossesque (-> any/c any/c)
      #:on-glossesque (-> any/c any/c))
    glossesque-sys?)
  (mapped-glossesque-glossesque-sys
    granted-glossesque on-glossesque gs))

(define-imitation-simple-struct
  (mapped-key-glossesque-sys?
    mapped-key-glossesque-sys-granted-key-knowable
    mapped-key-glossesque-sys-original)
  mapped-key-glossesque-sys
  'mapped-key-glossesque-sys (current-inspector) (auto-write)
  
  (#:prop prop:glossesque-sys /make-glossesque-sys-impl
    
    #:glossesque-union-of-zero
    (dissectfn (mapped-key-glossesque-sys >k<-knowable original)
      (glossesque-sys-glossesque-union-of-zero original))
    
    #:glossesque-skm-union-of-two-knowable
    (fn gs state a b skm-union-knowable
      (dissect gs (mapped-key-glossesque-sys >k<-knowable original)
      /glossesque-sys-glossesque-skm-union-of-two-knowable
        original state a b
      /fn state internal-k a-entry-m b-entry-m
        (w- k
          (mat a-entry-m (just a-entry)
            (dissect a-entry (list k v)
              k)
          /dissect b-entry-m (just b-entry)
            (dissect b-entry (list k v)
              k))
        /knowable-map
          (skm-union-knowable state k
            (maybe-map a-entry-m /dissectfn (list k v) v)
            (maybe-map b-entry-m /dissectfn (list k v) v))
        /dissectfn (list state m)
          (list state /maybe-map m /fn v /list k v))))
    
    #:glossesque-ref-maybe-knowable
    (fn gs g k
      (dissect gs (mapped-key-glossesque-sys >k<-knowable original)
      /knowable-bind (>k<-knowable k) /fn internal-k
      /knowable-bind
        (glossesque-sys-glossesque-ref-maybe-knowable
          original g internal-k)
      /fn entry-m
      /known /maybe-map entry-m /dissectfn (list k v) v))
    
    #:rider-and-glossesque-update-maybe-knowable
    (fn gs rider-and-g k on-rider-and-m-knowable
      (dissect gs (mapped-key-glossesque-sys >k<-knowable original)
      /knowable-bind (>k<-knowable k) /fn internal-k
      /glossesque-sys-rider-and-glossesque-update-maybe-knowable
        original rider-and-g internal-k
      /dissectfn (list rider entry-m)
        (w- k
          (expect entry-m (just entry) k
          /dissect entry (list k v)
            k)
        /w- v-m (maybe-map entry-m /dissectfn (list k v) v)
        /knowable-map (on-rider-and-m-knowable /list rider v-m)
        /dissectfn (list rider v-m)
          (list rider /maybe-map v-m /fn v /list k v))))
    
    #:glossesque-empty?
    (fn gs g
      (dissect gs (mapped-key-glossesque-sys >k<-knowable original)
      /glossesque-sys-glossesque-empty? original g))
    
    #:glossesque-count
    (fn gs g
      (dissect gs (mapped-key-glossesque-sys >k<-knowable original)
      /glossesque-sys-glossesque-count original g))
    
    #:glossesque-iteration-sequence
    (fn gs g
      (dissect gs (mapped-key-glossesque-sys >k<-knowable original)
      /sequence-map
        (fn internal-k entry
          (dissect entry (list k v)
          /values k v))
        (glossesque-sys-glossesque-iteration-sequence original g)))
    
    )
  
  )

(define/own-contract
  (glossesque-sys-map-key gs
    #:granted-key [granted-key (fn k k)]
    
    #:granted-key-knowable
    [granted-key-knowable (fn k /known /granted-key k)]
    
    )
  (->*
    (glossesque-sys?)
    (
      #:granted-key (-> any/c any/c)
      #:granted-key-knowable (-> any/c knowable?))
    glossesque-sys?)
  (mapped-key-glossesque-sys granted-key-knowable gs))

(define/own-contract (maybe-nonempty-glossesque-sys original)
  (-> glossesque-sys? glossesque-sys?)
  (glossesque-sys-map-glossesque original
    #:granted-glossesque
    (fn g-m
      (mat g-m (just g) g
      /glossesque-sys-glossesque-union-of-zero original))
    #:on-glossesque
    (fn g
      (maybe-if (not /glossesque-sys-glossesque-empty? original g) /fn
        g))))


(define-imitation-simple-struct
  (tagged-glossesque-sys?
    tagged-glossesque-sys-get-inhabitant?
    tagged-glossesque-sys-get-glossesque-sys)
  tagged-glossesque-sys
  'tagged-glossesque-sys (current-inspector) (auto-write) (auto-equal))
(ascribe-own-contract tagged-glossesque-sys? (-> any/c boolean?))
(ascribe-own-contract tagged-glossesque-sys-get-inhabitant?
  (-> tagged-glossesque-sys? (-> any/c boolean?)))
(ascribe-own-contract tagged-glossesque-sys-get-glossesque-sys
  (-> tagged-glossesque-sys? glossesque-sys?))


(define-imitation-simple-generics
  custom-gloss-key-report? custom-gloss-key-report-impl?
  ; Returns a `glossesque-sys?` which compares keys by smooshing them
  ; along ==, i.e. (<= and >=).
  (#:method
    custom-gloss-key-report-get-==-tagged-glossesque-sys-knowable
    (#:this))
  ; Returns a `glossesque-sys?` which compares keys by smooshing them
  ; along the transitive closure of (<= or >=).
  (#:method
    custom-gloss-key-report-get-path-related-tagged-glossesque-sys-knowable
    (#:this))
  prop:custom-gloss-key-report
  make-custom-gloss-key-report-impl-from-various-unkeyworded
  'custom-gloss-key-report 'custom-gloss-key-report-impl (list))
(ascribe-own-contract custom-gloss-key-report? (-> any/c boolean?))
(ascribe-own-contract custom-gloss-key-report-impl? (-> any/c boolean?))
(ascribe-own-contract
  custom-gloss-key-report-get-==-tagged-glossesque-sys-knowable
  (-> custom-gloss-key-report? (knowable/c tagged-glossesque-sys?)))
(ascribe-own-contract
  custom-gloss-key-report-get-path-related-tagged-glossesque-sys-knowable
  (-> custom-gloss-key-report? (knowable/c tagged-glossesque-sys?)))
(ascribe-own-contract prop:custom-gloss-key-report
  (struct-type-property/c custom-gloss-key-report-impl?))

(define/own-contract
  (make-custom-gloss-key-report-impl
    #:get-==-tagged-glossesque-sys-knowable
    get-==-tagged-glossesque-sys-knowable
    
    #:get-path-related-tagged-glossesque-sys-knowable
    get-path-related-tagged-glossesque-sys-knowable)
  (->
    #:get-==-tagged-glossesque-sys-knowable
    (-> custom-gloss-key-report? (knowable/c tagged-glossesque-sys?))
    
    #:get-path-related-tagged-glossesque-sys-knowable
    (-> custom-gloss-key-report? (knowable/c tagged-glossesque-sys?))
    
    custom-gloss-key-report-impl?)
  (make-custom-gloss-key-report-impl-from-various-unkeyworded
    get-==-tagged-glossesque-sys-knowable
    get-path-related-tagged-glossesque-sys-knowable))

(define-imitation-simple-struct
  (uninformative-custom-gloss-key-report?)
  uninformative-custom-gloss-key-report-unguarded
  'uninformative-custom-gloss-key-report (current-inspector)
  (auto-write)
  (#:prop prop:custom-gloss-key-report
    (make-custom-gloss-key-report-impl
      
      #:get-==-tagged-glossesque-sys-knowable
      (fn self
        (unknown))
      
      #:get-path-related-tagged-glossesque-sys-knowable
      (fn self
        (unknown))
      
      )))

(define/own-contract (uninformative-custom-gloss-key-report)
  (-> custom-gloss-key-report?)
  (uninformative-custom-gloss-key-report-unguarded))

(define/own-contract (uninformative-custom-gloss-key-reports)
  (-> (sequence/c custom-gloss-key-report?))
  (in-cycle /list /uninformative-custom-gloss-key-report))

(define-imitation-simple-struct
  (mapped-custom-gloss-key-report?
    mapped-custom-gloss-key-report-on-==-tagged-glossesque-sys-knowable
    mapped-custom-gloss-key-report-on-path-related-tagged-glossesque-sys-knowable
    mapped-custom-gloss-key-report-original)
  mapped-custom-gloss-key-report
  'mapped-custom-gloss-key-report (current-inspector) (auto-write)
  (#:prop prop:custom-gloss-key-report
    (make-custom-gloss-key-report-impl
      
      #:get-==-tagged-glossesque-sys-knowable
      (dissectfn
        (mapped-custom-gloss-key-report
          on-==-tagged-glossesque-sys-knowable
          on-path-related-tagged-glossesque-sys-knowable
          original)
        (on-==-tagged-glossesque-sys-knowable
          (custom-gloss-key-report-get-==-tagged-glossesque-sys-knowable
            original)))
      
      #:get-path-related-tagged-glossesque-sys-knowable
      (dissectfn
        (mapped-custom-gloss-key-report
          on-==-tagged-glossesque-sys-knowable
          on-path-related-tagged-glossesque-sys-knowable
          original)
        (on-path-related-tagged-glossesque-sys-knowable
          (custom-gloss-key-report-get-path-related-tagged-glossesque-sys-knowable
            original)))
      
      )))

(define/own-contract
  (custom-gloss-key-report-map report
    
    #:on-tagged-glossesque-sys-knowable
    [ on-tagged-glossesque-sys-knowable
      (fn tgs-k
        tgs-k)]
    
    #:on-==-tagged-glossesque-sys-knowable
    [ on-==-tagged-glossesque-sys-knowable
      on-tagged-glossesque-sys-knowable]
    
    #:on-path-related-tagged-glossesque-sys-knowable
    [ on-path-related-tagged-glossesque-sys-knowable
      on-tagged-glossesque-sys-knowable]
    
    )
  (->*
    (custom-gloss-key-report?)
    (
      #:on-tagged-glossesque-sys-knowable
      (-> (knowable/c tagged-glossesque-sys?)
        (knowable/c tagged-glossesque-sys?))
      
      #:on-==-tagged-glossesque-sys-knowable
      (-> (knowable/c tagged-glossesque-sys?)
        (knowable/c tagged-glossesque-sys?))
      
      #:on-path-related-tagged-glossesque-sys-knowable
      (-> (knowable/c tagged-glossesque-sys?)
        (knowable/c tagged-glossesque-sys?))
      
      )
    custom-gloss-key-report?)
  (mapped-custom-gloss-key-report
    on-==-tagged-glossesque-sys-knowable
    on-path-related-tagged-glossesque-sys-knowable
    report))

(define/own-contract
  (custom-gloss-key-reports-map reports
    
    #:on-tagged-glossesque-sys-knowable
    [ on-tagged-glossesque-sys-knowable
      (fn tgs-k
        tgs-k)]
    
    #:on-==-tagged-glossesque-sys-knowable
    [ on-==-tagged-glossesque-sys-knowable
      on-tagged-glossesque-sys-knowable]
    
    #:on-path-related-tagged-glossesque-sys-knowable
    [ on-path-related-tagged-glossesque-sys-knowable
      on-tagged-glossesque-sys-knowable]
    
    )
  (->*
    ((sequence/c custom-gloss-key-report?))
    (
      #:on-tagged-glossesque-sys-knowable
      (-> (knowable/c tagged-glossesque-sys?)
        (knowable/c tagged-glossesque-sys?))
      
      #:on-==-tagged-glossesque-sys-knowable
      (-> (knowable/c tagged-glossesque-sys?)
        (knowable/c tagged-glossesque-sys?))
      
      #:on-path-related-tagged-glossesque-sys-knowable
      (-> (knowable/c tagged-glossesque-sys?)
        (knowable/c tagged-glossesque-sys?))
      
      )
    (sequence/c custom-gloss-key-report?))
  (sequence-map
    (fn report
      (custom-gloss-key-report-map report
        
        #:on-==-tagged-glossesque-sys-knowable
        on-==-tagged-glossesque-sys-knowable
        
        #:on-path-related-tagged-glossesque-sys-knowable
        on-path-related-tagged-glossesque-sys-knowable
        
        ))
    reports))

(define-imitation-simple-struct
  (zip*-mapped-custom-gloss-key-report?
    zip*-mapped-custom-gloss-key-report-on-==-tagged-glossesque-sys-knowable
    zip*-mapped-custom-gloss-key-report-on-path-related-tagged-glossesque-sys-knowable
    zip*-mapped-custom-gloss-key-report-original-list)
  zip*-mapped-custom-gloss-key-report
  'zip*-mapped-custom-gloss-key-report (current-inspector)
  (auto-write)
  (#:prop prop:custom-gloss-key-report
    (make-custom-gloss-key-report-impl
      
      #:get-==-tagged-glossesque-sys-knowable
      (dissectfn
        (zip*-mapped-custom-gloss-key-report
          on-==-tagged-glossesque-sys-knowable
          on-path-related-tagged-glossesque-sys-knowable
          original-list)
        (on-==-tagged-glossesque-sys-knowable
          (list-map original-list /fn original
            (custom-gloss-key-report-get-==-tagged-glossesque-sys-knowable
              original))))
      
      #:get-path-related-tagged-glossesque-sys-knowable
      (dissectfn
        (zip*-mapped-custom-gloss-key-report
          on-==-tagged-glossesque-sys-knowable
          on-path-related-tagged-glossesque-sys-knowable
          original-list)
        (on-path-related-tagged-glossesque-sys-knowable
          (list-map original-list /fn original
            (custom-gloss-key-report-get-path-related-tagged-glossesque-sys-knowable
              original))))
      
      )))

(define/own-contract
  (custom-gloss-key-report-zip*-map report-list
    
    #:on-tagged-glossesque-sys-knowable
    [ on-tagged-glossesque-sys-knowable
      (fn tgs-k-list
        (raise-arguments-error 'custom-gloss-key-report-zip*-map
          "tried to retrieve a glossesque system from a custom gloss key report when its mapping behavior was undefined"
          "tagged-glossesque-sys-knowable-list" tgs-k-list))]
    
    #:on-==-tagged-glossesque-sys-knowable
    [ on-==-tagged-glossesque-sys-knowable
      on-tagged-glossesque-sys-knowable]
    
    #:on-path-related-tagged-glossesque-sys-knowable
    [ on-path-related-tagged-glossesque-sys-knowable
      on-tagged-glossesque-sys-knowable]
    
    )
  (->*
    ((listof custom-gloss-key-report?))
    (
      #:on-tagged-glossesque-sys-knowable
      (-> (listof (knowable/c tagged-glossesque-sys?))
        (knowable/c tagged-glossesque-sys?))
      
      #:on-==-tagged-glossesque-sys-knowable
      (-> (listof (knowable/c tagged-glossesque-sys?))
        (knowable/c tagged-glossesque-sys?))
      
      #:on-path-related-tagged-glossesque-sys-knowable
      (-> (listof (knowable/c tagged-glossesque-sys?))
        (knowable/c tagged-glossesque-sys?))
      
      )
    custom-gloss-key-report?)
  (zip*-mapped-custom-gloss-key-report
    on-==-tagged-glossesque-sys-knowable
    on-path-related-tagged-glossesque-sys-knowable
    report-list))

(define/own-contract
  (custom-gloss-key-reports-zip*-map reports-list
    
    #:on-tagged-glossesque-sys-knowable
    [ on-tagged-glossesque-sys-knowable
      (fn tgs-k-list
        (raise-arguments-error 'custom-gloss-key-reports-zip*-map
          "tried to retrieve a glossesque system from a custom gloss key report when its mapping behavior was undefined"
          "tagged-glossesque-sys-knowable-list" tgs-k-list))]
    
    #:on-==-tagged-glossesque-sys-knowable
    [ on-==-tagged-glossesque-sys-knowable
      on-tagged-glossesque-sys-knowable]
    
    #:on-path-related-tagged-glossesque-sys-knowable
    [ on-path-related-tagged-glossesque-sys-knowable
      on-tagged-glossesque-sys-knowable]
    
    )
  (->*
    ((listof (sequence/c custom-gloss-key-report?)))
    (
      #:on-tagged-glossesque-sys-knowable
      (-> (listof (knowable/c tagged-glossesque-sys?))
        (knowable/c tagged-glossesque-sys?))
      
      #:on-==-tagged-glossesque-sys-knowable
      (-> (listof (knowable/c tagged-glossesque-sys?))
        (knowable/c tagged-glossesque-sys?))
      
      #:on-path-related-tagged-glossesque-sys-knowable
      (-> (listof (knowable/c tagged-glossesque-sys?))
        (knowable/c tagged-glossesque-sys?))
      
      )
    (sequence/c custom-gloss-key-report?))
  (sequence-zip*-map reports-list /fn report-list
    (custom-gloss-key-report-zip*-map report-list
      
      #:on-==-tagged-glossesque-sys-knowable
      on-==-tagged-glossesque-sys-knowable
      
      #:on-path-related-tagged-glossesque-sys-knowable
      on-path-related-tagged-glossesque-sys-knowable)))

(define/own-contract
  (constant-custom-gloss-key-report
    #:tagged-glossesque-sys-knowable tagged-glossesque-sys-knowable)
  (->
    #:tagged-glossesque-sys-knowable
    (knowable/c tagged-glossesque-sys?)
    
    custom-gloss-key-report?)
  (custom-gloss-key-report-zip*-map (list)
    #:on-tagged-glossesque-sys-knowable
    (dissectfn (list)
      tagged-glossesque-sys-knowable)))

(define/own-contract
  (constant-custom-gloss-key-reports
    #:tagged-glossesque-sys-knowable tagged-glossesque-sys-knowable)
  (->
    #:tagged-glossesque-sys-knowable
    (knowable/c tagged-glossesque-sys?)
    
    (sequence/c custom-gloss-key-report?))
  (in-cycle /list /constant-custom-gloss-key-report
    #:tagged-glossesque-sys-knowable tagged-glossesque-sys-knowable))

(define/own-contract (sequence-first s)
  (-> sequence? any)
  (sequence-ref s 0))

(define-imitation-simple-struct
  (path-related-wrapper? path-related-wrapper-value)
  path-related-wrapper
  'path-related-wrapper (current-inspector) (auto-write)
  (#:gen gen:equal-mode+hash
    
    (define (equal-mode-proc a b recur now?)
      (knowable->falsable /knowable-map
        (force
          ; TODO FORWARD: These uses of
          ; `smoosh-report-==-knowable-promise-maybe-knowable-promise`,
          ; `smoosh-and-comparison-of-two-report-get-smoosh-report`,
          ; `dynamic-type-get-smoosh-and-comparison-of-two-reports`,
          ; and `any-dynamic-type` are forward references. See if we can
          ; untangle them.
          (smoosh-report-==-knowable-promise-maybe-knowable-promise
            (smoosh-and-comparison-of-two-report-get-smoosh-report
              (sequence-first
                (dynamic-type-get-smoosh-and-comparison-of-two-reports
                  (any-dynamic-type)
                  a
                  b)))))
        (fn kpm
          (just? kpm))))
    
    (define (hash-mode-proc v recur now?)
      ; TODO FORWARD: These uses of
      ; `smoosh-equal-hash-code-support-report-==-hash-code-promise`,
      ; `dynamic-type-get-smoosh-equal-hash-code-support-reports`, and
      ; `any-dynamic-type` are forward references. See if we can
      ; untangle them.
      (force
        (smoosh-equal-hash-code-support-report-==-hash-code-promise
          (sequence-first
            (dynamic-type-get-smoosh-equal-hash-code-support-reports
              (any-dynamic-type)
              v))
          #f)))
    
    ))
(ascribe-own-contract path-related-wrapper? (-> any/c boolean?))
(ascribe-own-contract path-related-wrapper-value
  (-> path-related-wrapper? any/c))

(define-imitation-simple-struct (info-wrapper? info-wrapper-value)
  info-wrapper 'info-wrapper (current-inspector) (auto-write)
  (#:gen gen:equal-mode+hash
    
    (define (equal-mode-proc a b recur now?)
      (knowable->falsable /knowable-map
        (force
          ; TODO FORWARD: These uses of
          ; `smoosh-report-==-knowable-promise-maybe-knowable-promise`,
          ; `smoosh-and-comparison-of-two-report-get-smoosh-report`,
          ; `dynamic-type-get-smoosh-and-comparison-of-two-reports`,
          ; and `any-dynamic-type` are forward references. See if we can
          ; untangle them.
          (smoosh-report-==-knowable-promise-maybe-knowable-promise
            (smoosh-and-comparison-of-two-report-get-smoosh-report
              (sequence-first
                (dynamic-type-get-smoosh-and-comparison-of-two-reports
                  (any-dynamic-type)
                  a
                  b)))))
        (fn kpm
          (just? kpm))))
    
    (define (hash-mode-proc v recur now?)
      ; TODO FORWARD: These uses of
      ; `smoosh-equal-hash-code-support-report-==-hash-code-promise`,
      ; `dynamic-type-get-smoosh-equal-hash-code-support-reports`, and
      ; `any-dynamic-type` are forward references. See if we can
      ; untangle them.
      (force
        (smoosh-equal-hash-code-support-report-==-hash-code-promise
          (sequence-first
            (dynamic-type-get-smoosh-equal-hash-code-support-reports
              (any-dynamic-type)
              v))
          #f)))
    
    ))
(ascribe-own-contract info-wrapper? (-> any/c boolean?))
(ascribe-own-contract info-wrapper-value (-> info-wrapper? any/c))

(define-imitation-simple-generics
  expressly-custom-gloss-key-dynamic-type?
  expressly-custom-gloss-key-dynamic-type-impl?
  (#:method expressly-custom-gloss-key-dynamic-type-get-custom-gloss-key-reports
    (#:this)
    ())
  prop:expressly-custom-gloss-key-dynamic-type
  make-expressly-custom-gloss-key-dynamic-type-impl-from-various-unkeyworded
  'expressly-custom-gloss-key-dynamic-type
  'expressly-custom-gloss-key-dynamic-type-impl (list))
(ascribe-own-contract expressly-custom-gloss-key-dynamic-type-impl?
  (-> any/c boolean?))
(ascribe-own-contract prop:expressly-custom-gloss-key-dynamic-type
  (struct-type-property/c
    expressly-custom-gloss-key-dynamic-type-impl?))

(define/own-contract
  (make-expressly-custom-gloss-key-dynamic-type-impl
    #:get-custom-gloss-key-reports get-custom-gloss-key-reports)
  (->
    #:get-custom-gloss-key-reports
    (-> any/c any/c (sequence/c custom-gloss-key-report?))
    
    expressly-custom-gloss-key-dynamic-type-impl?)
  (make-expressly-custom-gloss-key-dynamic-type-impl-from-various-unkeyworded
    get-custom-gloss-key-reports))

(define/own-contract
  (dynamic-type-get-custom-gloss-key-reports dt inhabitant)
  ; For each report in the infinite sequence, the next report creates
  ; glossesques that not only compare keys by whether they smoosh
  ; along that one's `==` but also, only if they do, smooshes their
  ; information ordering representatives along their information
  ; ordering.
  (-> any/c any/c (sequence/c custom-gloss-key-report?))
  (if (expressly-custom-gloss-key-dynamic-type? dt)
    (expressly-custom-gloss-key-dynamic-type-get-custom-gloss-key-reports
      dt inhabitant)
  /uninformative-custom-gloss-key-reports))


; TODO SMOOSH: Aren't we going to have the default "any" type depend
; partly on what orphan instances are in scope?
(define/own-contract (get-dynamic-type-with-default-bindings v)
  (-> any/c any/c)
  ; TODO FORWARD: These uses of `known-to-lathe-comforts-data?`,
  ; `known-to-lathe-comforts-data-dynamic-type`, `any-dynamic-type`,
  ; `get-dynamic-type`, `make-gloss`, and
  ; `dynamic-type-var-for-any-dynamic-type` are forward references.
  ; See if we can untangle them.
  (if (known-to-lathe-comforts-data? v)
    (known-to-lathe-comforts-data-dynamic-type /any-dynamic-type)
  /get-dynamic-type
    (known-value /make-gloss /list /cons
      (dynamic-type-var-for-any-dynamic-type)
      (just /any-dynamic-type))
    v))

(define (knowable-zip* knowable-list)
  (-> (listof knowable?) (knowable/c list?))
  (expect knowable-list (cons knowable knowable-list) (known /list)
  /knowable-bind knowable /fn element
  /knowable-map (knowable-zip* knowable-list) /fn element-list
    (cons element element-list)))

(define (maybe-min-zip* maybe-list)
  (-> (listof maybe?) (maybe/c list?))
  (expect maybe-list (cons maybe maybe-list) (list)
  /maybe-bind maybe /fn element
  /maybe-map (maybe-min-zip* maybe-list) /fn element-list
    (cons element element-list)))

(define/own-contract (promise-zip*-map p-list on-value)
  (-> (listof promise?) (-> any/c any/c) promise?)
  (delay /on-value /list-map p-list /fn p /force p))

(define/own-contract (knowable-promise-zip*-map kp-list on-value)
  (-> (listof (promise/c knowable?)) (-> any/c any/c)
    (promise/c knowable?))
  (delay
    (knowable-if (list-all kp-list /fn kp /known? /force kp) /fn
      (on-value /list-map kp-list /fn kp /known-value /force kp))))

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
  (boolean-and-knowable-promise-zip*
    (list-map kble-thunk-list /fn kble-thunk /delay /kble-thunk)))

(define/own-contract (boolean-or-knowable-thunk-zip* kble-thunk-list)
  (-> (listof (-> (knowable/c boolean?))) (knowable/c boolean?))
  (not /boolean-and-knowable-thunk-zip*
    (list-map kble-thunk-list /fn kble-thunk /fn /not /kble-thunk)))

(define/own-contract
  (maybe-min-knowable-promise-zip*-map mkp-list on-value)
  (-> (listof (promise/c (knowable/c maybe?))) (-> list? any/c)
    (promise/c (knowable/c maybe?)))
  (delay
    (if
      (list-any mkp-list /fn mkp
        (mat (force mkp) (known /nothing) #t #f))
      (known /nothing)
    /force /knowable-promise-zip*-map mkp-list /fn m-list
      (just /on-value /list-map m-list /fn m /just-value m))))

(define/own-contract (sequence-zip*-map sequences on-element)
  (->
    (non-empty-listof (sequence/c any/c))
    (-> (non-empty-listof any/c) any/c)
    (sequence/c any/c))
  (sequence-map
    (lambda elements /on-element elements)
    (apply in-parallel sequences)))

(define/own-contract (knowable-promise-or kp-list)
  (-> (listof (knowable/c promise?)) (knowable/c promise?))
  (delay
    (w-loop next kp-list kp-list
      (expect kp-list (cons kp kp-list) (unknown)
      /w- k (force kp)
      /if (known? k) k
      /next kp-list))))

(define/own-contract
  (gloss-equal-always?-knowable a b value-equal-always?-knowable)
  (-> any/c any/c (-> any/c any/c (knowable/c boolean?))
    (knowable/c boolean?))
  (if (not /equal-always? (gloss-count a) (gloss-count b)) (known #f)
  /knowable-bind
    (knowable-zip*
      (list-map
        (sequence->list
          (in-values-sequence /gloss-iteration-sequence a))
      /dissectfn (list k a-v)
        (knowable-map (gloss-ref-maybe-knowable b k) /fn b-v-m
          (maybe-map b-v-m /fn b-v
            (delay /value-equal-always?-knowable a-v b-v)))))
  /fn v=?-kpm-list
  /expect (maybe-min-zip* v=?-kpm-list) (just v=?-kp-list) (known #f)
  /force /boolean-and-knowable-promise-zip* v=?-kp-list))

(define-imitation-simple-struct (gloss-rep-empty?) gloss-rep-empty
  'gloss-rep-empty (current-inspector) (auto-write))
(define-imitation-simple-struct
  (gloss-rep-glossesque?
    
    ; A `natural?` summarizing the combined size of the
    ; `gloss-rep-glossesque-then` glossesque and the
    ; `gloss-rep-glossesque-else` gloss.
    gloss-rep-glossesque-count
    
    ; A `tagged-glossesque-sys?`.
    gloss-rep-glossesque-tagged-glossesque-sys
    
    ; A glossesque of that system containing entries with a known true
    ; result for its `inhabitant?` predicate.
    gloss-rep-glossesque-then
    
    ; A gloss of entries with a known false result.
    gloss-rep-glossesque-else
    
    )
  gloss-rep-glossesque
  'gloss-rep-glossesque (current-inspector) (auto-write))
(define-imitation-simple-struct
  (gloss? gloss-rep)
  gloss 'gloss (current-inspector)
  
  (#:gen gen:custom-write
    (define write-proc
      (make-constructor-style-printer
        (fn self 'make-gloss)
        (fn self
          (for/list
            ; TODO FORWARD: This use of `gloss-iteration-sequence` is
            ; a forward reference. See if we can untangle it.
            ([(k v) (in-sequences /gloss-iteration-sequence self)])
            (cons k v))))))
  
  (#:gen gen:equal-mode+hash
    
    (define (equal-mode-proc a b recur now?)
      (knowable->falsable /gloss-equal-always?-knowable a b recur))
    
    (define (hash-mode-proc v recur now?)
      (define (hash-code-smooshable v)
        (force
          (smoosh-equal-hash-code-support-report-==-hash-code-promise
            (sequence-first
              (dynamic-type-get-smoosh-equal-hash-code-support-reports
                (any-dynamic-type)
                v))
            #f)))
      (hash-code-combine (equal-always-hash-code gloss?)
        (hash-code-combine-unordered* /for/list
          ; TODO FORWARD: This use of `gloss-iteration-sequence` is a
          ; forward reference. See if we can untangle it.
          ([(k v) (in-sequences /gloss-iteration-sequence v)])
          (hash-code-combine (hash-code-smooshable k) (recur v)))))
    
    ))
(ascribe-own-contract gloss? (-> any/c boolean?))

(define/own-contract
  (list-map-foldl-knowable state lst on-elem-knowable)
  (-> any/c list?
    (-> any/c any/c (knowable/c (list/c any/c any/c)))
    (knowable/c (list/c any/c list?)))
  (expect lst (cons elem lst) (known /list state /list)
  /knowable-bind (on-elem-knowable state elem)
  /dissectfn (list state elem)
  /knowable-bind (list-map-foldl-knowable state lst on-elem-knowable)
  /dissectfn (list state lst)
  /known /list state /cons elem lst))

(define/own-contract
  (rider-and-hash-update-maybe-knowable
    rider-and-h k on-rider-and-m-knowable)
  (-> (list/c any/c hash?) any/c
    (-> (list/c any/c maybe?) (knowable/c (list/c any/c maybe?)))
    (knowable/c (list/c any/c maybe?)))
  (dissect rider-and-h (list rider h)
  /w- m (hash-ref-maybe h k)
  /knowable-bind (on-rider-and-m-knowable /list rider m)
  /dissectfn (list rider m)
  /w- h (hash-set-maybe h k m)
  /known /list rider h))

(define/own-contract
  (hash-skv-map-maybe-knowable state h skv-map-maybe-knowable)
  (-> any/c (and/c hash? immutable?)
    (-> any/c any/c any/c (knowable/c (list/c any/c maybe?)))
    (knowable/c (list/c any/c (and/c hash? immutable?))))
  (knowable-map
    (knowable-zip*
      (list-map-foldl-knowable state (hash->list h)
        (fn state-and-entry
          (dissect state-and-entry (list state /cons k v)
          /knowable-map (skv-map-maybe-knowable state k v)
            (dissectfn (list state v-m)
              (list state
                (expect v-m (just v) (list)
                /list /cons k v)))))))
  /dissectfn (list state kv-list-list)
    (list state /make-similar-hash h /append* kv-list-list)))

(define/own-contract
  (hash-skm-union-of-two-knowable state a b skm-union-knowable)
  (-> any/c (and/c hash? immutable?) (and/c hash? immutable?)
    (-> any/c any/c maybe? maybe?
      (knowable/c (list/c any/c maybe?)))
    (knowable/c (list/c any/c (and/c hash? immutable?))))
  (hash-skv-map-maybe-knowable state
    (hash-union a b #:combine /fn a b a)
    (fn state k v
      (skm-union-knowable
        state k (hash-ref-maybe a k) (hash-ref-maybe b k)))))

(define/own-contract
  (rider-and-maybe-update-knowable
    rider-and-m on-rider-and-v-knowable)
  (-> (list/c any/c maybe?)
    (-> (list/c any/c any/c) (knowable/c (list/c any/c any/c)))
    (knowable/c (list/c any/c maybe?)))
  (dissect rider-and-m (list rider m)
  /expect m (just v) (known rider-and-m)
  /knowable-bind (on-rider-and-v-knowable /list rider v)
    (dissectfn (list rider v)
      (known /list rider /just v))))

; Given a `maybe?` value and a stateful way of updating it, this is a
; stateful way of updating it that maintains a count included in the
; state. The operand is considered to have contributed 1 to the count
; if it's a `just?` and 0 otherwise. With the way this operation
; maintains the count, the result will be contributing 1 or 0 in the
; same way in its place. This can reduce or increase the count by no
; more than 1.
;
(define/own-contract
  (count-and-rider-and-maybe-update-maybe-knowable
    crm on-rider-and-m-knowable)
  (-> (list/c (list/c natural? any/c) maybe?)
    (-> (list/c any/c maybe?) (knowable/c (list/c any/c maybe?)))
    (knowable/c (and/c (list/c natural? any/c) maybe?)))
  (dissect crm (list (list count rider) m)
  /knowable-map (on-rider-and-m-knowable /list rider m)
    (dissectfn (list rider new-m)
      (w- count
        (+ count (- (if (just? new-m) 1 0) (if (just? m) 1 0)))
      /list (list count rider) new-m))))

; Given two `maybe?` values and a stateful way of taking their union,
; this is a stateful way of taking their union that skips actually
; doing the work if both of the `maybe?` values is a `mothing?`. It
; just returns a `nothing?` in that case.
;
(define/own-contract
  (maybe-or-sm-union-of-two-knowable state a b sm-union-knowable)
  (-> any/c maybe? maybe?
    (-> any/c maybe? maybe? (knowable/c (list/c any/c maybe?)))
    (knowable/c (list/c any/c maybe?)))
  (mat a (just _) (sm-union-knowable state a b)
  /mat b (just _) (sm-union-knowable state a b)
  /known /list state /nothing))

; Given two `maybe?` values and a stateful way of taking their union,
; this is a stateful way of taking their union that maintains a count
; included in the state. The left-hand-side operand is considered to
; have contributed 1 to the count if it's a `just?` and 0 otherwise.
; With the way this operation maintains the count, the result will be
; contributing 1 or 0 in the same way in its place. This can reduce or
; increase the count by no more than 1.
;
(define/own-contract
  (maybe-left-count-sm-union-of-two-knowable cs a b sm-union-knowable)
  (-> (list/c natural? any/c) maybe? maybe?
    (-> any/c maybe? maybe? (knowable/c (list/c any/c maybe?)))
    (knowable/c (list/c (list/c natural? any/c) maybe?)))
  (count-and-rider-and-maybe-update-maybe-knowable (list cs a)
    (dissectfn (list state a)
      (sm-union-knowable state a b))))

(define/own-contract (gloss-union-of-zero)
  (-> gloss?)
  (gloss /gloss-rep-empty))

(define/own-contract (gloss-iteration-sequence g)
  (-> gloss? (sequence/c any/c any/c))
  (dissect g (gloss rep)
  /mat rep (gloss-rep-empty) (list)
  /dissect rep
    (gloss-rep-glossesque _ (tagged-glossesque-sys _ gs) then else)
  /in-sequences
    (glossesque-sys-glossesque-iteration-sequence gs then)
    (gloss-iteration-sequence else)))

(define/own-contract
  (gloss-skm-union-of-two-knowable state a b skm-union-knowable)
  (-> any/c gloss? gloss?
    (-> any/c any/c maybe? maybe? (knowable/c (list/c any/c maybe?)))
    (knowable/c (list/c any/c gloss?)))
  ; Without loss of generality, we suppose `a` is at least `b`'s size.
  (dissect
    (if (< (gloss-count a) (gloss-count b)) (list b a) (list a b))
    (list a b)
  /w- as
    (sequence->stream /in-values-sequence /gloss-iteration-sequence a)
  /w-loop next state state as as b b result (gloss-union-of-zero)
    (expect as (stream* a-entry as)
      (w- bs
        (sequence->stream
          (in-values-sequence /gloss-iteration-sequence b))
      /w-loop next state state bs bs result result
        (expect bs (stream* b-entry bs)
          (known /list state result)
        /dissect b-entry (list k b-v)
        /knowable-bind
          (skm-union-knowable state k (nothing) (just b-v))
        /dissectfn (list state m)
        ; TODO FORWARD: This use of `gloss-set-maybe-knowable` is a
        ; forward reference. See if we can untangle it.
        /knowable-bind (gloss-set-maybe-knowable result k m)
        /fn result
        /next state bs result))
    /dissect a-entry (list k a-v)
    ; TODO FORWARD: This use of
    ; `rider-and-gloss-update-maybe-knowable` is a forward reference.
    ; See if we can untangle it.
    /knowable-bind
      (rider-and-gloss-update-maybe-knowable
        (list (trivial) b)
        k
        (dissectfn (list (trivial) b-v-m)
          (known /list b-v-m /nothing)))
    /dissectfn (list b-v-m b)
    /knowable-bind (skm-union-knowable state k (just a-v) b-v-m)
    /dissectfn (list state m)
    ; TODO FORWARD: This use of `gloss-set-maybe-knowable` is a
    ; forward reference. See if we can untangle it.
    /knowable-bind (gloss-set-maybe-knowable result k m) /fn result
    /next state as b result)))

(define/own-contract (gloss-ref-maybe-knowable g k)
  (-> gloss? any/c (knowable/c maybe?))
  (dissect g (gloss rep)
  /mat rep (gloss-rep-empty) (known /nothing)
  /dissect rep (gloss-rep-glossesque _ tgs then else)
  /dissect tgs (tagged-glossesque-sys inhabitant? gs)
  /knowable-bind (call-knowable inhabitant? k) /fn k-inhabits?
  /if k-inhabits?
    (glossesque-sys-glossesque-ref-maybe-knowable gs then k)
    (gloss-ref-maybe-knowable else k)))

(define/own-contract (gloss-count g)
  (-> gloss? natural?)
  (dissect g (gloss rep)
  /mat rep (gloss-rep-empty) 0
  /dissect rep (gloss-rep-glossesque count tgs then else)
    count))

(define/own-contract (gloss-empty? g)
  (-> gloss? boolean?)
  (dissect g (gloss rep)
  /mat rep (gloss-rep-empty) #t
    #f))

(define (build-gloss tgs then else)
  (dissect tgs (tagged-glossesque-sys inhabitant? gs)
  /w- count
    (+ (glossesque-sys-glossesque-count gs then) (gloss-count else))
  /gloss /gloss-rep-glossesque count tgs then else))

(define/own-contract
  (rider-and-gloss-update-maybe-knowable
    rider-and-g k on-rider-and-m-knowable)
  (-> (list/c any/c gloss?) any/c
    (-> (list/c any/c maybe?) (knowable/c (list/c any/c maybe?)))
    (knowable/c (and/c any/c gloss?)))
  (dissect rider-and-g (list rider (gloss rep))
  /mat rep (gloss-rep-empty)
    (knowable-bind (on-rider-and-m-knowable /list rider /nothing)
    /dissectfn (list rider m)
    /expect m (just v) (known /list rider /gloss /gloss-rep-empty)
    /knowable-bind
      (custom-gloss-key-report-get-==-tagged-glossesque-sys-knowable
        (sequence-first
          (dynamic-type-get-custom-gloss-key-reports
            (any-dynamic-type)
            k)))
    /fn tgs
    /dissect tgs (tagged-glossesque-sys inhabitant? gs)
    /knowable-bind (call-knowable inhabitant? k) /fn k-inhabits?
    /expect k-inhabits? #t (unknown)
    /w- then (glossesque-sys-glossesque-union-of-zero gs)
    ; TODO FORWARD: This use of `gloss-set-maybe-knowable` is a
    ; forward reference. See if we can untangle it.
    /knowable-bind (gloss-set-maybe-knowable gs then k /just v)
    /fn then
    /known
      (list rider
        (gloss
          (gloss-rep-glossesque 1 tgs then (gloss-union-of-zero)))))
  /dissect rep (gloss-rep-glossesque _ tgs then else)
  /dissect tgs (tagged-glossesque-sys inhabitant? gs)
  /knowable-bind (call-knowable inhabitant? k) /fn k-inhabits?
  /if k-inhabits?
    (knowable-map
      (glossesque-sys-rider-and-glossesque-update-maybe-knowable
        gs (list rider then) k on-rider-and-m-knowable)
    /dissectfn (list rider then)
      (if (glossesque-sys-glossesque-empty? gs then)
        (list rider else)
        (list rider /build-gloss tgs then else)))
    (knowable-map
      (rider-and-gloss-update-maybe-knowable
        gs (list rider else) k on-rider-and-m-knowable)
    /dissectfn (list rider else)
      (list rider /build-gloss tgs then else))))

(define-imitation-simple-struct
  (gloss-glossesque-sys?)
  gloss-glossesque-sys
  'gloss-glossesque-sys (current-inspector) (auto-write) (auto-equal)
  
  (#:prop prop:glossesque-sys /make-glossesque-sys-impl
    
    #:glossesque-union-of-zero (fn gs /gloss-union-of-zero)
    
    #:glossesque-skm-union-of-two-knowable
    (fn gs state a b skm-union-knowable
      (gloss-skm-union-of-two-knowable state a b skm-union-knowable))
    
    #:glossesque-ref-maybe-knowable
    (fn gs g k /gloss-ref-maybe-knowable g k)
    
    #:rider-and-glossesque-update-maybe-knowable
    (fn gs rider-and-g k on-rider-and-m
      (rider-and-gloss-update-maybe-knowable
        rider-and-g k on-rider-and-m))
    
    #:glossesque-empty? (fn gs g /gloss-empty? g)
    #:glossesque-count (fn gs g /gloss-count g)
    
    #:glossesque-iteration-sequence
    (fn gs g /gloss-iteration-sequence g)
    
    )
  
  )

(define/own-contract (make-gloss-glossesque-sys)
  (-> glossesque-sys?)
  (gloss-glossesque-sys))

(define/own-contract (gloss-set-maybe-knowable g k m)
  (-> gloss? any/c maybe? (knowable/c gloss?))
  (w- gs (make-gloss-glossesque-sys)
  /glossesque-sys-glossesque-set-maybe-knowable gs g k m))


(define-imitation-simple-struct (uninformative-dynamic-type?)
  uninformative-dynamic-type-unguarded
  'uninformative-dynamic-type (current-inspector) (auto-write))

(define/own-contract (uninformative-dynamic-type)
  (-> any/c)
  (uninformative-dynamic-type-unguarded))

(define-imitation-simple-struct
  (dynamic-type-var-for-any-dynamic-type?)
  dynamic-type-var-for-any-dynamic-type
  'dynamic-type-var-for-any-dynamic-type (current-inspector)
  (auto-write)
  (auto-equal))
(ascribe-own-contract dynamic-type-var-for-any-dynamic-type?
  (-> any/c boolean?))

(define-imitation-simple-generics
  expressly-has-dynamic-type? expressly-has-dynamic-type-impl?
  (#:method expressly-has-dynamic-type-get-dynamic-type () (#:this))
  prop:expressly-has-dynamic-type make-expressly-has-dynamic-type-impl
  'expressly-has-dynamic-type 'expressly-has-dynamic-type-impl (list))
(ascribe-own-contract expressly-has-dynamic-type-impl?
  (-> any/c boolean?))
(ascribe-own-contract prop:expressly-has-dynamic-type
  (struct-type-property/c expressly-has-dynamic-type-impl?))
(ascribe-own-contract make-expressly-has-dynamic-type-impl
  (-> (-> gloss? any/c any/c) expressly-has-dynamic-type-impl?))

(define/own-contract (get-dynamic-type bindings v)
  (-> gloss? any/c any/c)
  (if (expressly-has-dynamic-type? v)
    (expressly-has-dynamic-type-get-dynamic-type bindings v)
    (uninformative-dynamic-type)))


(define-imitation-simple-generics
  smoosh-report? smoosh-report-impl?
  
  ; This says the operands' join, i.e. what result they smoosh into
  ; such that each operand satisfies `operand <= result`.
  ;
  (#:method smoosh-report-join-knowable-promise-maybe-knowable-promise
    (#:this))
  
  ; This says the operands' join, i.e. what result they smoosh into
  ; such that each operand satisfies `operand >= result`.
  ;
  (#:method smoosh-report-meet-knowable-promise-maybe-knowable-promise
    (#:this))
  
  ; This says how the operands smoosh along ==, in the sense of a
  ; result such that each operand satisfies both `operand <= result`
  ; and `operand >= result`.
  ;
  (#:method smoosh-report-==-knowable-promise-maybe-knowable-promise
    (#:this))
  
  ; This says how they smoosh along path-relatedness, in the sense of a
  ; result such that each operand is path-related to the result. Two
  ; elements are path-related if they're related by the transitive
  ; closure of `<= or >=`, or in other words, if there's a sequence of
  ; values `v1`, `v2`, ..., `vn` such that
  ; `((operand <= v1) or (operand >= v1)) and
  ; ((v1 <= v2) or (v1 >= v2)) and ... and
  ; ((vn <= result) or (vn >= result))`.
  ;
  ; If `<=` were an information ordering and `aInfo` and `bInfo` were
  ; the information lattice elements corresponding to two values `a`
  ; and `b`, and if the smooshing were to succeed (with a `just`),
  ; then `a` and `b` would be known to be related by the tightest
  ; possible equivalence relation that was still loose enough to have
  ; its truth value preserved when the values `a` and `b` were
  ; replaced with other values `a2` and `b2` for which additional
  ; information was known (i.e., `aInfo <= a2Info` and
  ; `bInfo <= b2Info`).
  ;
  ; Conceptually, the bottom element in an information ordering (if it
  ; exists) is related to *every* element, so what we're going to do
  ; is not have a bottom element in ours -- at least not one that lets
  ; us *know* it's a bottom element. More generally, we're not going
  ; to let any two values have a value that's known to have less
  ; information than one and less information than the other unless we
  ; want those two values to index the same entry in an
  ; information-equality-keyed table.
  ;
  (#:method
    smoosh-report-path-related-knowable-promise-maybe-knowable-promise
    (#:this))
  
  prop:smoosh-report make-smoosh-report-impl-from-various-unkeyworded
  'smoosh-report 'smoosh-report-impl (list))
(define smoosh-report-component/c
  (-> smoosh-report?
    (promise/c
      (knowable/c (maybe/c (promise/c (knowable/c any/c)))))))
(ascribe-own-contract smoosh-report? (-> any/c boolean?))
(ascribe-own-contract smoosh-report-impl? (-> any/c boolean?))
(ascribe-own-contract
  smoosh-report-join-knowable-promise-maybe-knowable-promise
  smoosh-report-component/c)
(ascribe-own-contract
  smoosh-report-meet-knowable-promise-maybe-knowable-promise
  smoosh-report-component/c)
(ascribe-own-contract
  smoosh-report-==-knowable-promise-maybe-knowable-promise
  smoosh-report-component/c)
(ascribe-own-contract
  smoosh-report-path-related-knowable-promise-maybe-knowable-promise
  smoosh-report-component/c)
(ascribe-own-contract prop:smoosh-report
  (struct-type-property/c smoosh-report-impl?))

(define/own-contract
  (make-smoosh-report-impl
    
    #:join-knowable-promise-maybe-knowable-promise
    join-knowable-promise-maybe-knowable-promise
    
    #:meet-knowable-promise-maybe-knowable-promise
    meet-knowable-promise-maybe-knowable-promise
    
    #:==-knowable-promise-maybe-knowable-promise
    ==-knowable-promise-maybe-knowable-promise
    
    #:path-related-knowable-promise-maybe-knowable-promise
    path-related-knowable-promise-maybe-knowable-promise)
  (->
    #:join-knowable-promise-maybe-knowable-promise
    smoosh-report-component/c
    
    #:meet-knowable-promise-maybe-knowable-promise
    smoosh-report-component/c
    
    #:==-knowable-promise-maybe-knowable-promise
    smoosh-report-component/c
    
    #:path-related-knowable-promise-maybe-knowable-promise
    smoosh-report-component/c
    
    smoosh-report-impl?)
  (make-smoosh-report-impl-from-various-unkeyworded
    join-knowable-promise-maybe-knowable-promise
    meet-knowable-promise-maybe-knowable-promise
    ==-knowable-promise-maybe-knowable-promise
    path-related-knowable-promise-maybe-knowable-promise))

(define-imitation-simple-struct
  (uninformative-smoosh-report?)
  uninformative-smoosh-report-unguarded
  'uninformative-smoosh-report (current-inspector) (auto-write)
  (#:prop prop:smoosh-report /make-smoosh-report-impl
    
    #:join-knowable-promise-maybe-knowable-promise
    (fn self
      (delay/strict /unknown))
    
    #:meet-knowable-promise-maybe-knowable-promise
    (fn self
      (delay/strict /unknown))
    
    #:==-knowable-promise-maybe-knowable-promise
    (fn self
      (delay/strict /unknown))
    
    #:path-related-knowable-promise-maybe-knowable-promise
    (fn self
      (delay/strict /unknown))
    
    ))

(define/own-contract (uninformative-smoosh-report)
  (-> smoosh-report?)
  (uninformative-smoosh-report-unguarded))

(define/own-contract (uninformative-smoosh-reports)
  (-> (sequence/c smoosh-report?))
  (in-cycle /list /uninformative-smoosh-report))

(define-imitation-simple-generics
  smoosh-and-comparison-of-two-report?
  smoosh-and-comparison-of-two-report-impl?
  ; This says whether they're related as (lhs <= rhs).
  (#:method smoosh-and-comparison-of-two-report-<=?-knowable-promise
    (#:this))
  ; This says whether they're related as (lhs >= rhs).
  (#:method smoosh-and-comparison-of-two-report->=?-knowable-promise
    (#:this))
  (#:method smoosh-and-comparison-of-two-report-get-smoosh-report
    (#:this))
  prop:smoosh-and-comparison-of-two-report
  make-smoosh-and-comparison-of-two-report-impl-from-various-unkeyworded
  'smoosh-and-comparison-of-two-report
  'smoosh-and-comparison-of-two-report-impl
  (list))
(ascribe-own-contract smoosh-and-comparison-of-two-report?
  (-> any/c boolean?))
(ascribe-own-contract smoosh-and-comparison-of-two-report-impl?
  (-> any/c boolean?))
(ascribe-own-contract
  smoosh-and-comparison-of-two-report-<=?-knowable-promise
  (-> smoosh-and-comparison-of-two-report?
    (promise/c (knowable/c boolean?))))
(ascribe-own-contract
  smoosh-and-comparison-of-two-report->=?-knowable-promise
  (-> smoosh-and-comparison-of-two-report?
    (promise/c (knowable/c boolean?))))
(ascribe-own-contract
  smoosh-and-comparison-of-two-report-get-smoosh-report
  (-> smoosh-and-comparison-of-two-report? smoosh-report?))
(ascribe-own-contract prop:smoosh-and-comparison-of-two-report
  (struct-type-property/c smoosh-and-comparison-of-two-report-impl?))

(define/own-contract
  (make-smoosh-and-comparison-of-two-report-impl
    #:<=?-knowable-promise <=?-knowable-promise
    #:>=?-knowable-promise >=?-knowable-promise
    #:get-smoosh-report get-smoosh-report)
  (->
    #:<=?-knowable-promise
    (-> smoosh-and-comparison-of-two-report?
      (promise/c (knowable/c boolean?)))
    
    #:>=?-knowable-promise
    (-> smoosh-and-comparison-of-two-report?
      (promise/c (knowable/c boolean?)))
    
    #:get-smoosh-report
    (-> smoosh-and-comparison-of-two-report? smoosh-report?)
    
    smoosh-report-impl?)
  (make-smoosh-and-comparison-of-two-report-impl-from-various-unkeyworded
    <=?-knowable-promise
    >=?-knowable-promise
    get-smoosh-report))

(define-imitation-simple-struct
  (uninformative-smoosh-and-comparison-of-two-report?)
  uninformative-smoosh-and-comparison-of-two-report-unguarded
  'uninformative-smoosh-and-comparison-of-two-report
  (current-inspector)
  (auto-write)
  (#:prop prop:smoosh-and-comparison-of-two-report
    (make-smoosh-and-comparison-of-two-report-impl
      
      #:<=?-knowable-promise
      (fn self
        (delay/strict /unknown))
      
      #:>=?-knowable-promise
      (fn self
        (delay/strict /unknown))
      
      #:get-smoosh-report
      (fn self
        (uninformative-smoosh-report))
      
      )))

(define/own-contract
  (uninformative-smoosh-and-comparison-of-two-report)
  (-> smoosh-and-comparison-of-two-report?)
  (uninformative-smoosh-and-comparison-of-two-report-unguarded))

(define/own-contract
  (uninformative-smoosh-and-comparison-of-two-reports)
  (-> (sequence/c smoosh-and-comparison-of-two-report?))
  (in-cycle /list /uninformative-smoosh-and-comparison-of-two-report))


(define-imitation-simple-generics
  expressly-smooshable-dynamic-type?
  expressly-smooshable-dynamic-type-impl?
  (#:method
    expressly-smooshable-dynamic-type-get-smoosh-of-zero-reports
    (#:this))
  (#:method
    expressly-smooshable-dynamic-type-get-smoosh-of-one-reports
    (#:this)
    ())
  (#:method
    expressly-smooshable-dynamic-type-get-smoosh-and-comparison-of-two-reports
    (#:this)
    ()
    ())
  (#:method
    expressly-smooshable-dynamic-type-get-smoosh-and-comparison-of-two-reports-via-second
    (#:this)
    ()
    ())
  prop:expressly-smooshable-dynamic-type
  make-expressly-smooshable-dynamic-type-impl-from-various-unkeyworded
  'expressly-smooshable-dynamic-type
  'expressly-smooshable-dynamic-type-impl
  (list))
(ascribe-own-contract expressly-smooshable-dynamic-type-impl?
  (-> any/c boolean?))
(ascribe-own-contract prop:expressly-smooshable-dynamic-type
  (struct-type-property/c expressly-smooshable-dynamic-type-impl?))

(define/own-contract
  (make-expressly-smooshable-dynamic-type-impl
    #:get-smoosh-of-zero-reports get-smoosh-of-zero-reports
    #:get-smoosh-of-one-reports get-smoosh-of-one-reports
    
    #:get-smoosh-and-comparison-of-two-reports
    get-smoosh-and-comparison-of-two-reports
    
    #:get-smoosh-and-comparison-of-two-reports-via-second
    [ get-smoosh-and-comparison-of-two-reports-via-second
      (fn dt a b
        (get-smoosh-and-comparison-of-two-reports dt a b))])
  (->*
    (
      #:get-smoosh-of-zero-reports
      (-> any/c (sequence/c smoosh-report?))
      
      #:get-smoosh-of-one-reports
      (-> any/c any/c (sequence/c smoosh-report?))
      
      #:get-smoosh-and-comparison-of-two-reports
      (-> any/c any/c any/c
        (sequence/c smoosh-and-comparison-of-two-report?))
      
      )
    (
      #:get-smoosh-and-comparison-of-two-reports-via-second
      (-> any/c any/c any/c
        (sequence/c smoosh-and-comparison-of-two-report?))
      
      )
    expressly-smooshable-dynamic-type-impl?)
  (make-expressly-smooshable-dynamic-type-impl-from-various-unkeyworded
    get-smoosh-of-zero-reports
    get-smoosh-of-one-reports
    get-smoosh-and-comparison-of-two-reports
    get-smoosh-and-comparison-of-two-reports-via-second))

(define/own-contract (dynamic-type-get-smoosh-of-zero-reports dt)
  (-> any/c
    ; Each report in the infinite sequence gives the smoosh identity
    ; elements, first for the type's bespoke notion of ordering, then
    ; for the information ordering, then for the information ordering
    ; of the information ordering representatives, and so on.
    (sequence/c smoosh-report?))
  (if (expressly-smooshable-dynamic-type? dt)
    (expressly-smooshable-dynamic-type-get-smoosh-of-zero-reports dt)
    (uninformative-smoosh-reports)))

(define/own-contract (dynamic-type-get-smoosh-of-one-reports a-dt a)
  (-> any/c any/c (sequence/c smoosh-report?))
  (if (expressly-smooshable-dynamic-type? a-dt)
    (expressly-smooshable-dynamic-type-get-smoosh-of-one-reports
      a-dt a)
    (uninformative-smoosh-reports)))

(define/own-contract
  (dynamic-type-get-smoosh-and-comparison-of-two-reports dt a b)
  (-> any/c any/c any/c
    ; For each report in the infinite sequence, the next report says
    ; not only whether they smoosh along that one's == but also, only
    ; if they do, how their information ordering representatives
    ; smoosh along their information ordering.
    (sequence/c smoosh-and-comparison-of-two-report?))
  (if (expressly-smooshable-dynamic-type? dt)
    (expressly-smooshable-dynamic-type-get-smoosh-and-comparison-of-two-reports
      dt a b)
    (uninformative-smoosh-and-comparison-of-two-reports)))

(define/own-contract
  (dynamic-type-get-smoosh-and-comparison-of-two-reports-via-second dt a b)
  (-> any/c any/c any/c
    ; For each report in the infinite sequence, the next report says
    ; not only whether they smoosh along that one's == but also, only
    ; if they do, how their information ordering representatives
    ; smoosh along their information ordering.
    (sequence/c smoosh-report?))
  (if (expressly-smooshable-dynamic-type? dt)
    (expressly-smooshable-dynamic-type-get-smoosh-and-comparison-of-two-reports-via-second
      dt a b)
    (uninformative-smoosh-and-comparison-of-two-reports)))


(define/own-contract (uninformative-hash-code)
  (-> fixnum?)
  0)


(define-imitation-simple-generics
  smoosh-equal-hash-code-support-report?
  smoosh-equal-hash-code-support-report-impl?
  
  ; This says the `equal-hash-code` (if the given boolean is `#t`) or
  ; `equal-always-hash-code` (if the given boolean is `#f`) of the
  ; value.
  (#:method smoosh-equal-hash-code-support-report-==-hash-code-promise
    (#:this)
    ())
  
  ; This says the `equal-hash-code` (if the given boolean is `#t`) or
  ; `equal-always-hash-code` (if the given boolean is `#f`) of a
  ; `path-related-wrapper` around the value.
  (#:method
    smoosh-equal-hash-code-support-report-path-related-hash-code-promise
    (#:this)
    ())
  
  prop:smoosh-equal-hash-code-support-report
  make-smoosh-equal-hash-code-support-report-impl-from-various-unkeyworded
  'smoosh-equal-hash-code-support-report
  'smoosh-equal-hash-code-support-report-impl (list))
(define smoosh-equal-hash-code-support-report-component/c
  (-> smoosh-equal-hash-code-support-report? boolean?
    (promise/c fixnum?)))
(ascribe-own-contract smoosh-equal-hash-code-support-report?
  (-> any/c boolean?))
(ascribe-own-contract smoosh-equal-hash-code-support-report-impl?
  (-> any/c boolean?))
(ascribe-own-contract
  smoosh-equal-hash-code-support-report-==-hash-code-promise
  smoosh-equal-hash-code-support-report-component/c)
(ascribe-own-contract
  smoosh-equal-hash-code-support-report-path-related-hash-code-promise
  smoosh-equal-hash-code-support-report-component/c)
(ascribe-own-contract prop:smoosh-equal-hash-code-support-report
  (struct-type-property/c
    smoosh-equal-hash-code-support-report-impl?))

(define/own-contract
  (make-smoosh-equal-hash-code-support-report-impl
    #:==-hash-code-promise ==-hash-code-promise
    #:path-related-hash-code-promise path-related-hash-code-promise)
  (->
    #:==-hash-code-promise
    smoosh-equal-hash-code-support-report-component/c
    
    #:path-related-hash-code-promise
    smoosh-equal-hash-code-support-report-component/c
    
    smoosh-equal-hash-code-support-report-impl?)
  (make-smoosh-equal-hash-code-support-report-impl-from-various-unkeyworded
    ==-hash-code-promise
    path-related-hash-code-promise))

(define-imitation-simple-struct
  (uninformative-smoosh-equal-hash-code-support-report?)
  uninformative-smoosh-equal-hash-code-support-report-unguarded
  'uninformative-smoosh-equal-hash-code-support-report (current-inspector) (auto-write)
  (#:prop prop:smoosh-equal-hash-code-support-report
    (make-smoosh-equal-hash-code-support-report-impl
      
      #:==-hash-code-promise
      (fn self now?
        (delay/strict /uninformative-hash-code))
      
      #:path-related-hash-code-promise
      (fn self now?
        (delay/strict /uninformative-hash-code))
      
      )))

(define/own-contract
  (uninformative-smoosh-equal-hash-code-support-report)
  (-> smoosh-equal-hash-code-support-report?)
  (uninformative-smoosh-equal-hash-code-support-report-unguarded))

(define/own-contract
  (uninformative-smoosh-equal-hash-code-support-reports)
  (-> (sequence/c smoosh-equal-hash-code-support-report?))
  (in-cycle /list
    (uninformative-smoosh-equal-hash-code-support-report)))


(define-imitation-simple-generics
  expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type?
  expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type-impl?
  (#:method
    expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type-get-smoosh-equal-hash-code-support-reports
    (#:this)
    ())
  prop:expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type
  make-expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type-impl-from-various-unkeyworded
  'expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type
  'expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type-impl
  (list))
(ascribe-own-contract
  expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type-impl?
  (-> any/c boolean?))
(ascribe-own-contract
  prop:expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type
  (struct-type-property/c
    expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type-impl?))

(define/own-contract
  (make-expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type-impl
    #:get-smoosh-equal-hash-code-support-reports
    get-smoosh-equal-hash-code-support-reports)
  (->
    #:get-smoosh-equal-hash-code-support-reports
    (-> any/c any/c (sequence/c smoosh-report?))
    
    expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type-impl?)
  (make-expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type-impl-from-various-unkeyworded
    get-smoosh-equal-hash-code-support-reports))

(define/own-contract
  (dynamic-type-get-smoosh-equal-hash-code-support-reports a-dt a)
  (-> any/c any/c (sequence/c smoosh-report?))
  (if
    (expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type?
      a-dt)
    (expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type-get-smoosh-equal-hash-code-support-reports
      a-dt a)
    (uninformative-smoosh-equal-hash-code-support-reports)))


(define-imitation-simple-struct
  (mapped-smoosh-report?
    mapped-smoosh-report-on-join-knowable-promise-maybe-knowable-promise
    mapped-smoosh-report-on-meet-knowable-promise-maybe-knowable-promise
    mapped-smoosh-report-on-==-knowable-promise-maybe-knowable-promise
    mapped-smoosh-report-on-path-related-knowable-promise-maybe-knowable-promise
    mapped-smoosh-report-original)
  mapped-smoosh-report 'mapped-smoosh-report (current-inspector)
  (auto-write)
  (#:prop prop:smoosh-report /make-smoosh-report-impl
    
    #:join-knowable-promise-maybe-knowable-promise
    (dissectfn
      (mapped-smoosh-report
        on-join-knowable-promise-maybe-knowable-promise
        on-meet-knowable-promise-maybe-knowable-promise
        on-==-knowable-promise-maybe-knowable-promise
        on-path-related-knowable-promise-maybe-knowable-promise
        original)
      (on-join-knowable-promise-maybe-knowable-promise
        (smoosh-report-join-knowable-promise-maybe-knowable-promise
          original)))
    
    #:meet-knowable-promise-maybe-knowable-promise
    (dissectfn
      (mapped-smoosh-report
        on-join-knowable-promise-maybe-knowable-promise
        on-meet-knowable-promise-maybe-knowable-promise
        on-==-knowable-promise-maybe-knowable-promise
        on-path-related-knowable-promise-maybe-knowable-promise
        original)
      (on-meet-knowable-promise-maybe-knowable-promise
        (smoosh-report-meet-knowable-promise-maybe-knowable-promise
          original)))
    
    #:==-knowable-promise-maybe-knowable-promise
    (dissectfn
      (mapped-smoosh-report
        on-join-knowable-promise-maybe-knowable-promise
        on-meet-knowable-promise-maybe-knowable-promise
        on-==-knowable-promise-maybe-knowable-promise
        on-path-related-knowable-promise-maybe-knowable-promise
        original)
      (on-==-knowable-promise-maybe-knowable-promise
        (smoosh-report-==-knowable-promise-maybe-knowable-promise
          original)))
    
    #:path-related-knowable-promise-maybe-knowable-promise
    (dissectfn
      (mapped-smoosh-report
        on-join-knowable-promise-maybe-knowable-promise
        on-meet-knowable-promise-maybe-knowable-promise
        on-==-knowable-promise-maybe-knowable-promise
        on-path-related-knowable-promise-maybe-knowable-promise
        original)
      (on-path-related-knowable-promise-maybe-knowable-promise
        (smoosh-report-path-related-knowable-promise-maybe-knowable-promise
          original)))
    
    ))

(define/own-contract
  (smoosh-report-map report
    
    #:on-smoosh-result-knowable-promise-maybe-knowable-promise
    [ on-smoosh-result-knowable-promise-maybe-knowable-promise
      (fn kpmkp
        kpmkp)]
    
    #:on-join-knowable-promise-maybe-knowable-promise
    [ on-join-knowable-promise-maybe-knowable-promise
      on-smoosh-result-knowable-promise-maybe-knowable-promise]
    
    #:on-meet-knowable-promise-maybe-knowable-promise
    [ on-meet-knowable-promise-maybe-knowable-promise
      on-smoosh-result-knowable-promise-maybe-knowable-promise]
    
    #:on-==-knowable-promise-maybe-knowable-promise
    [ on-==-knowable-promise-maybe-knowable-promise
      on-smoosh-result-knowable-promise-maybe-knowable-promise]
    
    #:on-path-related-knowable-promise-maybe-knowable-promise
    [ on-path-related-knowable-promise-maybe-knowable-promise
      on-smoosh-result-knowable-promise-maybe-knowable-promise]
    
    )
  (->*
    (smoosh-report?)
    (
      #:on-smoosh-result-knowable-promise-maybe-knowable-promise
      (-> (promise/c (knowable/c (maybe/c (promise/c knowable?))))
        (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
      
      #:on-join-knowable-promise-maybe-knowable-promise
      (-> (promise/c (knowable/c (maybe/c (promise/c knowable?))))
        (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
      
      #:on-meet-knowable-promise-maybe-knowable-promise
      (-> (promise/c (knowable/c (maybe/c (promise/c knowable?))))
        (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
      
      #:on-==-knowable-promise-maybe-knowable-promise
      (-> (promise/c (knowable/c (maybe/c (promise/c knowable?))))
        (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
      
      #:on-path-related-knowable-promise-maybe-knowable-promise
      (-> (promise/c (knowable/c (maybe/c (promise/c knowable?))))
        (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
      
      )
    smoosh-report?)
  (mapped-smoosh-report
    on-join-knowable-promise-maybe-knowable-promise
    on-meet-knowable-promise-maybe-knowable-promise
    on-==-knowable-promise-maybe-knowable-promise
    on-path-related-knowable-promise-maybe-knowable-promise
    report))

(define/own-contract
  (smoosh-reports-map reports
    
    #:on-smoosh-result-knowable-promise-maybe-knowable-promise
    [ on-smoosh-result-knowable-promise-maybe-knowable-promise
      (fn kpmkp
        kpmkp)]
    
    #:on-join-knowable-promise-maybe-knowable-promise
    [ on-join-knowable-promise-maybe-knowable-promise
      on-smoosh-result-knowable-promise-maybe-knowable-promise]
    
    #:on-meet-knowable-promise-maybe-knowable-promise
    [ on-meet-knowable-promise-maybe-knowable-promise
      on-smoosh-result-knowable-promise-maybe-knowable-promise]
    
    #:on-==-knowable-promise-maybe-knowable-promise
    [ on-==-knowable-promise-maybe-knowable-promise
      on-smoosh-result-knowable-promise-maybe-knowable-promise]
    
    #:on-path-related-knowable-promise-maybe-knowable-promise
    [ on-path-related-knowable-promise-maybe-knowable-promise
      on-smoosh-result-knowable-promise-maybe-knowable-promise]
    
    )
  (->*
    ((sequence/c smoosh-report?))
    (
      #:on-smoosh-result-knowable-promise-maybe-knowable-promise
      (-> (promise/c (knowable/c (maybe/c (promise/c knowable?))))
        (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
      
      #:on-join-knowable-promise-maybe-knowable-promise
      (-> (promise/c (knowable/c (maybe/c (promise/c knowable?))))
        (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
      
      #:on-meet-knowable-promise-maybe-knowable-promise
      (-> (promise/c (knowable/c (maybe/c (promise/c knowable?))))
        (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
      
      #:on-==-knowable-promise-maybe-knowable-promise
      (-> (promise/c (knowable/c (maybe/c (promise/c knowable?))))
        (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
      
      #:on-path-related-knowable-promise-maybe-knowable-promise
      (-> (promise/c (knowable/c (maybe/c (promise/c knowable?))))
        (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
      
      )
    (sequence/c smoosh-report?))
  (sequence-map
    (fn report
      (smoosh-report-map report
        
        #:on-join-knowable-promise-maybe-knowable-promise
        on-join-knowable-promise-maybe-knowable-promise
        
        #:on-meet-knowable-promise-maybe-knowable-promise
        on-meet-knowable-promise-maybe-knowable-promise
        
        #:on-==-knowable-promise-maybe-knowable-promise
        on-==-knowable-promise-maybe-knowable-promise
        
        #:on-path-related-knowable-promise-maybe-knowable-promise
        on-path-related-knowable-promise-maybe-knowable-promise
        
        ))
    reports))

(define-imitation-simple-struct
  (zip*-mapped-smoosh-report?
    zip*-mapped-smoosh-report-on-join-knowable-promise-maybe-knowable-promise
    zip*-mapped-smoosh-report-on-meet-knowable-promise-maybe-knowable-promise
    zip*-mapped-smoosh-report-on-==-knowable-promise-maybe-knowable-promise
    zip*-mapped-smoosh-report-on-path-related-knowable-promise-maybe-knowable-promise
    zip*-mapped-smoosh-report-original-list)
  zip*-mapped-smoosh-report
  'zip*-mapped-smoosh-report (current-inspector)
  (auto-write)
  (#:prop prop:smoosh-report /make-smoosh-report-impl
    
    #:join-knowable-promise-maybe-knowable-promise
    (dissectfn
      (zip*-mapped-smoosh-report
        on-join-knowable-promise-maybe-knowable-promise
        on-meet-knowable-promise-maybe-knowable-promise
        on-==-knowable-promise-maybe-knowable-promise
        on-path-related-knowable-promise-maybe-knowable-promise
        original-list)
      (on-join-knowable-promise-maybe-knowable-promise
        (list-map original-list /fn original
          (smoosh-report-join-knowable-promise-maybe-knowable-promise
            original))))
    
    #:meet-knowable-promise-maybe-knowable-promise
    (dissectfn
      (zip*-mapped-smoosh-report
        on-join-knowable-promise-maybe-knowable-promise
        on-meet-knowable-promise-maybe-knowable-promise
        on-==-knowable-promise-maybe-knowable-promise
        on-path-related-knowable-promise-maybe-knowable-promise
        original-list)
      (on-meet-knowable-promise-maybe-knowable-promise
        (list-map original-list /fn original
          (smoosh-report-meet-knowable-promise-maybe-knowable-promise
            original))))
    
    #:==-knowable-promise-maybe-knowable-promise
    (dissectfn
      (zip*-mapped-smoosh-report
        on-join-knowable-promise-maybe-knowable-promise
        on-meet-knowable-promise-maybe-knowable-promise
        on-==-knowable-promise-maybe-knowable-promise
        on-path-related-knowable-promise-maybe-knowable-promise
        original-list)
      (on-==-knowable-promise-maybe-knowable-promise
        (list-map original-list /fn original
          (smoosh-report-==-knowable-promise-maybe-knowable-promise
            original))))
    
    #:path-related-knowable-promise-maybe-knowable-promise
    (dissectfn
      (zip*-mapped-smoosh-report
        on-join-knowable-promise-maybe-knowable-promise
        on-meet-knowable-promise-maybe-knowable-promise
        on-==-knowable-promise-maybe-knowable-promise
        on-path-related-knowable-promise-maybe-knowable-promise
        original-list)
      (on-path-related-knowable-promise-maybe-knowable-promise
        (list-map original-list /fn original
          (smoosh-report-path-related-knowable-promise-maybe-knowable-promise
            original))))
    
    ))

(define/own-contract
  (smoosh-report-zip*-map report-list
    
    #:on-smoosh-result-knowable-promise-maybe-knowable-promise
    [ on-smoosh-result-knowable-promise-maybe-knowable-promise
      (fn kpmkp-list
        (raise-arguments-error 'smoosh-report-zip*-map
          "tried to retrieve a smoosh result when its mapping behavior was undefined"
          
          "smoosh-result-knowable-promise-maybe-knowable-promise-list"
          kpmkp-list
          
          ))]
    
    #:on-join-knowable-promise-maybe-knowable-promise
    [ on-join-knowable-promise-maybe-knowable-promise
      on-smoosh-result-knowable-promise-maybe-knowable-promise]
    
    #:on-meet-knowable-promise-maybe-knowable-promise
    [ on-meet-knowable-promise-maybe-knowable-promise
      on-smoosh-result-knowable-promise-maybe-knowable-promise]
    
    #:on-==-knowable-promise-maybe-knowable-promise
    [ on-==-knowable-promise-maybe-knowable-promise
      on-smoosh-result-knowable-promise-maybe-knowable-promise]
    
    #:on-path-related-knowable-promise-maybe-knowable-promise
    [ on-path-related-knowable-promise-maybe-knowable-promise
      on-smoosh-result-knowable-promise-maybe-knowable-promise]
    
    )
  (->*
    ((listof smoosh-report?))
    (
      #:on-smoosh-result-knowable-promise-maybe-knowable-promise
      (->
        (listof
          (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
        (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
      
      #:on-join-knowable-promise-maybe-knowable-promise
      (->
        (listof
          (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
        (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
      
      #:on-meet-knowable-promise-maybe-knowable-promise
      (->
        (listof
          (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
        (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
      
      #:on-==-knowable-promise-maybe-knowable-promise
      (->
        (listof
          (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
        (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
      
      #:on-path-related-knowable-promise-maybe-knowable-promise
      (->
        (listof
          (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
        (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
      
      )
    smoosh-report?)
  (zip*-mapped-smoosh-report
    on-join-knowable-promise-maybe-knowable-promise
    on-meet-knowable-promise-maybe-knowable-promise
    on-==-knowable-promise-maybe-knowable-promise
    on-path-related-knowable-promise-maybe-knowable-promise
    report-list))

(define/own-contract
  (smoosh-reports-zip*-map reports-list
    
    #:on-smoosh-result-knowable-promise-maybe-knowable-promise
    [ on-smoosh-result-knowable-promise-maybe-knowable-promise
      (fn kpmkp-list
        (raise-arguments-error 'smoosh-reports-zip*-map
          "tried to retrieve a smoosh result when its mapping behavior was undefined"
          
          "smoosh-result-knowable-promise-maybe-knowable-promise-list"
          kpmkp-list
          
          ))]
    
    #:on-join-knowable-promise-maybe-knowable-promise
    [ on-join-knowable-promise-maybe-knowable-promise
      on-smoosh-result-knowable-promise-maybe-knowable-promise]
    
    #:on-meet-knowable-promise-maybe-knowable-promise
    [ on-meet-knowable-promise-maybe-knowable-promise
      on-smoosh-result-knowable-promise-maybe-knowable-promise]
    
    #:on-==-knowable-promise-maybe-knowable-promise
    [ on-==-knowable-promise-maybe-knowable-promise
      on-smoosh-result-knowable-promise-maybe-knowable-promise]
    
    #:on-path-related-knowable-promise-maybe-knowable-promise
    [ on-path-related-knowable-promise-maybe-knowable-promise
      on-smoosh-result-knowable-promise-maybe-knowable-promise]
    
    )
  (->*
    ((listof (sequence/c smoosh-report?)))
    (
      #:on-smoosh-result-knowable-promise-maybe-knowable-promise
      (->
        (listof
          (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
        (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
      
      #:on-join-knowable-promise-maybe-knowable-promise
      (->
        (listof
          (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
        (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
      
      #:on-meet-knowable-promise-maybe-knowable-promise
      (->
        (listof
          (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
        (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
      
      #:on-==-knowable-promise-maybe-knowable-promise
      (->
        (listof
          (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
        (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
      
      #:on-path-related-knowable-promise-maybe-knowable-promise
      (->
        (listof
          (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
        (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
      
      )
    (sequence/c smoosh-report?))
  (sequence-zip*-map reports-list /fn report-list
    (smoosh-report-zip*-map report-list
      
      #:on-join-knowable-promise-maybe-knowable-promise
      on-join-knowable-promise-maybe-knowable-promise
      
      #:on-meet-knowable-promise-maybe-knowable-promise
      on-meet-knowable-promise-maybe-knowable-promise
      
      #:on-==-knowable-promise-maybe-knowable-promise
      on-==-knowable-promise-maybe-knowable-promise
      
      #:on-path-related-knowable-promise-maybe-knowable-promise
      on-path-related-knowable-promise-maybe-knowable-promise)))

(define-imitation-simple-struct
  (mapped-smoosh-and-comparison-of-two-report?
    mapped-smoosh-and-comparison-of-two-report-on-<=?-knowable-promise
    mapped-smoosh-and-comparison-of-two-report-on->=?-knowable-promise
    mapped-smoosh-and-comparison-of-two-report-on-join-knowable-promise-maybe-knowable-promise
    mapped-smoosh-and-comparison-of-two-report-on-meet-knowable-promise-maybe-knowable-promise
    mapped-smoosh-and-comparison-of-two-report-on-==-knowable-promise-maybe-knowable-promise
    mapped-smoosh-and-comparison-of-two-report-on-path-related-knowable-promise-maybe-knowable-promise
    mapped-smoosh-and-comparison-of-two-report-original)
  mapped-smoosh-and-comparison-of-two-report
  'mapped-smoosh-and-comparison-of-two-report (current-inspector)
  (auto-write)
  (#:prop prop:smoosh-and-comparison-of-two-report
    (make-smoosh-and-comparison-of-two-report-impl
      
      #:<=?-knowable-promise
      (dissectfn
        (mapped-smoosh-and-comparison-of-two-report
          on-<=?-knowable-promise
          on->=?-knowable-promise
          on-join-knowable-promise-maybe-knowable-promise
          on-meet-knowable-promise-maybe-knowable-promise
          on-==-knowable-promise-maybe-knowable-promise
          on-path-related-knowable-promise-maybe-knowable-promise
          original)
        (on-<=?-knowable-promise
          (smoosh-and-comparison-of-two-report-<=?-knowable-promise
            original)))
      
      #:>=?-knowable-promise
      (dissectfn
        (mapped-smoosh-and-comparison-of-two-report
          on-<=?-knowable-promise
          on->=?-knowable-promise
          on-join-knowable-promise-maybe-knowable-promise
          on-meet-knowable-promise-maybe-knowable-promise
          on-==-knowable-promise-maybe-knowable-promise
          on-path-related-knowable-promise-maybe-knowable-promise
          original)
        (on->=?-knowable-promise
          (smoosh-and-comparison-of-two-report->=?-knowable-promise
            original)))
      
      #:get-smoosh-report
      (dissectfn
        (mapped-smoosh-and-comparison-of-two-report
          on-<=?-knowable-promise
          on->=?-knowable-promise
          on-join-knowable-promise-maybe-knowable-promise
          on-meet-knowable-promise-maybe-knowable-promise
          on-==-knowable-promise-maybe-knowable-promise
          on-path-related-knowable-promise-maybe-knowable-promise
          original)
        (smoosh-report-map
          (smoosh-and-comparison-of-two-report-get-smoosh-report
            original)
          
          #:on-join-knowable-promise-maybe-knowable-promise
          on-join-knowable-promise-maybe-knowable-promise
          
          #:on-meet-knowable-promise-maybe-knowable-promise
          on-meet-knowable-promise-maybe-knowable-promise
          
          #:on-==-knowable-promise-maybe-knowable-promise
          on-==-knowable-promise-maybe-knowable-promise
          
          #:on-path-related-knowable-promise-maybe-knowable-promise
          on-path-related-knowable-promise-maybe-knowable-promise
          
          ))
      
      )))

(define/own-contract
  (smoosh-and-comparison-of-two-report-map report
    
    #:on-check-result-knowable-promise
    [ on-check-result-knowable-promise
      (fn kp
        kp)]
    
    #:on-<=?-knowable-promise
    [on-<=?-knowable-promise on-check-result-knowable-promise]
    
    #:on->=?-knowable-promise
    [on->=?-knowable-promise on-check-result-knowable-promise]
    
    #:on-smoosh-result-knowable-promise-maybe-knowable-promise
    [ on-smoosh-result-knowable-promise-maybe-knowable-promise
      (fn kpmkp
        kpmkp)]
    
    #:on-join-knowable-promise-maybe-knowable-promise
    [ on-join-knowable-promise-maybe-knowable-promise
      on-smoosh-result-knowable-promise-maybe-knowable-promise]
    
    #:on-meet-knowable-promise-maybe-knowable-promise
    [ on-meet-knowable-promise-maybe-knowable-promise
      on-smoosh-result-knowable-promise-maybe-knowable-promise]
    
    #:on-==-knowable-promise-maybe-knowable-promise
    [ on-==-knowable-promise-maybe-knowable-promise
      on-smoosh-result-knowable-promise-maybe-knowable-promise]
    
    #:on-path-related-knowable-promise-maybe-knowable-promise
    [ on-path-related-knowable-promise-maybe-knowable-promise
      on-smoosh-result-knowable-promise-maybe-knowable-promise]
    
    )
  (->*
    (smoosh-and-comparison-of-two-report?)
    (
      #:on-check-result-knowable-promise
      (-> (promise/c (knowable/c boolean?))
        (promise/c (knowable/c boolean?)))
      
      #:on-<=?-knowable-promise
      (-> (promise/c (knowable/c boolean?))
        (promise/c (knowable/c boolean?)))
      
      #:on->=?-knowable-promise
      (-> (promise/c (knowable/c boolean?))
        (promise/c (knowable/c boolean?)))
      
      #:on-smoosh-result-knowable-promise-maybe-knowable-promise
      (-> (promise/c (knowable/c (maybe/c (promise/c knowable?))))
        (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
      
      #:on-join-knowable-promise-maybe-knowable-promise
      (-> (promise/c (knowable/c (maybe/c (promise/c knowable?))))
        (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
      
      #:on-meet-knowable-promise-maybe-knowable-promise
      (-> (promise/c (knowable/c (maybe/c (promise/c knowable?))))
        (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
      
      #:on-==-knowable-promise-maybe-knowable-promise
      (-> (promise/c (knowable/c (maybe/c (promise/c knowable?))))
        (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
      
      #:on-path-related-knowable-promise-maybe-knowable-promise
      (-> (promise/c (knowable/c (maybe/c (promise/c knowable?))))
        (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
      
      )
    smoosh-and-comparison-of-two-report?)
  (mapped-smoosh-and-comparison-of-two-report
    on-<=?-knowable-promise
    on->=?-knowable-promise
    on-join-knowable-promise-maybe-knowable-promise
    on-meet-knowable-promise-maybe-knowable-promise
    on-==-knowable-promise-maybe-knowable-promise
    on-path-related-knowable-promise-maybe-knowable-promise
    report))

(define/own-contract
  (smoosh-and-comparison-of-two-reports-map reports
    
    #:on-check-result-knowable-promise
    [ on-check-result-knowable-promise
      (fn kp
        kp)]
    
    #:on-<=?-knowable-promise
    [on-<=?-knowable-promise on-check-result-knowable-promise]
    
    #:on->=?-knowable-promise
    [on->=?-knowable-promise on-check-result-knowable-promise]
    
    #:on-smoosh-result-knowable-promise-maybe-knowable-promise
    [ on-smoosh-result-knowable-promise-maybe-knowable-promise
      (fn kpmkp
        kpmkp)]
    
    #:on-join-knowable-promise-maybe-knowable-promise
    [ on-join-knowable-promise-maybe-knowable-promise
      on-smoosh-result-knowable-promise-maybe-knowable-promise]
    
    #:on-meet-knowable-promise-maybe-knowable-promise
    [ on-meet-knowable-promise-maybe-knowable-promise
      on-smoosh-result-knowable-promise-maybe-knowable-promise]
    
    #:on-==-knowable-promise-maybe-knowable-promise
    [ on-==-knowable-promise-maybe-knowable-promise
      on-smoosh-result-knowable-promise-maybe-knowable-promise]
    
    #:on-path-related-knowable-promise-maybe-knowable-promise
    [ on-path-related-knowable-promise-maybe-knowable-promise
      on-smoosh-result-knowable-promise-maybe-knowable-promise]
    
    )
  (->*
    ((sequence/c smoosh-and-comparison-of-two-report?))
    (
      #:on-check-result-knowable-promise
      (-> (promise/c (knowable/c boolean?))
        (promise/c (knowable/c boolean?)))
      
      #:on-<=?-knowable-promise
      (-> (promise/c (knowable/c boolean?))
        (promise/c (knowable/c boolean?)))
      
      #:on->=?-knowable-promise
      (-> (promise/c (knowable/c boolean?))
        (promise/c (knowable/c boolean?)))
      
      #:on-smoosh-result-knowable-promise-maybe-knowable-promise
      (-> (promise/c (knowable/c (maybe/c (promise/c knowable?))))
        (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
      
      #:on-join-knowable-promise-maybe-knowable-promise
      (-> (promise/c (knowable/c (maybe/c (promise/c knowable?))))
        (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
      
      #:on-meet-knowable-promise-maybe-knowable-promise
      (-> (promise/c (knowable/c (maybe/c (promise/c knowable?))))
        (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
      
      #:on-==-knowable-promise-maybe-knowable-promise
      (-> (promise/c (knowable/c (maybe/c (promise/c knowable?))))
        (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
      
      #:on-path-related-knowable-promise-maybe-knowable-promise
      (-> (promise/c (knowable/c (maybe/c (promise/c knowable?))))
        (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
      
      )
    (sequence/c smoosh-and-comparison-of-two-report?))
  (sequence-map
    (fn report
      (smoosh-and-comparison-of-two-report-map report
        #:on-<=?-knowable-promise on-<=?-knowable-promise
        #:on->=?-knowable-promise on->=?-knowable-promise
        
        #:on-join-knowable-promise-maybe-knowable-promise
        on-join-knowable-promise-maybe-knowable-promise
        
        #:on-meet-knowable-promise-maybe-knowable-promise
        on-meet-knowable-promise-maybe-knowable-promise
        
        #:on-==-knowable-promise-maybe-knowable-promise
        on-==-knowable-promise-maybe-knowable-promise
        
        #:on-path-related-knowable-promise-maybe-knowable-promise
        on-path-related-knowable-promise-maybe-knowable-promise
        
        ))
    reports))

(define-imitation-simple-struct
  (zip*-mapped-smoosh-and-comparison-of-two-report?
    zip*-mapped-smoosh-and-comparison-of-two-report-on-<=?-knowable-promise
    zip*-mapped-smoosh-and-comparison-of-two-report-on->=?-knowable-promise
    zip*-mapped-smoosh-and-comparison-of-two-report-on-join-knowable-promise-maybe-knowable-promise
    zip*-mapped-smoosh-and-comparison-of-two-report-on-meet-knowable-promise-maybe-knowable-promise
    zip*-mapped-smoosh-and-comparison-of-two-report-on-==-knowable-promise-maybe-knowable-promise
    zip*-mapped-smoosh-and-comparison-of-two-report-on-path-related-knowable-promise-maybe-knowable-promise
    zip*-mapped-smoosh-and-comparison-of-two-report-original-list)
  zip*-mapped-smoosh-and-comparison-of-two-report
  'zip*-mapped-smoosh-and-comparison-of-two-report (current-inspector)
  (auto-write)
  (#:prop prop:smoosh-and-comparison-of-two-report
    (make-smoosh-and-comparison-of-two-report-impl
      
      #:<=?-knowable-promise
      (dissectfn
        (zip*-mapped-smoosh-and-comparison-of-two-report
          on-<=?-knowable-promise
          on->=?-knowable-promise
          on-join-knowable-promise-maybe-knowable-promise
          on-meet-knowable-promise-maybe-knowable-promise
          on-==-knowable-promise-maybe-knowable-promise
          on-path-related-knowable-promise-maybe-knowable-promise
          original-list)
        (on-<=?-knowable-promise
          (list-map original-list /fn original
            (smoosh-and-comparison-of-two-report-<=?-knowable-promise
              original))))
      
      #:>=?-knowable-promise
      (dissectfn
        (zip*-mapped-smoosh-and-comparison-of-two-report
          on-<=?-knowable-promise
          on->=?-knowable-promise
          on-join-knowable-promise-maybe-knowable-promise
          on-meet-knowable-promise-maybe-knowable-promise
          on-==-knowable-promise-maybe-knowable-promise
          on-path-related-knowable-promise-maybe-knowable-promise
          original-list)
        (on->=?-knowable-promise
          (list-map original-list /fn original
            (smoosh-and-comparison-of-two-report->=?-knowable-promise
              original))))
      
      #:get-smoosh-report
      (dissectfn
        (zip*-mapped-smoosh-and-comparison-of-two-report
          on-<=?-knowable-promise
          on->=?-knowable-promise
          on-join-knowable-promise-maybe-knowable-promise
          on-meet-knowable-promise-maybe-knowable-promise
          on-==-knowable-promise-maybe-knowable-promise
          on-path-related-knowable-promise-maybe-knowable-promise
          original-list)
        (smoosh-report-zip*-map
          (list-map original-list /fn original
            (smoosh-and-comparison-of-two-report-get-smoosh-report
              original))
          
          #:on-join-knowable-promise-maybe-knowable-promise
          on-join-knowable-promise-maybe-knowable-promise
          
          #:on-meet-knowable-promise-maybe-knowable-promise
          on-meet-knowable-promise-maybe-knowable-promise
          
          #:on-==-knowable-promise-maybe-knowable-promise
          on-==-knowable-promise-maybe-knowable-promise
          
          #:on-path-related-knowable-promise-maybe-knowable-promise
          on-path-related-knowable-promise-maybe-knowable-promise
          
          ))
      
      )))

(define/own-contract
  (smoosh-and-comparison-of-two-report-zip*-map report-list
    
    #:on-check-result-knowable-promise
    [ on-check-result-knowable-promise
      (fn kp-list
        (raise-arguments-error 'smoosh-and-comparison-of-two-report-zip*-map
          "tried to retrieve a check result when its mapping behavior was undefined"
          "check-result-knowable-promise-list" kp-list))]
    
    #:on-<=?-knowable-promise
    [on-<=?-knowable-promise on-check-result-knowable-promise]
    
    #:on->=?-knowable-promise
    [on->=?-knowable-promise on-check-result-knowable-promise]
    
    #:on-smoosh-result-knowable-promise-maybe-knowable-promise
    [ on-smoosh-result-knowable-promise-maybe-knowable-promise
      (fn kpmkp-list
        (raise-arguments-error 'smoosh-and-comparison-of-two-report-zip*-map
          "tried to retrieve a smoosh result when its mapping behavior was undefined"
          
          "smoosh-result-knowable-promise-maybe-knowable-promise-list"
          kpmkp-list
          
          ))]
    
    #:on-join-knowable-promise-maybe-knowable-promise
    [ on-join-knowable-promise-maybe-knowable-promise
      on-smoosh-result-knowable-promise-maybe-knowable-promise]
    
    #:on-meet-knowable-promise-maybe-knowable-promise
    [ on-meet-knowable-promise-maybe-knowable-promise
      on-smoosh-result-knowable-promise-maybe-knowable-promise]
    
    #:on-==-knowable-promise-maybe-knowable-promise
    [ on-==-knowable-promise-maybe-knowable-promise
      on-smoosh-result-knowable-promise-maybe-knowable-promise]
    
    #:on-path-related-knowable-promise-maybe-knowable-promise
    [ on-path-related-knowable-promise-maybe-knowable-promise
      on-smoosh-result-knowable-promise-maybe-knowable-promise]
    
    )
  (->*
    ((listof smoosh-and-comparison-of-two-report?))
    (
      #:on-check-result-knowable-promise
      (-> (listof (promise/c (knowable/c boolean?)))
        (promise/c (knowable/c boolean?)))
      
      #:on-<=?-knowable-promise
      (-> (listof (promise/c (knowable/c boolean?)))
        (promise/c (knowable/c boolean?)))
      
      #:on->=?-knowable-promise
      (-> (listof (promise/c (knowable/c boolean?)))
        (promise/c (knowable/c boolean?)))
      
      #:on-smoosh-result-knowable-promise-maybe-knowable-promise
      (->
        (listof
          (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
        (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
      
      #:on-join-knowable-promise-maybe-knowable-promise
      (->
        (listof
          (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
        (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
      
      #:on-meet-knowable-promise-maybe-knowable-promise
      (->
        (listof
          (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
        (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
      
      #:on-==-knowable-promise-maybe-knowable-promise
      (->
        (listof
          (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
        (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
      
      #:on-path-related-knowable-promise-maybe-knowable-promise
      (->
        (listof
          (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
        (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
      
      )
    smoosh-and-comparison-of-two-report?)
  (zip*-mapped-smoosh-and-comparison-of-two-report
    on-<=?-knowable-promise
    on->=?-knowable-promise
    on-join-knowable-promise-maybe-knowable-promise
    on-meet-knowable-promise-maybe-knowable-promise
    on-==-knowable-promise-maybe-knowable-promise
    on-path-related-knowable-promise-maybe-knowable-promise
    report-list))

(define/own-contract
  (smoosh-and-comparison-of-two-reports-zip*-map reports-list
    
    #:on-check-result-knowable-promise
    [ on-check-result-knowable-promise
      (fn kp-list
        (raise-arguments-error 'smoosh-and-comparison-of-two-report-zip*-map
          "tried to retrieve a check result when its mapping behavior was undefined"
          "check-result-knowable-promise-list" kp-list))]
    
    #:on-<=?-knowable-promise
    [ on-<=?-knowable-promise
      on-check-result-knowable-promise]
    
    #:on->=?-knowable-promise
    [ on->=?-knowable-promise
      on-check-result-knowable-promise]
    
    #:on-smoosh-result-knowable-promise-maybe-knowable-promise
    [ on-smoosh-result-knowable-promise-maybe-knowable-promise
      (fn kpmkp-list
        (raise-arguments-error 'smoosh-and-comparison-of-two-report-zip*-map
          "tried to retrieve a smoosh result when its mapping behavior was undefined"
          
          "smoosh-result-knowable-promise-maybe-knowable-promise-list"
          kpmkp-list
          
          ))]
    
    #:on-join-knowable-promise-maybe-knowable-promise
    [ on-join-knowable-promise-maybe-knowable-promise
      on-smoosh-result-knowable-promise-maybe-knowable-promise]
    
    #:on-meet-knowable-promise-maybe-knowable-promise
    [ on-meet-knowable-promise-maybe-knowable-promise
      on-smoosh-result-knowable-promise-maybe-knowable-promise]
    
    #:on-==-knowable-promise-maybe-knowable-promise
    [ on-==-knowable-promise-maybe-knowable-promise
      on-smoosh-result-knowable-promise-maybe-knowable-promise]
    
    #:on-path-related-knowable-promise-maybe-knowable-promise
    [ on-path-related-knowable-promise-maybe-knowable-promise
      on-smoosh-result-knowable-promise-maybe-knowable-promise]
    
    )
  (->*
    ((listof (sequence/c smoosh-and-comparison-of-two-report?)))
    (
      #:on-check-result-knowable-promise
      (-> (listof (promise/c (knowable/c boolean?)))
        (promise/c (knowable/c boolean?)))
      
      #:on-<=?-knowable-promise
      (-> (listof (promise/c (knowable/c boolean?)))
        (promise/c (knowable/c boolean?)))
      
      #:on->=?-knowable-promise
      (-> (listof (promise/c (knowable/c boolean?)))
        (promise/c (knowable/c boolean?)))
      
      #:on-smoosh-result-knowable-promise-maybe-knowable-promise
      (->
        (listof
          (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
        (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
      
      #:on-join-knowable-promise-maybe-knowable-promise
      (->
        (listof
          (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
        (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
      
      #:on-meet-knowable-promise-maybe-knowable-promise
      (->
        (listof
          (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
        (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
      
      #:on-==-knowable-promise-maybe-knowable-promise
      (->
        (listof
          (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
        (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
      
      #:on-path-related-knowable-promise-maybe-knowable-promise
      (->
        (listof
          (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
        (promise/c (knowable/c (maybe/c (promise/c knowable?)))))
      
      )
    (sequence/c smoosh-and-comparison-of-two-report?))
  (sequence-zip*-map reports-list /fn report-list
    (smoosh-and-comparison-of-two-report-zip*-map report-list
      #:on-<=?-knowable-promise on-<=?-knowable-promise
      #:on->=?-knowable-promise on->=?-knowable-promise
      
      #:on-join-knowable-promise-maybe-knowable-promise
      on-join-knowable-promise-maybe-knowable-promise
      
      #:on-meet-knowable-promise-maybe-knowable-promise
      on-meet-knowable-promise-maybe-knowable-promise
      
      #:on-==-knowable-promise-maybe-knowable-promise
      on-==-knowable-promise-maybe-knowable-promise
      
      #:on-path-related-knowable-promise-maybe-knowable-promise
      on-path-related-knowable-promise-maybe-knowable-promise)))


(define-imitation-simple-struct
  (mapped-smoosh-equal-hash-code-support-report?
    mapped-smoosh-equal-hash-code-support-report-on-==-hash-code-promise
    mapped-smoosh-equal-hash-code-support-report-on-path-related-hash-code-promise
    mapped-smoosh-equal-hash-code-support-report-original)
  mapped-smoosh-equal-hash-code-support-report
  'mapped-smoosh-equal-hash-code-support-report (current-inspector)
  (auto-write)
  (#:prop prop:smoosh-equal-hash-code-support-report
    (make-smoosh-equal-hash-code-support-report-impl
      
      #:==-hash-code-promise
      (fn self a now?
        (dissect self
          (mapped-smoosh-equal-hash-code-support-report
            on-==-hash-code-promise
            on-path-related-hash-code-promise
            original)
        /on-==-hash-code-promise
          (smoosh-equal-hash-code-support-report-==-hash-code-promise
            original)))
      
      #:path-related-hash-code-promise
      (fn self a now?
        (dissect self
          (mapped-smoosh-equal-hash-code-support-report
            on-==-hash-code-promise
            on-path-related-hash-code-promise
            original)
        /on-path-related-hash-code-promise
          (smoosh-equal-hash-code-support-report-path-related-hash-code-promise
            original)))
      
      )))

(define/own-contract
  (smoosh-equal-hash-code-support-report-map report
    
    #:on-hash-code-promise
    [ on-hash-code-promise
      (fn p
        p)]
    
    #:on-==-hash-code-promise
    [on-==-hash-code-promise on-hash-code-promise]
    
    #:on-path-related-hash-code-promise
    [on-path-related-hash-code-promise on-hash-code-promise]
    
    )
  (->*
    (smoosh-equal-hash-code-support-report?)
    (
      #:on-hash-code-promise
      (-> (promise/c fixnum?) (promise/c fixnum?))
      
      #:on-==-hash-code-promise
      (-> (promise/c fixnum?) (promise/c fixnum?))
      
      #:on-path-related-hash-code-promise
      (-> (promise/c fixnum?) (promise/c fixnum?))
      
      )
    smoosh-equal-hash-code-support-report?)
  (mapped-smoosh-equal-hash-code-support-report
    on-==-hash-code-promise on-path-related-hash-code-promise report))

(define/own-contract
  (smoosh-equal-hash-code-support-reports-map reports
    
    #:on-hash-code-promise
    [ on-hash-code-promise
      (fn p
        p)]
    
    #:on-==-hash-code-promise
    [on-==-hash-code-promise on-hash-code-promise]
    
    #:on-path-related-hash-code-promise
    [on-path-related-hash-code-promise on-hash-code-promise]
    
    )
  (->*
    ((sequence/c smoosh-equal-hash-code-support-report?))
    (
      #:on-hash-code-promise
      (-> (promise/c fixnum?) (promise/c fixnum?))
      
      #:on-==-hash-code-promise
      (-> (promise/c fixnum?) (promise/c fixnum?))
      
      #:on-path-related-hash-code-promise
      (-> (promise/c fixnum?) (promise/c fixnum?))
      
      )
    (sequence/c smoosh-equal-hash-code-support-report?))
  (sequence-map
    (fn report
      (smoosh-equal-hash-code-support-report-map report
        #:on-hash-code-promise on-hash-code-promise
        #:on-==-hash-code-promise on-==-hash-code-promise
        
        #:on-path-related-hash-code-promise
        on-path-related-hash-code-promise
        
        ))
    reports))

(define-imitation-simple-struct
  (zip*-mapped-smoosh-equal-hash-code-support-report?
    zip*-mapped-smoosh-equal-hash-code-support-report-on-==-hash-code-promise
    zip*-mapped-smoosh-equal-hash-code-support-report-on-path-related-hash-code-promise
    zip*-mapped-smoosh-equal-hash-code-support-report-original-list)
  zip*-mapped-smoosh-equal-hash-code-support-report
  'zip*-mapped-smoosh-equal-hash-code-support-report (current-inspector)
  (auto-write)
  (#:prop prop:smoosh-equal-hash-code-support-report
    (make-smoosh-equal-hash-code-support-report-impl
      
      #:==-hash-code-promise
      (fn self a now?
        (dissect self
          (zip*-mapped-smoosh-equal-hash-code-support-report
            on-==-hash-code-promise
            on-path-related-hash-code-promise
            original-list)
        /on-==-hash-code-promise
          (list-map original-list /fn original
            (smoosh-equal-hash-code-support-report-==-hash-code-promise
              original))))
      
      #:path-related-hash-code-promise
      (fn self a now?
        (dissect self
          (zip*-mapped-smoosh-equal-hash-code-support-report
            on-==-hash-code-promise
            on-path-related-hash-code-promise
            original-list)
        /on-path-related-hash-code-promise
          (list-map original-list /fn original
            (smoosh-equal-hash-code-support-report-path-related-hash-code-promise
              original))))
      
      )))

(define/own-contract
  (smoosh-equal-hash-code-support-report-zip*-map report-list
    
    #:on-hash-code-promise
    [ on-hash-code-promise
      (fn p-list
        (raise-arguments-error 'smoosh-equal-hash-code-support-report-zip*-map
          "tried to retrieve a smoosh result when its mapping behavior was undefined"
          "hash-code-promise-list" p-list))]
    
    #:on-==-hash-code-promise
    [on-==-hash-code-promise on-hash-code-promise]
    
    #:on-path-related-hash-code-promise
    [on-path-related-hash-code-promise on-hash-code-promise]
    
    )
  (->*
    ((listof smoosh-equal-hash-code-support-report?))
    (
      #:on-hash-code-promise
      (-> (listof (promise/c fixnum?)) (promise/c fixnum?))
      
      #:on-==-hash-code-promise
      (-> (listof (promise/c fixnum?)) (promise/c fixnum?))
      
      #:on-path-related-hash-code-promise
      (-> (listof (promise/c fixnum?)) (promise/c fixnum?))
      
      )
    smoosh-equal-hash-code-support-report?)
  (zip*-mapped-smoosh-equal-hash-code-support-report
    on-==-hash-code-promise
    on-path-related-hash-code-promise
    report-list))

(define/own-contract
  (smoosh-equal-hash-code-support-reports-zip*-map reports-list
    
    #:on-hash-code-promise
    [ on-hash-code-promise
      (fn p-list
        (raise-arguments-error 'smoosh-equal-hash-code-support-reports-zip*-map
          "tried to retrieve a smoosh result when its mapping behavior was undefined"
          "hash-code-promise-list" p-list))]
    
    #:on-==-hash-code-promise
    [on-==-hash-code-promise on-hash-code-promise]
    
    #:on-path-related-hash-code-promise
    [on-path-related-hash-code-promise on-hash-code-promise]
    
    )
  (->*
    ((listof (sequence/c smoosh-equal-hash-code-support-report?)))
    (
      #:on-hash-code-promise
      (-> (listof (promise/c fixnum?)) (promise/c fixnum?))
      
      #:on-==-hash-code-promise
      (-> (listof (promise/c fixnum?)) (promise/c fixnum?))
      
      #:on-path-related-hash-code-promise
      (-> (listof (promise/c fixnum?)) (promise/c fixnum?))
      
      )
    (sequence/c smoosh-equal-hash-code-support-report?))
  (sequence-zip*-map reports-list /fn report-list
    (smoosh-equal-hash-code-support-report-zip*-map report-list
      #:on-==-hash-code-promise on-==-hash-code-promise
      
      #:on-path-related-hash-code-promise
      on-path-related-hash-code-promise)))

(define/own-contract (false-smoosh-and-comparison-of-two-reports)
  (-> (sequence/c smoosh-and-comparison-of-two-report?))
  (smoosh-and-comparison-of-two-reports-zip*-map (list)
    #:on-check-result-knowable-promise
    (dissectfn (list)
      (delay/strict /known #f))
    #:on-smoosh-result-knowable-promise-maybe-knowable-promise
    (dissectfn (list)
      (delay/strict /known /nothing))))

(define-imitation-simple-struct
  (constant-smoosh-report?
    constant-smoosh-report-result-knowable-promise-maybe-knowable-promise)
  constant-smoosh-report-unguarded
  'constant-smoosh-report (current-inspector) (auto-write)
  (#:prop prop:smoosh-report /make-smoosh-report-impl
    
    #:join-knowable-promise-maybe-knowable-promise
    (dissectfn
      (constant-smoosh-report-unguarded
        result-knowable-promise-maybe-knowable-promise)
      result-knowable-promise-maybe-knowable-promise)
    
    #:meet-knowable-promise-maybe-knowable-promise
    (dissectfn
      (constant-smoosh-report-unguarded
        result-knowable-promise-maybe-knowable-promise)
      result-knowable-promise-maybe-knowable-promise)
    
    #:==-knowable-promise-maybe-knowable-promise
    (dissectfn
      (constant-smoosh-report-unguarded
        result-knowable-promise-maybe-knowable-promise)
      result-knowable-promise-maybe-knowable-promise)
    
    #:path-related-knowable-promise-maybe-knowable-promise
    (dissectfn
      (constant-smoosh-report-unguarded
        result-knowable-promise-maybe-knowable-promise)
      result-knowable-promise-maybe-knowable-promise)
    
    ))

(define/own-contract
  (constant-smoosh-report
    result-knowable-promise-maybe-knowable-promise)
  (->
    (promise/c (knowable/c (maybe/c (promise/c (knowable/c any/c)))))
    smoosh-report?)
  (constant-smoosh-report-unguarded
    result-knowable-promise-maybe-knowable-promise))

(define/own-contract
  (constant-smoosh-reports
    result-knowable-promise-maybe-knowable-promise)
  (->
    (promise/c (knowable/c (maybe/c (promise/c (knowable/c any/c)))))
    (sequence/c smoosh-report?))
  (in-cycle /list /constant-smoosh-report
    result-knowable-promise-maybe-knowable-promise))

(define/own-contract (promise-map promise on-value)
  (-> promise? (-> any/c any/c) promise?)
  (delay /on-value /force promise))

(define-imitation-simple-struct
  (constant-smoosh-and-comparison-of-two-report?
    constant-smoosh-and-comparison-of-two-report-result-knowable-promise-maybe-knowable-promise)
  constant-smoosh-and-comparison-of-two-report-unguarded
  'constant-smoosh-and-comparison-of-two-report (current-inspector) (auto-write)
  (#:prop prop:smoosh-and-comparison-of-two-report
    (make-smoosh-and-comparison-of-two-report-impl
      
      #:<=?-knowable-promise
      (dissectfn
        (constant-smoosh-and-comparison-of-two-report-unguarded
          result-knowable-promise-maybe-knowable-promise)
        (promise-map result-knowable-promise-maybe-knowable-promise
          (fn kpmk
            (knowable-map kpmk /fn kpm
              (just? kpm)))))
      
      #:>=?-knowable-promise
      (dissectfn
        (constant-smoosh-and-comparison-of-two-report-unguarded
          result-knowable-promise-maybe-knowable-promise)
        (promise-map result-knowable-promise-maybe-knowable-promise
          (fn kpmk
            (knowable-map kpmk /fn kpm
              (just? kpm)))))
      
      #:get-smoosh-report
      (dissectfn
        (constant-smoosh-and-comparison-of-two-report-unguarded
          result-knowable-promise-maybe-knowable-promise)
        (constant-smoosh-report
          result-knowable-promise-maybe-knowable-promise))
      
      )))

(define/own-contract
  (constant-smoosh-and-comparison-of-two-report
    result-knowable-promise-maybe-knowable-promise)
  (->
    (promise/c (knowable/c (maybe/c (promise/c (knowable/c any/c)))))
    smoosh-and-comparison-of-two-report?)
  (constant-smoosh-and-comparison-of-two-report-unguarded
    result-knowable-promise-maybe-knowable-promise))

(define/own-contract
  (constant-smoosh-and-comparison-of-two-reports
    result-knowable-promise-maybe-knowable-promise)
  (->
    (promise/c (knowable/c (maybe/c (promise/c (knowable/c any/c)))))
    (sequence/c smoosh-and-comparison-of-two-report?))
  (in-cycle /list /constant-smoosh-and-comparison-of-two-report
    result-knowable-promise-maybe-knowable-promise))

(define-imitation-simple-struct
  (constant-smoosh-equal-hash-code-support-report?
    constant-smoosh-equal-hash-code-support-report-hash-code-promise)
  constant-smoosh-equal-hash-code-support-report-unguarded
  'constant-smoosh-equal-hash-code-support-report (current-inspector)
  (auto-write)
  (#:prop prop:smoosh-equal-hash-code-support-report
    (make-smoosh-equal-hash-code-support-report-impl
      
      #:==-hash-code-promise
      (fn self now?
        (dissect self
          (constant-smoosh-equal-hash-code-support-report-unguarded
            hash-code-promise)
          hash-code-promise))
      
      #:path-related-hash-code-promise
      (fn self now?
        (dissect self
          (constant-smoosh-equal-hash-code-support-report-unguarded
            hash-code-promise)
          hash-code-promise))
      
      )))

(define/own-contract
  (constant-smoosh-equal-hash-code-support-report hash-code-promise)
  (-> (promise/c fixnum?) smoosh-equal-hash-code-support-report?)
  (constant-smoosh-equal-hash-code-support-report-unguarded
    hash-code-promise))

(define/own-contract
  (constant-smoosh-equal-hash-code-support-reports hash-code-promise)
  (-> (promise/c fixnum?)
    (sequence/c smoosh-equal-hash-code-support-report?))
  (in-cycle /list /constant-smoosh-equal-hash-code-support-report
    hash-code-promise))

(define-imitation-simple-struct
  (equal-always-wrapper? equal-always-wrapper-value)
  equal-always-wrapper
  'equal-always-wrapper (current-inspector) (auto-write)
  ; We use a comparison that consistently compares the value using
  ; `equal-always?`.
  (#:gen gen:equal-mode+hash
    
    (define (equal-mode-proc a b recur now?)
      (dissect a (equal-always-wrapper a-value)
      /dissect b (equal-always-wrapper b-value)
      /equal-always? a-value b-value))
    
    (define (hash-mode-proc v recur now?)
      (dissect v (equal-always-wrapper v-value)
      /hash-code-combine
        (equal-always-hash-code equal-always-wrapper?)
        (equal-always-hash-code v-value)))
    
    ))
(ascribe-own-contract equal-always-wrapper? (-> any/c boolean?))
(ascribe-own-contract equal-always-wrapper-value
  (-> equal-always-wrapper? any/c))

(define-imitation-simple-struct
  (indistinct-wrapper? indistinct-wrapper-value)
  indistinct-wrapper
  'indistinct-wrapper (current-inspector) (auto-write) (auto-equal))
(ascribe-own-contract indistinct-wrapper? (-> any/c boolean?))
(ascribe-own-contract indistinct-wrapper-value
  (-> indistinct-wrapper? any/c))

(define/own-contract
  (smoosh-and-comparison-of-two-report-join reports-list)
  (-> (listof smoosh-report?) smoosh-report?)
  (smoosh-and-comparison-of-two-report-zip*-map reports-list
    #:on-check-result-knowable-promise
    (fn kp-list
      (knowable-promise-or kp-list))
    #:on-smoosh-result-knowable-promise-maybe-knowable-promise
    (fn kpmkp-list
      (delay
        (w-loop next kpmkp-list kpmkp-list
          (expect kpmkp-list (cons kpmkp kpmkp-list) (unknown)
          /expect (force kpmkp) (known kpm) (next kpmkp-list)
          /known
            (maybe-map kpm /fn kp
              (promise-map kp /fn k
                (if (known? k) k
                /w-loop next kpmkp-list kpmkp-list
                  (expect kpmkp-list (cons kpmkp kpmkp-list) (unknown)
                  /expect (force kpmkp) (known kpm) (next kpmkp-list)
                  /expect kpm (just kp)
                    ; TODO: Add details to this error message.
                    (raise-argument-error
                      'smoosh-and-comparison-of-two-report-join
                      "contradictory information")
                  /w- k (force kp)
                  /if (known? k) k
                  /next kpmkp-list))))))))))

(define/own-contract
  (smoosh-and-comparison-of-two-reports-join reports-list)
  (-> (listof (sequence/c smoosh-report?))
    (sequence/c smoosh-report?))
  (sequence-zip*-map reports-list /fn report-list
    (smoosh-and-comparison-of-two-report-join report-list)))

(define/own-contract
  (smoosh-reports-with-hesitation-at-discrepancies reports
    #:known-distinct? [known-distinct? #t]
    #:known-discrete? [known-discrete? #f])
  (->*
    ((sequence/c smoosh-report?))
    (#:known-distinct? boolean? #:known-discrete? boolean?)
    (sequence/c smoosh-report?))
  (if (and known-distinct? known-discrete?) reports
  /w- kpmkp->known-true
    (fn kpmkp
      (promise-map kpmkp /fn kpmk
        (knowable-bind kpmk /fn kpm
        /knowable-if (just? kpm) /fn kpm)))
  /if (not known-distinct?)
    (smoosh-reports-map reports
      #:on-smoosh-result-knowable-promise-maybe-knowable-promise
      kpmkp->known-true)
  /dissect reports (app sequence->stream /stream* report-0 report-1+)
  /stream*
    (smoosh-report-map report-0
      #:on-smoosh-result-knowable-promise-maybe-knowable-promise
      kpmkp->known-true
      
      #:on-==-knowable-promise-maybe-knowable-promise
      (fn kpmkp
        kpmkp)
      
      )
    report-1+))

(define/own-contract
  (smoosh-and-comparison-of-two-reports-with-hesitation-at-discrepancies
    reports
    #:known-distinct? [known-distinct? #t]
    #:known-discrete? [known-discrete? #f])
  (->*
    ((sequence/c smoosh-and-comparison-of-two-report?))
    (#:known-distinct? boolean? #:known-discrete? boolean?)
    (sequence/c smoosh-and-comparison-of-two-report?))
  (if (and known-distinct? known-discrete?) reports
  /w- kp->known-true
    (fn kp
      (promise-map kp /fn k
        (knowable-bind k /fn result
        /knowable-if result /fn result)))
  /w- kpmkp->known-true
    (fn kpmkp
      (promise-map kpmkp /fn kpmk
        (knowable-bind kpmk /fn kpm
        /knowable-if (just? kpm) /fn kpm)))
  /if (not known-distinct?)
    (smoosh-and-comparison-of-two-reports-map reports
      #:on-check-result-knowable-promise kp->known-true
      
      #:on-smoosh-result-knowable-promise-maybe-knowable-promise
      kpmkp->known-true
      
      )
  /dissect reports (app sequence->stream /stream* report-0 report-1+)
  /stream*
    (smoosh-and-comparison-of-two-report-map report-0
      #:on-check-result-knowable-promise kp->known-true
      
      #:on-smoosh-result-knowable-promise-maybe-knowable-promise
      kpmkp->known-true
      
      #:on-==-knowable-promise-maybe-knowable-promise
      (fn kpmkp
        kpmkp)
      
      )
    report-1+))

(define/own-contract
  (make-glossesque-sys-impl-for-hash make-empty-hash)
  (-> (-> (and/c hash? immutable?)) glossesque-sys-impl?)
  (make-glossesque-sys-impl
    
    #:glossesque-union-of-zero (fn gs /make-empty-hash)
    
    #:glossesque-skm-union-of-two-knowable
    (fn gs state a b skm-union-knowable
      (hash-skm-union-of-two-knowable state a b skm-union-knowable))
    
    #:glossesque-ref-maybe-knowable
    (fn gs g k
      (known /hash-ref-maybe g k))
    
    #:rider-and-glossesque-update-maybe-knowable
    (fn gs rider-and-g k on-rider-and-m
      (rider-and-hash-update-maybe-knowable
        rider-and-g k on-rider-and-m))
    
    #:glossesque-empty? (fn gs g /hash-empty? g)
    #:glossesque-count (fn gs g /hash-count g)
    #:glossesque-iteration-sequence (fn gs g /in-hash g)
    
    ))

(define/own-contract (atom-chaperone=? a b)
  (-> any/c any/c boolean?)
  (and (chaperone-of? a b) (chaperone-of? b a)))

(define/own-contract (list-rev-append rev-past rest)
  (-> list? any/c any/c)
  (expect rev-past (cons elem rev-past) rest
  /list-rev-append rev-past (cons elem rest)))

(define/own-contract
  (list-rem-first-maybe-knowable lst check?-knowable)
  (-> list? (-> any/c (knowable/c boolean?))
    (knowable/c (maybe/c (list/c any/c list?))))
  (w-loop next rev-past (list) lst lst
    (expect lst (cons elem lst) (known /nothing)
    /knowable-bind (check?-knowable elem) /fn succeeded?
    /if succeeded?
      (known /just /list elem (list-rev-append rev-past lst))
    /next (cons elem rev-past) lst)))

(define/own-contract
  (assoc-list-skm-union-of-two-knowable
    ==?-knowable state a b skm-union-knowable)
  (->
    (-> any/c any/c (knowable/c boolean?))
    any/c
    (listof pair?)
    (listof pair?)
    (-> any/c any/c maybe? maybe? (knowable/c (list/c any/c maybe?)))
    (knowable/c (list/c any/c (listof pair?))))
  (w-loop next state state a a b b rev-result (list)
    (w- entry-and-next
      (fn state a b rev-result k a-v-m b-v-m
        (knowable-bind (skm-union-knowable state k a-v-m b-v-m)
        /dissectfn (list state v-m)
        /next state a b
          (expect v-m (just v) rev-result
          /cons (cons k v) rev-result)))
    /expect a (cons a-entry a)
      (expect b (cons b-entry b)
        (known /list state /reverse rev-result)
      /dissect b-entry (cons b-k b-v)
      /entry-and-next state a b rev-result b-k (nothing) (just b-v))
    /dissect a-entry (cons a-k a-v)
    /knowable-bind
      (list-rem-first-maybe-knowable b /dissectfn (cons b-k b-v)
        (==?-knowable a-k b-k))
    /fn maybe-b-entry-and-b
    /expect maybe-b-entry-and-b (just b-entry-and-b)
      (entry-and-next state a b rev-result a-k (just a-v) (nothing))
    /dissect b-entry-and-b (list (cons b-k b-v) b)
    /entry-and-next state a b rev-result a-k (just a-v) (just b-v))))

(define/own-contract
  (assoc-list-ref-maybe-knowable ==?-knowable a k)
  (-> (-> any/c any/c (knowable/c boolean?)) (listof pair?) any/c
    (knowable/c maybe?))
  (w-loop next a a
    (expect a (cons entry a) (known /nothing)
    /dissect entry (cons a-k v)
    /knowable-bind (==?-knowable k a-k) /fn succeeded?
    /if succeeded? (known /just v)
    /next a)))

(define/own-contract
  (rider-and-assoc-list-update-maybe-knowable
    ==?-knowable rider-and-a k on-rider-and-m-knowable)
  (->
    (-> any/c any/c (knowable/c boolean?))
    (list/c any/c (listof pair?))
    any/c
    (-> (list/c any/c maybe?) (knowable/c (list/c any/c maybe?)))
    (knowable/c (list/c any/c (listof pair?))))
  (dissect rider-and-a (list rider a)
  /w-loop next a a
    (expect a (cons entry a)
      (knowable-map (on-rider-and-m-knowable /list rider /nothing)
      /dissectfn (list rider m)
        (list rider
          (expect m (just v) (list)
          /list /cons k v)))
    /dissect entry (cons a-k v)
    /knowable-bind (==?-knowable k a-k) /fn succeeded?
    /if succeeded?
      (knowable-map (on-rider-and-m-knowable /list rider /just v)
      /dissectfn (list rider m)
        (list rider
          (expect m (just v) a
          /cons (cons a-k v) a)))
    /knowable-bind (next a) /dissectfn (list rider a)
    /known /list rider /cons entry a)))

(define-imitation-simple-struct
  (counted-glossesque?
    counted-glossesque-count
    counted-glossesque-original)
  counted-glossesque
  'counted-glossesque (current-inspector) (auto-write))

(define/own-contract
  (make-glossesque-sys-impl-for-counted-glossesque-sys
    get-uncounted-gs)
  (-> (-> glossesque-sys? glossesque-sys?)
    glossesque-sys-impl?)
  (make-glossesque-sys-impl
    
    #:glossesque-union-of-zero
    (fn gs
      (w- ugs (get-uncounted-gs gs)
      /counted-glossesque 0
        (glossesque-sys-glossesque-union-of-zero ugs)))
    
    #:glossesque-skm-union-of-two-knowable
    (fn gs state a b skm-union-knowable
      (w- ugs (get-uncounted-gs gs)
      /dissect a (counted-glossesque count a)
      /dissect b (counted-glossesque _ b)
      /knowable-map
        (glossesque-sys-glossesque-skm-union-of-two-knowable
          ugs (list count state) a b
          (fn cs k a-v-m b-v-m
            (maybe-left-count-sm-union-of-two-knowable
              cs a-v-m b-v-m
              (fn state a-v-m b-v-m
                (skm-union-knowable state k a-v-m b-v-m)))))
      /dissectfn (list (list count state) result)
        (list state /counted-glossesque count result)))
    
    #:glossesque-ref-maybe-knowable
    (fn gs g k
      (w- ugs (get-uncounted-gs gs)
      /dissect g (counted-glossesque _ g)
      /glossesque-sys-glossesque-ref-maybe-knowable ugs g k))
    
    #:rider-and-glossesque-update-maybe-knowable
    (fn gs rider-and-g k on-rider-and-m-knowable
      (w- ugs (get-uncounted-gs gs)
      /dissect rider-and-g (list rider /counted-glossesque count g)
      /knowable-map
        (glossesque-sys-rider-and-glossesque-update-maybe-knowable
          ugs (list (list count rider) g) k
        /fn crm
          (count-and-rider-and-maybe-update-maybe-knowable crm
          /fn rider-and-m
            (on-rider-and-m-knowable rider-and-m)))
      /dissectfn (list (list count rider) result)
        (list rider /counted-glossesque count result)))
    
    #:glossesque-empty?
    (fn gs g
      (zero? /glossesque-sys-glossesque-count gs g))
    
    #:glossesque-count
    (fn gs g
      (dissect g (counted-glossesque count g)
        count))
    
    #:glossesque-iteration-sequence
    (fn gs g
      (w- ugs (get-uncounted-gs gs)
      /dissect g (counted-glossesque _ g)
      /glossesque-sys-glossesque-iteration-sequence ugs g))
    
    ))

(define-imitation-simple-struct
  (counted-glossesque-sys? counted-glossesque-sys-original)
  counted-glossesque-sys-unguarded
  'counted-glossesque-sys (current-inspector) (auto-write)
  (#:prop prop:glossesque-sys
    (make-glossesque-sys-impl-for-counted-glossesque-sys
      (dissectfn (counted-glossesque-sys-unguarded original)
        original))))

(define/own-contract (counted-glossesque-sys original)
  (-> glossesque-sys? glossesque-sys?)
  (counted-glossesque-sys-unguarded original))

(define/own-contract
  (make-glossesque-sys-impl-for-equality-check-knowable
    get-==?-knowable)
  (-> (-> glossesque-sys? (-> any/c any/c (knowable/c boolean?)))
    glossesque-sys-impl?)
  (make-glossesque-sys-impl
    
    #:glossesque-union-of-zero
    (fn gs
      (list))
    
    #:glossesque-skm-union-of-two-knowable
    (fn gs state a b skm-union-knowable
      (w- ==?-knowable (get-==?-knowable gs)
      /assoc-list-skm-union-of-two-knowable
        ==?-knowable state a b skm-union-knowable))
    
    #:glossesque-ref-maybe-knowable
    (fn gs g k
      (w- ==?-knowable (get-==?-knowable gs)
      /assoc-list-ref-maybe-knowable ==?-knowable g k))
    
    #:rider-and-glossesque-update-maybe-knowable
    (fn gs rider-and-g k on-rider-and-m-knowable
      (w- ==?-knowable (get-==?-knowable gs)
      /rider-and-assoc-list-update-maybe-knowable
        ==?-knowable rider-and-g k on-rider-and-m-knowable))
    
    #:glossesque-empty?
    (fn gs g
      (null? g))
    
    ; NOTE: Since we're using `counted-glossesque-sys` for this type
    ; of glossesque, this method and `#:glossesque-empty?` won't
    ; actually be called.
    #:glossesque-count
    (fn gs g
      (length g))
    
    #:glossesque-iteration-sequence
    (fn gs g
      (sequence-map (dissectfn (cons k v) (values k v)) g))
    
    ))

(define-imitation-simple-struct
  (equality-check-knowable-atom-glossesque-sys?
    equality-check-knowable-atom-glossesque-sys-get-==?-knowable)
  equality-check-knowable-atom-glossesque-sys-unguarded
  'equality-check-knowable-atom-glossesque-sys (current-inspector)
  (auto-write)
  (#:prop prop:glossesque-sys
    (make-glossesque-sys-impl-for-equality-check-knowable
      (dissectfn
        (equality-check-knowable-atom-glossesque-sys-unguarded
          ==?-knowable)
        ==?-knowable))))

(define/own-contract
  (equality-check-knowable-atom-glossesque-sys ==?-knowable)
  (-> (-> any/c any/c (knowable/c boolean?)) glossesque-sys?)
  (counted-glossesque-sys
    (equality-check-knowable-atom-glossesque-sys-unguarded
      ==?-knowable)))

(define/own-contract (equality-check-atom-glossesque-sys ==?)
  (-> (-> any/c any/c boolean?) glossesque-sys?)
  (equality-check-knowable-atom-glossesque-sys /fn a b
    (known /==? a b)))

(define/own-contract
  (make-glossesque-sys-impl-for-chaperone=-atom
    gs-for-equal-always get-gs-for-chaperone=-assuming-equal-always)
  (-> glossesque-sys? (-> glossesque-sys? glossesque-sys?)
    glossesque-sys-impl?)
  (w- get-bin-gs get-gs-for-chaperone=-assuming-equal-always
  /make-glossesque-sys-impl
    
    #:glossesque-union-of-zero
    (fn gs
      (w- bin-gs (get-bin-gs gs)
      /glossesque-sys-glossesque-union-of-zero
        gs-for-equal-always bin-gs))
    
    #:glossesque-skm-union-of-two-knowable
    (fn gs state a b skm-union-knowable
      (glossesque-sys-glossesque-skm-union-of-two-knowable
        gs-for-equal-always state a b
        (fn state k a-bin-m b-bin-m
          (w- bin-gs (get-bin-gs gs)
          /glossesque-sys-glossesque-skm-union-of-two-knowable
            (maybe-nonempty-glossesque-sys bin-gs)
            state a-bin-m b-bin-m
            (fn state k a-v-m b-v-m
              (skm-union-knowable state k a-v-m b-v-m))))))
    
    #:glossesque-ref-maybe-knowable
    (fn gs g k
      (knowable-bind
        (glossesque-sys-glossesque-ref-maybe-knowable
          gs-for-equal-always g k)
      /fn bin-m
      /w- bin-gs (get-bin-gs gs)
      /glossesque-sys-glossesque-ref-maybe-knowable
        (maybe-nonempty-glossesque-sys bin-gs)
        bin-m
        k))
    
    #:rider-and-glossesque-update-maybe-knowable
    (fn gs rider-and-g k on-rider-and-m-knowable
      (glossesque-sys-rider-and-glossesque-update-maybe-knowable
        gs-for-equal-always rider-and-g k
        (dissectfn (list rider bin-m)
          (w- bin-gs (get-bin-gs gs)
          /glossesque-sys-rider-and-glossesque-update-maybe-knowable
            (maybe-nonempty-glossesque-sys bin-gs)
            (list rider bin-m)
            k
            (fn rider-and-m
              (on-rider-and-m-knowable rider-and-m))))))
    
    #:glossesque-empty?
    (fn gs g
      (glossesque-sys-glossesque-empty? gs-for-equal-always g))
    
    ; NOTE: Since we're using `counted-glossesque-sys` for this type
    ; of glossesque, this method and `#:glossesque-empty?` won't
    ; actually be called.
    #:glossesque-count
    (fn gs g
      (sequence-length
        (glossesque-sys-glossesque-iteration-sequence gs g)))
    
    #:glossesque-iteration-sequence
    (fn gs g
      (w- bin-gs (get-bin-gs gs)
      /apply in-sequences
        (for/list
          (
            [ (k bin)
              (in-sequences
                (glossesque-sys-glossesque-iteration-sequence
                  gs-for-equal-always g))])
          (glossesque-sys-glossesque-iteration-sequence bin-gs bin))))
    
    ))

(define-imitation-simple-struct (equal-always-atom-glossesque-sys?)
  equal-always-atom-glossesque-sys-unguarded
  'equal-always-atom-glossesque-sys (current-inspector) (auto-write)
  (#:prop prop:glossesque-sys /make-glossesque-sys-impl-for-hash /fn
    (hashalw)))

(define/own-contract (equal-always-atom-glossesque-sys)
  (-> glossesque-sys?)
  (equal-always-atom-glossesque-sys-unguarded))

(define/own-contract
  (equality-check-indistinct-atom-glossesque-sys ==?)
  (-> (-> any/c any/c boolean?) glossesque-sys?)
  (equality-check-knowable-atom-glossesque-sys /fn a b
    (falsable->uninformative-knowable /==? a b)))

(define/own-contract (indistinct-glossesque-sys original-gs)
  (-> glossesque-sys? glossesque-sys?)
  (equality-check-knowable-atom-glossesque-sys /fn a b
    (w- g (glossesque-sys-glossesque-union-of-zero original-gs)
    /knowable-bind
      (glossesque-sys-glossesque-set-maybe-knowable
        original-gs g a (just /trivial))
    /fn g
    /knowable-bind
      (glossesque-sys-glossesque-ref-maybe-knowable original-gs g b)
    /fn v-m
    /known /just? v-m)))

(define/own-contract (equal-always-indistinct-atom-glossesque-sys)
  (-> glossesque-sys?)
  (equality-check-indistinct-atom-glossesque-sys /fn a b
    (equal-always? a b)))

(define-imitation-simple-struct (chaperone=-atom-glossesque-sys?)
  chaperone=-atom-glossesque-sys-unguarded
  'chaperone=-atom-glossesque-sys (current-inspector) (auto-write)
  (#:prop prop:glossesque-sys
    (make-glossesque-sys-impl-for-chaperone=-atom
      (equal-always-atom-glossesque-sys)
      (equality-check-indistinct-atom-glossesque-sys /fn a b
        (atom-chaperone=? a b)))))

(define/own-contract (chaperone=-atom-glossesque-sys)
  (-> glossesque-sys?)
  (counted-glossesque-sys /chaperone=-atom-glossesque-sys-unguarded))

(define-imitation-simple-struct
  (chaperone=-indistinct-atom-glossesque-sys?)
  chaperone=-indistinct-atom-glossesque-sys-unguarded
  'chaperone=-indistinct-atom-glossesque-sys (current-inspector)
  (auto-write)
  (#:prop prop:glossesque-sys
    (make-glossesque-sys-impl-for-chaperone=-atom
      (equal-always-indistinct-atom-glossesque-sys)
      (equality-check-indistinct-atom-glossesque-sys /fn a b
        (atom-chaperone=? a b)))))

(define/own-contract (chaperone=-indistinct-atom-glossesque-sys)
  (-> glossesque-sys?)
  (counted-glossesque-sys
    (chaperone=-indistinct-atom-glossesque-sys-unguarded)))

(define-imitation-simple-struct (eq-atom-glossesque-sys?)
  eq-atom-glossesque-sys-unguarded
  'eq-atom-glossesque-sys (current-inspector) (auto-write)
  (#:prop prop:glossesque-sys /make-glossesque-sys-impl-for-hash /fn
    (hasheq)))

(define/own-contract (eq-atom-glossesque-sys)
  (-> glossesque-sys?)
  (eq-atom-glossesque-sys-unguarded))

(define/own-contract (eq-indistinct-atom-glossesque-sys)
  (-> glossesque-sys?)
  (equality-check-indistinct-atom-glossesque-sys /fn a b /eq? a b))

; NOTE: We don't export this. It's just an implementation detail of
; `make-expressly-smooshable-bundle-property-from-list-isomorphism`.
(define-imitation-simple-struct
  (shallow-wrapper? shallow-wrapper-value)
  shallow-wrapper 'shallow-wrapper (current-inspector) (auto-write)
  
  (#:gen gen:equal-mode+hash
    
    (define (equal-mode-proc a b recur now?)
      (dissect a (shallow-wrapper a-value)
      /dissect b (shallow-wrapper b-value)
      /equal-always?/recur a-value b-value /fn a b #t))
    
    (define (hash-mode-proc v recur now?)
      (dissect v (shallow-wrapper v-value)
      /equal-always-hash-code/recur v-value /fn v
        (uninformative-hash-code)))
    
    ))

(define (list-injection-trie-iteration-sequence trie)
  (dissect trie (list nil-m cons-tries)
  /apply in-sequences
    (expect nil-m (just kv) (list)
      (dissect kv (list k v)
      /in-parallel (in-value k) (in-value v)))
    (for/list
      (
        [ (elem trie)
          (in-sequences
            (gloss-iteration-sequence cons-tries))])
      (list-injection-trie-iteration-sequence trie))))

(define
  (rider-and-list-injection-trie-update-maybe-knowable
    rider-and-t current-k-as-list overall-k on-rider-and-m-knowable)
  (dissect rider-and-t (list rider /list nil-m cons-tries)
  /expect current-k-as-list (cons elem current-k-as-list)
    (w- overall-k
      (expect nil-m (just old-kv) overall-k
      /dissect old-kv (list old-k old-v)
        old-k)
    /knowable-map
      (on-rider-and-m-knowable /maybe-map nil-m /dissectfn (list k v)
        v)
    /dissectfn (list rider m)
      (list rider
        (list (maybe-map m /fn v /list overall-k v) cons-tries)))
  /knowable-map
    (rider-and-gloss-update-maybe-knowable
      (list rider cons-tries)
      elem
      (dissectfn (list rider trie-m)
        (w- trie
          (mat trie-m (just trie) trie
          /list (nothing) (gloss-union-of-zero))
        /knowable-map
          (rider-and-list-injection-trie-update-maybe-knowable
            (list rider trie)
            current-k-as-list
            overall-k
            on-rider-and-m-knowable)
        /dissectfn (list rider trie)
          (list rider /just trie))))
  /dissectfn (list rider cons-tries)
    (list rider /list nil-m cons-tries)))

(define/own-contract
  (make-glossesque-sys-impl-from-list-injection
    gs-for-shallow get-->->list-is-constant? get-->->list)
  (->
    glossesque-sys?
    (-> glossesque-sys? boolean?)
    (-> glossesque-sys? (-> any/c (-> any/c list?)))
    glossesque-sys-impl?)
  (make-glossesque-sys-impl
    
    #:glossesque-union-of-zero
    (fn gs
      (glossesque-sys-glossesque-union-of-zero gs-for-shallow))
    
    #:glossesque-skm-union-of-two-knowable
    (fn gs state a b skm-union-knowable
      (glossesque-sys-glossesque-skm-union-of-two-knowable
        gs-for-shallow state a b
        (fn state k a-->list-and-trie-m b-->list-and-trie-m
          (w- ->list
            (mat a-->list-and-trie-m (just a-->list-and-trie)
              (dissect a-->list-and-trie (list a-->list a-trie)
                a-->list)
            /dissect b-->list-and-trie-m (just b-->list-and-trie)
              (dissect b-->list-and-trie (list b-->list b-trie)
                b-->list))
          /w- a-trie-m
            (maybe-map a-->list-and-trie-m /dissectfn
              (list a-->list a-trie)
              a-trie)
          /w- b-trie-m
            (maybe-map b-->list-and-trie-m /dissectfn
              (list b-->list b-trie)
              b-trie)
          /w- ->->list-is-constant? (get-->->list-is-constant? gs)
          ; NOTE OPTIMIZATION: For things that have indeterminate
          ; encodings as ordered lists (namely, `hash?` and `gloss?`
          ; values), we have to use a more exhaustive method of
          ; inserting each value of one trie into the other,
          ; relistifying each key we insert in in terms of the other
          ; trie's listifier. Technically, the asymptotic time
          ; complexity of both this and the usual merging are probably
          ; about O(n), but this "more exhaustive method" makes O(n)
          ; invocations to `->list`, while the usual method makes
          ; none.
          /knowable-bind
            (if
              (and
                (just? a-trie-m)
                (just? b-trie-m)
                (not ->->list-is-constant?))
              (dissect a-->list-and-trie-m
                (just /list a-->list a-trie)
              /dissect b-->list-and-trie-m
                (just /list b-->list b-trie)
              /knowable-bind
                (w-loop process-a-trie-knowable
                  state-and-b (list state b-trie)
                  result a-trie
                  
                  (dissect result (list nil-m cons-tries)
                  /knowable-bind
                    (expect nil-m (just kv)
                      (known /list state-and-b result)
                    /dissect kv (list k a-v)
                    /dissect state-and-b (list state b-trie)
                    /knowable-bind
                      (rider-and-list-injection-trie-update-maybe-knowable
                        (list (trivial) b-trie)
                        (b-->list k)
                        k
                        (dissectfn (list (trivial) b-v-m)
                          (known /list b-v-m /nothing)))
                    /dissectfn (list b-v-m b-trie)
                    /rider-and-list-injection-trie-update-maybe-knowable
                      (list (list state b-trie) result)
                      (list)
                      k
                      (dissectfn (list state a-v-m)
                        (skm-union-knowable state k a-v-m b-v-m)))
                  /dissectfn (list state-and-b /list nil-m cons-tries)
                  /w-loop process-a-trie-entries-knowable
                    state-and-b state-and-b
                    
                    trie-entries
                    (sequence->stream /in-values-sequence
                      (gloss-iteration-sequence cons-tries))
                    
                    result result
                    
                    (expect trie-entries
                      (stream* trie-entry trie-entries)
                      (known /list state-and-b result)
                    /dissect trie-entry (cons elem trie)
                    /knowable-bind
                      (process-a-trie-knowable state-and-b trie)
                    /dissectfn (list state-and-b trie)
                    /process-a-trie-entries-knowable
                      state-and-b trie-entries trie)))
              /dissectfn (list (list state b-trie) result)
              /w-loop next
                state state
                result result
                
                b
                (sequence->stream
                  (list-injection-trie-iteration-sequence b-trie))
                
                (if (stream-empty? b) (known /list state result)
                /let-values ([(k b-v) (stream-first b)])
                /w- b (stream-rest b)
                /knowable-bind
                  (rider-and-list-injection-trie-update-maybe-knowable
                    (list state result)
                    (->list k)
                    k
                    (dissectfn (list state a-v-m)
                      (skm-union-knowable state k a-v-m (just b-v))))
                /dissectfn (list state result)
                /next state result b))
            /w-loop next
              state state
              a-trie-m a-trie-m
              b-trie-m b-trie-m
              
              (dissect
                (mat a-trie-m (just a-trie) a-trie
                  (list (nothing) (gloss-union-of-zero)))
                (list a-nil-m a-cons-tries)
              /dissect
                (mat b-trie-m (just b-trie) b-trie
                  (list (nothing) (gloss-union-of-zero)))
                (list b-nil-m b-cons-tries)
              /knowable-bind
                (maybe-or-sm-union-of-two-knowable
                  state a-nil-m b-nil-m
                  (fn state a-nil-m b-nil-m
                    (w- k
                      (mat a-nil-m (just a-kv)
                        (dissect a-kv (list a-k a-v)
                          a-k)
                      /dissect b-nil-m (just b-kv)
                        (dissect b-kv (list b-k b-v)
                          b-k))
                    /skm-union-knowable state k
                      (maybe-map a-nil-m /dissectfn (list k v) v)
                      (maybe-map b-nil-m /dissectfn (list k v) v))))
              /dissectfn (list state nil-m)
              /knowable-bind
                (gloss-skm-union-of-two-knowable
                  state a-cons-tries b-cons-tries
                  (fn state elem a-trie-m b-trie-m
                    (knowable-map (next state a-trie-m b-trie-m)
                    /dissectfn (list state trie)
                      (list state /just trie))))
              /dissectfn (list state cons-tries)
              /known /list state /just /list nil-m cons-tries))
          /dissectfn (list state trie)
          /known /list state /just /list ->list trie))))
    
    #:glossesque-ref-maybe-knowable
    (fn gs g k
      (knowable-bind
        (glossesque-sys-glossesque-ref-maybe-knowable gs-for-shallow g
          (shallow-wrapper k))
      /fn ->list-and-trie-maybe
      /expect ->list-and-trie-maybe (just ->list-and-trie)
        (known /nothing)
      /dissect ->list-and-trie (list ->list trie)
      /w-loop next trie trie k (->list k)
        (dissect trie (list nil-m cons-tries)
        /expect k (cons elem k)
          (known /maybe-map nil-m /dissectfn (list k v) v)
        /knowable-bind
          (gloss-ref-maybe-knowable cons-tries elem)
        /fn trie
        /next trie k)))
    
    #:rider-and-glossesque-update-maybe-knowable
    (fn gs rider-and-g k on-rider-and-m-knowable
      (dissect rider-and-g (list rider g)
      /glossesque-sys-rider-and-glossesque-update-maybe-knowable
        gs-for-shallow (list rider g) (shallow-wrapper k)
        (dissectfn (list rider ->list-and-trie-m)
          (w- ->->list (get-->->list gs)
          /dissect
            (mat ->list-and-trie-m (just ->list-and-trie)
              ->list-and-trie
              (list
                (->->list k)
                (list (nothing) (gloss-union-of-zero))))
            (list ->list trie)
          /knowable-bind
            (rider-and-list-injection-trie-update-maybe-knowable
              (list rider trie)
              (->list k)
              k
              (fn rider-and-m
                (on-rider-and-m-knowable rider-and-m)))
          /dissectfn (list rider trie)
          /dissect trie (list nil-m cons-tries)
          /known /list rider
            (maybe-if
              (not /and (nothing? nil-m) (gloss-empty? cons-tries))
              (fn /list ->list trie))))))
    
    #:glossesque-empty?
    (fn gs g
      (zero? /glossesque-sys-glossesque-count gs g))
    
    ; NOTE: Since we're using `counted-glossesque-sys` for this type
    ; of glossesque, this method and `#:glossesque-empty?` won't
    ; actually be called.
    #:glossesque-count
    (fn gs g
      (sequence-length
        (glossesque-sys-glossesque-iteration-sequence gs g)))
    
    #:glossesque-iteration-sequence
    (fn gs g
      (apply in-sequences
        (for/list
          (
            [ (tag ->list-and-trie)
              (in-sequences
                (glossesque-sys-glossesque-iteration-sequence
                  gs-for-shallow g))])
          (dissect ->list-and-trie (list ->list trie)
          /list-injection-trie-iteration-sequence trie))))
    
    ))

(define-imitation-simple-struct
  (equal-always-from-list-injection-glossesque-sys?
    equal-always-from-list-injection-glossesque-sys-->->list-is-constant?
    equal-always-from-list-injection-glossesque-sys-->->list)
  equal-always-from-list-injection-glossesque-sys-unguarded
  'equal-always-from-list-injection-glossesque-sys (current-inspector)
  (auto-write)
  (#:prop prop:glossesque-sys
    (make-glossesque-sys-impl-from-list-injection
      (equal-always-atom-glossesque-sys)
      (dissectfn
        (equal-always-from-list-injection-glossesque-sys-unguarded
          ->->list-is-constant? ->->list)
        ->->list-is-constant?)
      (dissectfn
        (equal-always-from-list-injection-glossesque-sys-unguarded
          ->->list-is-constant? ->->list)
        ->->list))))

(define/own-contract
  (equal-always-from-list-injection-glossesque-sys
    #:->list [->list #f]
    
    #:->->list
    [ ->->list
      (if ->list
        (dissectfn _ ->list)
        (raise-arguments-error 'equal-always-from-list-injection-glossesque-sys
          "expected either #:->list or #:->->list to be provided"))]
    
    )
  (->*
    ()
    (
      #:->list (or/c #f (-> any/c list?))
      #:->->list (-> any/c (-> any/c list?)))
    glossesque-sys?)
  (counted-glossesque-sys
    (equal-always-from-list-injection-glossesque-sys-unguarded
      (not /not ->list)
      ->->list)))

(define-imitation-simple-struct
  (equal-always-indistinct-from-list-injection-glossesque-sys?
    equal-always-indistinct-from-list-injection-glossesque-sys-->->list-is-constant?
    equal-always-indistinct-from-list-injection-glossesque-sys-->->list)
  equal-always-indistinct-from-list-injection-glossesque-sys-unguarded
  'equal-always-indistinct-from-list-injection-glossesque-sys
  (current-inspector)
  (auto-write)
  (#:prop prop:glossesque-sys
    (make-glossesque-sys-impl-from-list-injection
      (equal-always-indistinct-atom-glossesque-sys)
      (dissectfn
        (equal-always-indistinct-from-list-injection-glossesque-sys-unguarded
          ->->list-is-constant? ->->list)
        ->->list-is-constant?)
      (dissectfn
        (equal-always-indistinct-from-list-injection-glossesque-sys-unguarded
          ->->list-is-constant? ->->list)
        ->->list))))

(define/own-contract
  (equal-always-indistinct-from-list-injection-glossesque-sys
    #:->list [->list #f]
    
    #:->->list
    [ ->->list
      (if ->list
        (dissectfn _ ->list)
        (raise-arguments-error 'equal-always-indistinct-from-list-injection-glossesque-sys
          "expected either #:->list or #:->->list to be provided"))]
    
    )
  (->*
    ()
    (
      #:->list (or/c #f (-> any/c list?))
      #:->->list (-> any/c (-> any/c list?)))
    glossesque-sys?)
  (counted-glossesque-sys
    (equal-always-indistinct-from-list-injection-glossesque-sys-unguarded
      (not /not ->list)
      ->->list)))

; TODO: See if we should export this.
(define/own-contract (shallowly-unchaperoned? copy v)
  (-> (-> any/c any/c) any/c boolean?)
  (chaperone-of? (copy v) v))

(define/own-contract
  (make-glossesque-sys-impl-for-chaperone=-copiable
    gs-for-equal-always
    make-gs-for-wrapped-chaperone=-assuming-equal-always
    get-copy)
  (->
    glossesque-sys?
    (-> (-> any/c any/c boolean?) glossesque-sys?)
    (-> glossesque-sys? (-> any/c any/c))
    glossesque-sys-impl?)
  (make-glossesque-sys-impl-for-chaperone=-atom
    gs-for-equal-always
    (fn gs
      (w- copy (get-copy gs)
      /glossesque-sys-map-key
        #:granted-key
        (fn k
          (maybe-if (not /shallowly-unchaperoned? copy k) /fn k))
        
        (make-gs-for-wrapped-chaperone=-assuming-equal-always /fn a b
          (match (list a b)
            [(list (list (nothing) _) (list (nothing) _)) #t]
            [ (list (list (just a-value) _) (list (just b-value) _))
              (atom-chaperone=? a-value b-value)]
            [_ #f]))))))

(define-imitation-simple-struct
  (chaperone=-copiable-glossesque-sys?
    chaperone=-copiable-glossesque-sys-get-copy)
  chaperone=-copiable-glossesque-sys-unguarded
  'chaperone=-copiable-glossesque-sys (current-inspector) (auto-write)
  (#:prop prop:glossesque-sys
    (make-glossesque-sys-impl-for-chaperone=-copiable
      (equal-always-atom-glossesque-sys)
      (fn wrapped-key=?
        (equality-check-indistinct-atom-glossesque-sys /fn a b
          (wrapped-key=? a b)))
      (dissectfn (chaperone=-copiable-glossesque-sys-unguarded copy)
        copy))))

(define/own-contract (chaperone=-copiable-glossesque-sys #:copy copy)
  (-> #:copy (-> any/c (-> any/c list?)) glossesque-sys?)
  (counted-glossesque-sys
    (chaperone=-copiable-glossesque-sys-unguarded copy)))

(define-imitation-simple-struct
  (chaperone=-indistinct-copiable-glossesque-sys?
    chaperone=-indistinct-copiable-glossesque-sys-get-copy)
  chaperone=-indistinct-copiable-glossesque-sys-unguarded
  'chaperone=-indistinct-copiable-glossesque-sys
  (current-inspector)
  (auto-write)
  (#:prop prop:glossesque-sys
    (make-glossesque-sys-impl-for-chaperone=-copiable
      (equal-always-indistinct-atom-glossesque-sys)
      (fn wrapped-key=?
        (equality-check-indistinct-atom-glossesque-sys /fn a b
          (wrapped-key=? a b)))
      (dissectfn
        (chaperone=-indistinct-copiable-glossesque-sys-unguarded copy)
        copy))))

(define/own-contract
  (chaperone=-indistinct-copiable-glossesque-sys #:copy copy)
  (-> #:copy (-> any/c (-> any/c list?)) glossesque-sys?)
  (counted-glossesque-sys
    (chaperone=-indistinct-copiable-glossesque-sys-unguarded copy)))

(define/own-contract (normalized-glossesque-sys granted-key)
  (-> (-> any/c any/c) glossesque-sys?)
  (glossesque-sys-map-key (equal-always-atom-glossesque-sys)
    #:granted-key (fn k /granted-key k)))

(define/own-contract (terminal-glossesque-sys)
  (-> glossesque-sys?)
  (normalized-glossesque-sys /fn k #f))

; Given two lists, this checks whether they have the same length and
; `eq?` elements.
;
; TODO: Consider exporting this.
;
(define/own-contract (list-elements-eq? a b)
  (-> list? list? boolean?)
  (and (= (length a) (length b))
    (for/and ([a-elem (in-list a)] [b-elem (in-list b)])
      (eq? a-elem b-elem))))

; This is an appropriate `prop:expressly-smooshable-dynamic-type`
; implementation for immutable tuple data structures that can't be
; chaperoned or impersonated, information-ordered in a way that's
; consistent with `chaperone-of?` as long as the elements' information
; orderings are.
;
; The given `known-distinct?` indicates whether a false result of
; the underlying comparison means we positively know the values are
; distinct (rather than just not knowing that they're equal). The
; given `known-discrete?` indicates whether knowing that two values
; are distinct means we positively know that they aren't related by
; ordering either. (Otherwise, their results for those failed ordered
; comparisons are unknown.) These situations only apply for failures
; of shallow comparison; a known failure of comparison of elements
; still results in a known failure of comparison overall.
;
; The given `->->list` function should take an inhabitant (a value
; which passes the given `inhabitant?` predicate) and return a
; function that takes an inhabitant of similar structure and returns
; the same list of elements that would be passed to the callback of
; `equal-always?/recur`. By "similar structure," we mean that the
; second inhabitant is `equal-always?/recur` to the first if the
; recursive equality check callback always returns `#t` (or, if
; `inhabitant-shallowly-equal-always?-knowable` is given, we use
; that and assume that its true results are consistent with
; `equal-always?/recur`). The two-stage approach here lets us
; establish an iteration order and then use that iteration order
; consistently for every operand, even if our inhabitants are hash
; tables and don't have an entirely deterministic iteration order.
;
; The given `->->list` and `example-and-list->` functions should
; specify an isomorphism between inhabitants and some set of lists.
; This isn't possible for every type, so this is only a suitable
; abstraction when it is possible to satisfy this condition.
;
; Level 0:
;   <=, >=, path-related, join, meet:
;     Same as the description of level 1 path-related (as a check when
;     applicable), but with "the same smoosh" referring to this
;     level-0 smoosh or check, and translating a known nothing result
;     that comes from a failed shallow comparison into an unknown
;     result if `known-discrete?` is false.
;   ==:
;     Same as the description of level 1 path-related.
; Level 1:
;   path-related, join, meet, ==:
;     If the operands do not both have known results for the given
;     `inhabitant?` predicate, which may be an instance of
;     `prop:expressly-knowable-predicate?`, or if neither of them has
;     a known true result for it, then unknown.
;     
;     Otherwise, if at least one operand has a known true result for
;     `inhabitant?` and at least one has a known false result for it,
;     then a known nothing.
;     
;     Otherwise, if comparing the operands without regard for their
;     elements or their impersonator or chaperone wrappers using the
;     given `inhabitant-shallowly-equal-always?-knowable` (usually
;     `equal-always?/recur`) returns an unknown result, then unknown.
;     
;     Otherwise, if it shows they differ and `known-distinct?` is
;     true, then a known nothing. (This is the known nothing result
;     that level 0 smooshes and checks other than == adjust into an
;     unknown result when `known-discrete?` is false.)
;     
;     Otherwise, if it shows they differ, then unknown.
;     
;     Otherwise, if the results of smooshing corresponding elements
;     under the same smoosh include a known nothing, then a known
;     nothing.
;     
;     Otherwise, if those recursive results include an unknown, then
;     unknown.
;     
;     Otherwise, if those recursive results are `eq?` to the elements
;     of an operand, then the first such operand.
;     
;     Otherwise, a new inhabitant (created by the given
;     `example-and-list->` function using the first operand as the
;     example) whose elements are those recursive results.
;   <=, >=:
;     If the operands do not both have known results for the given
;     `inhabitant?` predicate, which may be an instance of
;     `prop:expressly-knowable-predicate?`, or if neither of them has
;     a known true result for it, then unknown.
;     
;     Otherwise, if at least one operand has a known true result for
;     `inhabitant?` and at least one has a known false result for it,
;     then a known `#f`.
;     
;     Otherwise, if comparing the operands without regard for their
;     elements or their impersonator or chaperone wrappers using the
;     given `inhabitant-shallowly-equal-always?-knowable` (usually
;     `equal-always?/recur`) returns an unknown result, then unknown.
;     
;     Otherwise, if it shows they differ and `known-distinct?` is
;     true, then a known `#f`.
;     
;     Otherwise, if it shows they differ, then unknown.
;     
;     Otherwise, if the results of smooshing corresponding elements
;     under the same smoosh include a known `#f`, then a known `#f`.
;     
;     Otherwise, if those recursive results include an unknown, then
;     unknown.
;     
;     Otherwise, a known `#t`.
; Level 2+:
;   path-related, join, meet, ==:
;     Same as the description of level 1 ==, but with "the same
;     smoosh" referring to this level-2+ smoosh.
;   <=, >=:
;     Same as the description of level 1 == as a check, but with "the
;     same smoosh" referring to this level-2+ check, understanding the
;     recursive smoosh result always to be the first operand upon
;     success so that finding an acceptable result for the overall
;     smoosh is possible.
;
(define/own-contract
  (make-expressly-smooshable-dynamic-type-impl-from-equal-always-list-isomorphism
    #:known-distinct? [known-distinct? #t]
    #:known-discrete? [known-discrete? #f]
    #:self-get-any-dynamic-type self-get-any-dynamic-type
    #:inhabitant? inhabitant?
    #:->list [->list #f]
    
    #:->->list
    [ ->->list
      (if ->list
        (dissectfn _ ->list)
        (raise-arguments-error 'make-expressly-smooshable-dynamic-type-impl-from-equal-always-list-isomorphism
          "expected either #:->list or #:->->list to be provided"))]
    
    #:example-and-list-> example-and-list->
    
    #:inhabitant-shallowly-equal-always?-knowable
    [ inhabitant-shallowly-equal-always?-knowable
      (fn a b /known /equal-always?/recur a b /fn a-elem b-elem #t)]
    
    #:get-smoosh-of-zero-reports
    [ get-smoosh-of-zero-reports
      (fn self
        (uninformative-smoosh-reports))])
  (->*
    (
      #:self-get-any-dynamic-type (-> any/c any/c)
      #:inhabitant? (-> any/c boolean?)
      #:example-and-list-> (-> any/c list? any/c))
    (
      #:known-distinct? boolean?
      #:known-discrete? boolean?
      #:->list (or/c #f (-> any/c list?))
      #:->->list (-> any/c (-> any/c list?))
      
      #:inhabitant-shallowly-equal-always?-knowable
      (-> any/c any/c (knowable/c boolean?))
      
      #:get-smoosh-of-zero-reports
      (-> any/c (sequence/c smoosh-report?))
      
      )
    expressly-smooshable-dynamic-type-impl?)
  (make-expressly-smooshable-dynamic-type-impl
    #:get-smoosh-of-zero-reports get-smoosh-of-zero-reports
    
    #:get-smoosh-of-one-reports
    (fn self a
      (w- any-dt (self-get-any-dynamic-type self)
      /expect (inhabitant? a) #t (uninformative-smoosh-reports)
      /w- ->list (->->list a)
      /w- a-list (->list a)
      /smoosh-reports-zip*-map
        (list-map a-list /fn a-elem
          (dynamic-type-get-smoosh-of-one-reports any-dt a-elem))
        #:on-result-knowable-promise-maybe-knowable-promise
        (fn kpmkp-list
          (maybe-min-knowable-promise-zip*-map kpmkp-list /fn kp-list
            (knowable-promise-zip*-map kp-list /fn result-list
              (if (list-elements-eq? result-list a-list) a
              /example-and-list-> a result-list))))))
    
    #:get-smoosh-and-comparison-of-two-reports
    (fn self a b
      (w- any-dt (self-get-any-dynamic-type self)
      /expect (call-knowable inhabitant? a) (known a-inhabits?)
        (uninformative-smoosh-and-comparison-of-two-reports)
      /expect (call-knowable inhabitant? b) (known b-inhabits?)
        (uninformative-smoosh-and-comparison-of-two-reports)
      /expect (or a-inhabits? b-inhabits?) #t
        (uninformative-smoosh-and-comparison-of-two-reports)
      /expect (and a-inhabits? b-inhabits?) #t
        (smoosh-and-comparison-of-two-reports-with-hesitation-at-discrepancies
          (false-smoosh-and-comparison-of-two-reports))
      ; If the comparing the operands without comparing their elements
      ; has an unknown result, we return an unknown result as well.
      /expect (inhabitant-shallowly-equal-always?-knowable a b)
        (known a-shallowly-equal-always-b?)
        (uninformative-smoosh-and-comparison-of-two-reports)
      ; Otherwise, if it returns `#f`, we return a known nothing (when
      ; doing a smoosh, or `#f` when doing a check). This is sometimes
      ; adjusted to an unknown result according to `known-distinct?`
      ; and `known-discrete?`.
      /if (not a-shallowly-equal-always-b?)
        (smoosh-and-comparison-of-two-reports-with-hesitation-at-discrepancies
          #:known-distinct? known-distinct?
          #:known-discrete? known-discrete?
          (false-smoosh-and-comparison-of-two-reports))
      /w- ->list (->->list a)
      /w- a-list (->list a)
      /w- b-list (->list b)
      /smoosh-and-comparison-of-two-reports-zip*-map
        (list-zip-map a-list b-list /fn a-elem b-elem
          (dynamic-type-get-smoosh-and-comparison-of-two-reports
            any-dt a-elem b-elem))
        #:on-check-result-knowable-promise
        (fn kp-list
          (boolean-and-knowable-promise-zip* kp-list))
        #:on-smoosh-result-knowable-promise-maybe-knowable-promise
        (fn kpmkp-list
          (maybe-min-knowable-promise-zip*-map kpmkp-list /fn kp-list
            (knowable-promise-zip*-map kp-list /fn result-list
              (if (list-elements-eq? result-list a-list) a
              /if (list-elements-eq? result-list b-list) b
              /example-and-list-> a result-list))))))
    
    ))

; This is an appropriate `prop:expressly-smooshable-dynamic-type`
; implementation for immutable tuple data structures and their
; chaperones, information-ordered in a way that's consistent with
; `chaperone-of?` as long as the elements' information orderings are.
;
; In order to be able to check `(chaperone-of? a b)` in a "shallow"
; way without redundant (quadratic-time) work checking all the
; elements, this uses a multi-step process: First it checks
; `(chaperone-of? (copy b) b)` to see if `b` has only non-interposing
; chaperone wrappers. (This step doesn't need to traverse into the
; elements because they'll be `eq?`.) If `b` does have only
; non-interposing chaperone wrappers, then we know the chaperone and
; impersonator wrappers on `a` are at least as extant, making `a`
; shallowly chaperone-of `b`. Otherwise, we know that `b` has some
; impersonator wrapper or interposing chaperone wrapper, in which case
; we know we can run `(chaperone-of? a b)` without it having to
; traverse all the way into the elements; once it hits that wrapper on
; `b`, it'll check `a`'s unwrappings for a wrapper that's `eq?` and
; stop if it's not there.
;
; The given `->->list` function should take an inhabitant (a value
; which passes the given `inhabitant?` predicate) and return a
; function that takes an inhabitant of similar structure and returns
; the same list of elements that would be passed to the callback of
; `equal-always?/recur`. By "similar structure," we mean that the
; second inhabitant is `equal-always?/recur` to the first if the
; recursive equality check callback always returns `#t` (or, if
; `inhabitant-shallowly-equal-always?-knowable` is given, we use
; that and assume that its true results are consistent with
; `equal-always?/recur`). The two-stage approach here lets us
; establish an iteration order and then use that iteration order
; consistently for every operand, even if our inhabitants are hash
; tables and don't have an entirely deterministic iteration order.
;
; The given `->->list` and `example-and-list->` functions should
; specify an isomorphism between inhabitants and some set of lists.
; This isn't possible for every type, so this is only a suitable
; abstraction when it is possible to satisfy this condition.
;
; The given `copy` function should return an inhabitant that's
; `equal-always?` to its input inhabitant, but that doesn't have any
; impersonator wrappers or interposing chaperone wrappers.
;
; Level 0:
;   <=, >=, path-related, join, meet:
;     Same as the description of level 1 path-related (as a check when
;     applicable), but with "the same smoosh" referring to this
;     level-0 smoosh or check, and translating a known nothing result
;     that comes from a failed shallow comparison into an unknown
;     result if `known-discrete?` is false.
;   ==:
;     Same as the description of level 1 path-related.
; Level 1:
;   path-related, join, meet, ==:
;     If the operands do not both have known results for the given
;     `inhabitant?` predicate, which may be an instance of
;     `prop:expressly-knowable-predicate?`, or if neither of them has
;     a known true result for it, then unknown.
;     
;     Otherwise, if at least one operand has a known true result for
;     `inhabitant?` and at least one has a known false result for it,
;     then a known nothing.
;     
;     Otherwise, if comparing the operands without regard for their
;     elements or their impersonator or chaperone wrappers using the
;     given `inhabitant-shallowly-equal-always?-knowable` (usually
;     `equal-always?/recur`) returns an unknown result, then unknown.
;     
;     Otherwise, if it shows they differ and `known-distinct?` is
;     true, then a known nothing. (This is the known nothing result
;     that level 0 smooshes and checks other than == adjust into an
;     unknown result when `known-discrete?` is false.)
;     
;     Otherwise, if it shows they differ, then unknown.
;     
;     Otherwise, if the results of smooshing corresponding elements
;     under the same smoosh include a known nothing, then a known
;     nothing.
;     
;     Otherwise, if those recursive results include an unknown, then
;     unknown.
;     
;     Otherwise, if those recursive results are `eq?` to the elements
;     of an operand that counts as an acceptable result, then the
;     first such operand.
;     
;     Otherwise, build a new inhabitant (created by the given
;     `example-and-list->` function using the first operand as the
;     example) whose elements are those recursive results. If the new
;     inhabitant is not an acceptable result, then unknown.
;     
;     Otherwise, that new inhabitant.
;     
;     Where "acceptable result" means:
;       If we're doing path-related:
;         Every result is acceptable.
;       If we're doing join:
;         Every result is acceptable if it's shallowly chaperone-of
;         every operand.
;         
;         (We'll allow for the possibility that Racket will introduce
;         chaperone wrappers that are chaperone-of each of two
;         preexisting chaperones, so we'll treat some results as
;         unknown even though a more Racket-version-pinned design
;         might treat them as known nothings.)
;       If we're doing meet:
;         Every operand is acceptable if each of the operands is
;         shallowly chaperone-of it.
;         
;         (Note that even though the operands will all be shallowly
;         chaperone-of the new inhabitant, sometimes there's a value
;         properly shallowly chaperone-of it that the operands would
;         all be shallowly chaperone-of too, so unless an acceptable
;         result is found among the operands, the result will be
;         unknown.)
;       If we're doing ==:
;         Every operand is acceptable if the operands are all
;         shallowly chaperone-of each other.
;         
;         (We'll allow for the possibility that Racket will introduce
;         chaperone wrappers that aren't compared by object identity,
;         so that wrappers around non-`eq?` values could be analogous;
;         with this in mind, if the element smoosh results aren't all
;         `eq?` to the elements of any one operand, and if the
;         operands aren't shallowly chaperoneless, sometimes they'll
;         all be shallowly chaperone-of each other but our result will
;         still have to be unknown.)
;   <=, >=:
;     If the operands do not both have known results for the given
;     `inhabitant?` predicate, which may be an instance of
;     `prop:expressly-knowable-predicate?`, or if neither of them has
;     a known true result for it, then unknown.
;     
;     Otherwise, if at least one operand has a known true result for
;     `inhabitant?` and at least one has a known false result for it,
;     then a known `#f`.
;     
;     Otherwise, if comparing the operands without regard for their
;     elements or their impersonator or chaperone wrappers using the
;     given `inhabitant-shallowly-equal-always?-knowable` (usually
;     `equal-always?/recur`) returns an unknown result, then unknown.
;     
;     Otherwise, if it shows they differ and `known-distinct?` is
;     true, then a known `#f`.
;     
;     Otherwise, if it shows they differ, then unknown.
;     
;     Otherwise, if the results of smooshing corresponding elements
;     under the same smoosh include a known `#f`, then a known `#f`.
;     
;     Otherwise, if those recursive results include an unknown, then
;     unknown.
;     
;     Otherwise, if the element we're proposing to be greater is
;     shallowly chaperone-of the other one, then a known `#t`.
;     
;     Otherwise, unknown.
;     
;     (We'll allow for the possibility that values which differ only
;     in the failure of a chaperone-of check could actually pass the
;     chaperone-of check in the future when they're written to reuse
;     chaperone-wrapped values more diligently. This kind of rewrite
;     may be more feasible if and when Racket introduces
;     chaperone-wrapping operations that create equivalent wrappers in
;     some reliable way. As such, we'll treat some results as unknown
;     even though a more Racket-version-pinned design might treat them
;     as known nothings.)
;   Where for values X and inhabitants Y, "X is shallowly chaperone-of
;   Y" means:
;     If Y is shallowly unchaperoned:
;       It's true.
;     Otherwise:
;       It's true iff `(chaperone-of? X Y)` (which in this case will
;       be guaranteed not to traverse any deeper than unwrapping
;       chaperone wrappers).
;   Where for an inhabitant Y, "Y is shallowly unchaperoned" means:
;     It's true iff `(chaperone-of? (copy Y) Y)` using the given
;     `copy` function.
;     
;     (In other words, there are no impersonator wrappers or
;     interposing chaperone wrappers around the inhabitant Y.)
;     
;     (Note that some chaperone wrappers are non-interposing, and
;     there may be wrappers like those around Y; these don't affect
;     its `chaperone-of?` behavior.)
; Level 2+:
;   path-related, join, meet, ==:
;     Same as the description of level 1 ==, but with "the same
;     smoosh" referring to this level-2+ smoosh.
;   <=, >=:
;     Same as the description of level 1 == as a check, but with "the
;     same smoosh" referring to this level-2+ check, understanding the
;     recursive smoosh result always to be the first operand upon
;     success so that finding an acceptable result for the overall
;     smoosh is possible.
;
(define/own-contract
  (make-expressly-smooshable-dynamic-type-impl-from-chaperone-of-list-isomorphism
    #:known-distinct? [known-distinct? #t]
    #:known-discrete? [known-discrete? #f]
    #:self-get-any-dynamic-type self-get-any-dynamic-type
    #:inhabitant? inhabitant?
    #:->list [->list #f]
    
    #:->->list
    [ ->->list
      (if ->list
        (dissectfn _ ->list)
        (raise-arguments-error 'make-expressly-smooshable-dynamic-type-impl-from-chaperone-of-list-isomorphism
          "expected either #:->list or #:->->list to be provided"))]
    
    #:example-and-list-> example-and-list->
    
    #:inhabitant-shallowly-equal-always?-knowable
    [ inhabitant-shallowly-equal-always?-knowable
      (fn a b /known /equal-always?/recur a b /fn a-elem b-elem #t)]
    
    #:copy [copy (fn v /example-and-list-> v /(->->list v) v)]
    
    #:get-smoosh-of-zero-reports
    [ get-smoosh-of-zero-reports
      (fn self
        (uninformative-smoosh-reports))])
  (->*
    (
      #:self-get-any-dynamic-type (-> any/c any/c)
      #:inhabitant? (-> any/c boolean?)
      #:example-and-list-> (-> any/c list? any/c))
    (
      #:known-distinct? boolean?
      #:known-discrete? boolean?
      #:->list (or/c #f (-> any/c list?))
      #:->->list (-> any/c (-> any/c list?))
      
      #:inhabitant-shallowly-equal-always?-knowable
      (-> any/c any/c (knowable/c boolean?))
      
      #:copy (-> any/c any/c)
      
      #:get-smoosh-of-zero-reports
      (-> any/c (sequence/c smoosh-report?))
      
      )
    expressly-smooshable-dynamic-type-impl?)
  (w- inhabitant-shallowly-unchaperoned?
    (fn v
      (shallowly-unchaperoned? copy v))
  /make-expressly-smooshable-dynamic-type-impl
    #:get-smoosh-of-zero-reports get-smoosh-of-zero-reports
    
    #:get-smoosh-of-one-reports
    (fn self a
      (w- any-dt (self-get-any-dynamic-type self)
      /expect (inhabitant? a) #t (uninformative-smoosh-reports)
      /w- ->list (->->list a)
      /w- a-list (->list a)
      /dissect
        (smoosh-reports-zip*-map
          (list-map a-list /fn a-elem
            (dynamic-type-get-smoosh-of-one-reports any-dt a-elem))
          #:on-result-knowable-promise-maybe-knowable-promise
          (fn kpmkp-list
            (maybe-min-knowable-promise-zip*-map kpmkp-list /fn kp-list
              (knowable-promise-zip*-map kp-list /fn result-list
                result-list))))
        (stream* report-0 report-1 report-2+)
      /w- a-shallowly-unchaperoned?-promise
        (delay /inhabitant-shallowly-unchaperoned? a)
      /w- on-smoosh-result-knowable-promise-maybe-knowable-promise
        (fn result-needs-to-be-chaperone-of?
          (fn result-list-kpmkp
            (promise-map result-list-kpmkp /fn list-kpmk
              (knowable-map list-kpmk /fn list-kpm
                (maybe-map list-kpm /fn list-kp
                  (promise-map list-kp /fn list-k
                    (knowable-bind list-k /fn result-list
                      (if (list-elements-eq? result-list a-list)
                        (known a)
                      /w- noncanonical-result
                        (example-and-list-> a result-list)
                      /if
                        ; If we're doing a particularly strict check
                        ; and the operand `a` is wrapped with
                        ; impersonators or interposing chaperones, we
                        ; have no `known?` result.
                        (or
                          (not result-needs-to-be-chaperone-of?)
                          (force
                            a-shallowly-unchaperoned?-promise))
                        (known noncanonical-result)
                      /unknown))))))))
      /stream*
        (smoosh-report-map report-0
          #:on-smoosh-result-knowable-promise-maybe-knowable-promise
          (on-smoosh-result-knowable-promise-maybe-knowable-promise
            #f))
        (smoosh-report-map report-1
          #:on-join-knowable-promise-maybe-knowable-promise
          (on-smoosh-result-knowable-promise-maybe-knowable-promise
            #t)
          #:on-meet-knowable-promise-maybe-knowable-promise
          (on-smoosh-result-knowable-promise-maybe-knowable-promise
            #t)
          #:on-==-knowable-promise-maybe-knowable-promise
          (on-smoosh-result-knowable-promise-maybe-knowable-promise
            #t)
          #:on-path-related-knowable-promise-maybe-knowable-promise
          (on-smoosh-result-knowable-promise-maybe-knowable-promise
            #f))
        (smoosh-reports-map report-2+
          #:on-smoosh-result-knowable-promise-maybe-knowable-promise
          (on-smoosh-result-knowable-promise-maybe-knowable-promise
            #t))))
    
    #:get-smoosh-and-comparison-of-two-reports
    (fn self a b
      (w- any-dt (self-get-any-dynamic-type self)
      /expect (call-knowable inhabitant? a) (known a-inhabits?)
        (uninformative-smoosh-and-comparison-of-two-reports)
      /expect (call-knowable inhabitant? b) (known b-inhabits?)
        (uninformative-smoosh-and-comparison-of-two-reports)
      /expect (or a-inhabits? b-inhabits?) #t
        (uninformative-smoosh-and-comparison-of-two-reports)
      /expect (and a-inhabits? b-inhabits?) #t
        (smoosh-and-comparison-of-two-reports-with-hesitation-at-discrepancies
          (false-smoosh-and-comparison-of-two-reports))
      ; If the comparing the operands without comparing their
      ; impersonator or chaperone wrappers or their elements has an
      ; unknown result, we return an unknown result as well.
      /expect (inhabitant-shallowly-equal-always?-knowable a b)
        (known a-shallowly-equal-always-b?)
        (uninformative-smoosh-and-comparison-of-two-reports)
      ; Otherwise, if it returns `#f`, we return a known nothing (when
      ; doing a smoosh, or `#f` when doing a check). This is sometimes
      ; adjusted to an unknown result according to `known-distinct?`
      ; and `known-discrete?`.
      /if (not a-shallowly-equal-always-b?)
        (smoosh-and-comparison-of-two-reports-with-hesitation-at-discrepancies
          #:known-distinct? known-distinct?
          #:known-discrete? known-discrete?
          (false-smoosh-and-comparison-of-two-reports))
      /w- ->list (->->list a)
      /w- a-list (->list a)
      /w- b-list (->list b)
      /dissect
        (smoosh-and-comparison-of-two-reports-zip*-map
          (list-zip-map a-list b-list /fn a-elem b-elem
            (dynamic-type-get-smoosh-and-comparison-of-two-reports
              any-dt a-elem b-elem))
          #:on-check-result-knowable-promise
          (fn kp-list
            (boolean-and-knowable-promise-zip* kp-list))
          #:on-smoosh-result-knowable-promise-maybe-knowable-promise
          (fn kpmkp-list
            (maybe-min-knowable-promise-zip*-map kpmkp-list /fn kp-list
              (knowable-promise-zip*-map kp-list /fn result-list
                result-list))))
        (app sequence->stream /stream* report-0 report-1 report-2+)
      /w- a-shallowly-unchaperoned?-promise
        (delay /inhabitant-shallowly-unchaperoned? a)
      /w- b-shallowly-unchaperoned?-promise
        (delay /inhabitant-shallowly-unchaperoned? b)
      /w- a-shallowly-chaperone-of-b?-promise
        (delay
          (or
            (force b-shallowly-unchaperoned?-promise)
            (chaperone-of? a b)))
      /w- b-shallowly-chaperone-of-a?-promise
        (delay
          (or
            (force a-shallowly-unchaperoned?-promise)
            (chaperone-of? b a)))
      ; Given an arbitrary value and an inhabitant, this checks
      ; whether they would pass `chaperone-of?` if we compared only
      ; their chaperone wrappers and not any other part of their
      ; immediate data or their elements. Like `chaperone-of?`, this
      ; takes constant time if the inhabitants are `eq?` themselves.
      /w- inhabitant-shallowly-chaperone-of?
        (fn s t
          (or (eq? s t)
          /if (and (eq? s b) (eq? t a))
            (force b-shallowly-chaperone-of-a?-promise)
          /if (and (eq? s a) (eq? t b))
            (force a-shallowly-chaperone-of-b?-promise)
          /or
            (cond
              [(eq? t a) (force a-shallowly-unchaperoned?-promise)]
              [(eq? t b) (force b-shallowly-unchaperoned?-promise)]
              [else (inhabitant-shallowly-unchaperoned? t)])
            (chaperone-of? s t)))
      /w- on-check-result-knowable-promise
        (fn should-a-be-small? should-b-be-small?
          (fn kp
            (promise-map kp /fn k
              (knowable-bind k /fn result
                (boolean-and-knowable-thunk-zip* /list
                  (fn /known result)
                  (fn /boolean-or-knowable-thunk-zip* /list
                    (fn /known /not should-a-be-small?)
                    (fn /falsable->uninformative-knowable
                      (inhabitant-shallowly-chaperone-of? b a)))
                  (fn /boolean-or-knowable-thunk-zip* /list
                    (fn /known /not should-b-be-small?)
                    (fn /falsable->uninformative-knowable
                      (inhabitant-shallowly-chaperone-of? a b))))))))
      /w- on-smoosh-result-knowable-promise-maybe-knowable-promise
        (fn acceptable-result?
          (fn result-list-kpmkp
            (promise-map result-list-kpmkp /fn list-kpmk
              (knowable-map list-kpmk /fn list-kpm
                (maybe-map list-kpm /fn list-kp
                  (promise-map list-kp /fn list-k
                    (knowable-bind list-k /fn result-list
                      (if
                        (and
                          (list-elements-eq? result-list a-list)
                          (acceptable-result? a))
                        (known a)
                      /if
                        (and
                          (list-elements-eq? result-list b-list)
                          (acceptable-result? b))
                        (known b)
                      /w- noncanonical-result
                        (example-and-list-> a result-list)
                      /if (acceptable-result? noncanonical-result)
                        (known noncanonical-result)
                      /unknown))))))))
      /w- chaperone=?-promise
        (delay
          (and
            (inhabitant-shallowly-chaperone-of? b a)
            (inhabitant-shallowly-chaperone-of? a b)))
      /w- ==-acceptable-result?
        (fn v
          (and
            (force chaperone=?-promise)
            (inhabitant-shallowly-chaperone-of? v a)))
      /w- path-related-acceptable-result?
        (fn v
          #t)
      /stream*
        (smoosh-and-comparison-of-two-report-map report-0
          #:on-check-result-knowable-promise
          (on-check-result-knowable-promise #f #f)
          #:on-smoosh-result-knowable-promise-maybe-knowable-promise
          (on-smoosh-result-knowable-promise-maybe-knowable-promise
            path-related-acceptable-result?))
        (smoosh-and-comparison-of-two-report-map report-1
          #:on-<=?-knowable-promise
          (on-check-result-knowable-promise #t #f)
          #:on->=?-knowable-promise
          (on-check-result-knowable-promise #f #t)
          #:on-join-knowable-promise-maybe-knowable-promise
          (on-smoosh-result-knowable-promise-maybe-knowable-promise
            (fn v
              (and
                (inhabitant-shallowly-chaperone-of? v a)
                (inhabitant-shallowly-chaperone-of? v b))))
          #:on-meet-knowable-promise-maybe-knowable-promise
          (on-smoosh-result-knowable-promise-maybe-knowable-promise
            (fn v
              (and
                (or (eq? v a) (eq? v b))
                (inhabitant-shallowly-chaperone-of? a v)
                (inhabitant-shallowly-chaperone-of? b v))))
          #:on-==-knowable-promise-maybe-knowable-promise
          (on-smoosh-result-knowable-promise-maybe-knowable-promise
            ==-acceptable-result?)
          #:on-path-related-knowable-promise-maybe-knowable-promise
          (on-smoosh-result-knowable-promise-maybe-knowable-promise
            path-related-acceptable-result?))
        (smoosh-and-comparison-of-two-reports-map report-2+
          #:on-check-result-knowable-promise
          (on-check-result-knowable-promise #t #t)
          #:on-smoosh-result-knowable-promise-maybe-knowable-promise
          (on-smoosh-result-knowable-promise-maybe-knowable-promise
            ==-acceptable-result?))))
    
    ))

(define/own-contract
  (make-expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type-impl-from-list-injection
    #:self-get-any-dynamic-type self-get-any-dynamic-type
    #:inhabitant? inhabitant?
    #:->list [->list #f]
    
    #:->->list
    [ ->->list
      (if ->list
        (dissectfn _ ->list)
        (raise-arguments-error 'make-expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type-impl-from-list-injection
          "expected either #:->list or #:->->list to be provided"))]
    
    #:combine-element-hash-codes
    [ combine-element-hash-codes
      (fn element-hash-codes
        (hash-code-combine* element-hash-codes))]
    
    )
  (->*
    (
      #:self-get-any-dynamic-type (-> any/c any/c)
      #:inhabitant? (-> any/c boolean?))
    (
      #:->list (or/c #f (-> any/c list?))
      #:->->list (-> any/c (-> any/c list?))
      #:combine-element-hash-codes (-> (listof fixnum?) fixnum?))
    expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type-impl?)
  (make-expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type-impl
    
    #:get-smoosh-equal-hash-code-support-reports
    (fn self a
      (constant-smoosh-equal-hash-code-support-reports /delay
        (expect (inhabitant? a) #t (uninformative-hash-code)
        /w- any-dt (self-get-any-dynamic-type self)
        /w- ->list (->->list a)
        /w- a-list (->list a)
        /smoosh-equal-hash-code-support-reports-zip*-map
          (list-map a-list /fn a-elem
            (dynamic-type-get-smoosh-equal-hash-code-support-reports
              any-dt a-elem))
          #:on-hash-code-promise
          (fn p-list
            (promise-zip*-map p-list /fn hash-code-list
              (hash-code-combine
                (equal-always-hash-code inhabitant?)
                (equal-always-hash-code/recur a /fn a-elem
                  (uninformative-hash-code))
                (combine-element-hash-codes hash-code-list)))))))
    
    ))

; Here are some rough notes for how this works when
; `#:ignore-chaperones?` is false, `#:known-distinct?` is true, and
; `#:known-discrete?` is false (their default values):
;
; Level 0:
;   path-related:
;     equal-always indistinct trie: Two collections with different
;     sets of keys cannot be added at once because their
;     path-relatedness is unknown.
;   ==:
;     Same as level 1 path-related.
; Level 1:
;   path-related:
;     equal-always distinct trie: Two collections with different sets
;     of keys can both be added and begin two independent subtries.
;   ==:
;     chaperone= distinct trie: Two collections with different sets of
;     keys begin two subtries again. Two with the same set of keys may
;     begin the same subtrie, but only if they're shallowly
;     chaperone=. To be shallowly chaperone=, we check that either
;     they're both chaperoneless (by making copies and checking that
;     the copies are `chaperone-of?` the originals) or they're
;     `chaperone-of?` in both directions. A key that has been inserted
;     into the trie has already had a copy made of it once, so we
;     don't need to repeat that work when comparing it to other tries.
; Level 2+:
;   path-related, ==:
;     Same as level 1 ==.
;
(define/own-contract
  (make-expressly-custom-gloss-key-dynamic-type-impl-from-list-injection
    #:ignore-chaperones? [ignore-chaperones? #f]
    #:known-distinct? [known-distinct? #t]
    #:known-discrete? [known-discrete? #f]
    #:inhabitant? inhabitant?
    #:->list [->list #f]
    
    #:->->list
    [ ->->list
      (if ->list
        (dissectfn _ ->list)
        (raise-arguments-error 'make-expressly-custom-gloss-key-dynamic-type-impl-from-list-injection
          "expected either #:->list or #:->->list to be provided"))]
    
    #:copy copy)
  (->*
    (
      #:inhabitant? (-> any/c boolean?)
      #:copy (-> any/c any/c))
    (
      #:ignore-chaperones? boolean?
      #:known-distinct? boolean?
      #:known-discrete? boolean?
      #:->list (or/c #f (-> any/c list?))
      #:->->list (-> any/c (-> any/c list?)))
    expressly-custom-gloss-key-dynamic-type-impl?)
  (make-expressly-custom-gloss-key-dynamic-type-impl
    
    #:get-custom-gloss-key-reports
    (fn self a
      (expect (inhabitant? a) #t
        (uninformative-custom-gloss-key-reports)
      /w- equal-always-indistinct-tgs-k
        (known /tagged-glossesque-sys
          inhabitant?
          (equal-always-indistinct-from-list-injection-glossesque-sys
            #:->list ->list
            #:->->list ->->list))
      /w- equal-always-distinct-tgs-k
        (known /tagged-glossesque-sys
          inhabitant?
          (equal-always-from-list-injection-glossesque-sys
            #:->list ->list
            #:->->list ->->list))
      /w- equal-always-tgs-k
        (if known-distinct?
          equal-always-distinct-tgs-k
          equal-always-indistinct-tgs-k)
      /w- chaperone=-indistinct-tgs-k
        (known /tagged-glossesque-sys
          inhabitant?
          (chaperone=-indistinct-copiable-glossesque-sys #:copy copy))
      /w- chaperone=-distinct-tgs-k
        (known /tagged-glossesque-sys
          inhabitant?
          (chaperone=-copiable-glossesque-sys #:copy copy))
      /w- chaperone=-tgs-k
        (if known-distinct?
          chaperone=-distinct-tgs-k
          chaperone=-indistinct-tgs-k)
      /w- possibly-chaperone=-tgs-k
        (if ignore-chaperones?
          equal-always-tgs-k
          chaperone=-tgs-k)
      /stream*
        (custom-gloss-key-report-zip*-map (list)
          #:on-path-related-glossesque-sys-knowable
          (dissectfn (list)
            (if (and known-distinct? known-discrete?)
              equal-always-distinct-tgs-k
              equal-always-indistinct-tgs-k))
          #:on-==-tagged-glossesque-sys-knowable
          (dissectfn (list)
            equal-always-tgs-k))
        (custom-gloss-key-report-zip*-map (list)
          #:on-path-related-glossesque-sys-knowable
          (dissectfn (list)
            equal-always-tgs-k)
          #:on-==-tagged-glossesque-sys-knowable
          (dissectfn (list)
            possibly-chaperone=-tgs-k))
        (constant-custom-gloss-key-reports
          #:tagged-glossesque-sys-knowable
          possibly-chaperone=-tgs-k)))
    
    ))

(define/own-contract
  (make-expressly-smooshable-bundle-property-from-list-isomorphism
    #:ignore-chaperones? [ignore-chaperones? #f]
    #:known-distinct? [known-distinct? #t]
    #:known-discrete? [known-discrete? #f]
    #:self-get-any-dynamic-type self-get-any-dynamic-type
    #:inhabitant? inhabitant?
    #:->list [->list #f]
    
    #:->->list
    [ ->->list
      (if ->list
        (dissectfn _ ->list)
        (raise-arguments-error 'make-expressly-smooshable-bundle-property-from-list-isomorphism
          "expected either #:->list or #:->->list to be provided"))]
    
    #:example-and-list-> example-and-list->
    
    #:combine-element-hash-codes
    [ combine-element-hash-codes
      (fn element-hash-codes
        (hash-code-combine* element-hash-codes))]
    
    #:inhabitant-shallowly-equal-always?-knowable
    [ inhabitant-shallowly-equal-always?-knowable
      (fn a b /known /equal-always?/recur a b /fn a-elem b-elem #t)]
    
    #:copy [copy (fn v /example-and-list-> v /(->->list v) v)]
    
    #:get-smoosh-of-zero-reports
    [ get-smoosh-of-zero-reports
      (fn self
        (uninformative-smoosh-reports))])
  (->*
    (
      #:self-get-any-dynamic-type (-> any/c any/c)
      #:inhabitant? (-> any/c boolean?)
      #:example-and-list-> (-> any/c list? any/c))
    (
      #:ignore-chaperones? boolean?
      #:known-distinct? boolean?
      #:known-discrete? boolean?
      #:->list (or/c #f (-> any/c list?))
      #:->->list (-> any/c (-> any/c list?))
      #:combine-element-hash-codes (-> (listof fixnum?) fixnum?)
      
      #:inhabitant-shallowly-equal-always?-knowable
      (-> any/c any/c (knowable/c boolean?))
      
      #:copy (-> any/c any/c)
      
      #:get-smoosh-of-zero-reports
      (-> any/c (sequence/c smoosh-report?))
      
      )
    (struct-type-property/c trivial?))
  (define-values (prop:bundle bundle? bundle-ref)
    (make-struct-type-property
      'expressly-smooshable-bundle-property-from-list-isomorphism
      (fn value info
        (expect value (trivial)
          (raise-arguments-error 'make-expressly-smooshable-bundle-property-from-list-isomorphism
            "expected the property value to be a trivial? value"
            "value" value)
          value))
      (list
        (cons
          prop:expressly-smooshable-dynamic-type
          (dissectfn (trivial)
            (if ignore-chaperones?
              (make-expressly-smooshable-dynamic-type-impl-from-equal-always-list-isomorphism
                #:known-distinct? known-distinct?
                #:known-discrete? known-discrete?
                #:self-get-any-dynamic-type self-get-any-dynamic-type
                #:inhabitant? inhabitant?
                #:->list ->list
                #:->->list ->->list
                #:example-and-list-> example-and-list->
                
                #:inhabitant-shallowly-equal-always?-knowable
                inhabitant-shallowly-equal-always?-knowable
                
                #:get-smoosh-of-zero-reports get-smoosh-of-zero-reports)
              (make-expressly-smooshable-dynamic-type-impl-from-chaperone-of-list-isomorphism
                #:known-distinct? known-distinct?
                #:known-discrete? known-discrete?
                #:self-get-any-dynamic-type self-get-any-dynamic-type
                #:inhabitant? inhabitant?
                #:->list ->list
                #:->->list ->->list
                #:example-and-list-> example-and-list->
                
                #:inhabitant-shallowly-equal-always?-knowable
                inhabitant-shallowly-equal-always?-knowable
                
                #:copy copy
                #:get-smoosh-of-zero-reports get-smoosh-of-zero-reports))))
        (cons
          prop:expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type
          (dissectfn (trivial)
            (make-expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type-impl-from-list-injection
              #:self-get-any-dynamic-type self-get-any-dynamic-type
              #:inhabitant? inhabitant?
              #:->list ->list
              #:->->list ->->list
              #:combine-element-hash-codes combine-element-hash-codes)))
        (cons
          prop:expressly-custom-gloss-key-dynamic-type
          (dissectfn (trivial)
            (make-expressly-custom-gloss-key-dynamic-type-impl-from-list-injection
              #:ignore-chaperones? ignore-chaperones?
              #:known-distinct? known-distinct?
              #:known-discrete? known-discrete?
              #:inhabitant? inhabitant?
              #:->list ->list
              #:->->list ->->list
              #:copy copy))))))
  prop:bundle)

; This is an appropriate `prop:expressly-smooshable-dynamic-type`
; implementation for simple values that can be compared by a simple
; equivalence comparison function.
;
; The given `known-distinct?` indicates whether a false result of
; the underlying comparison means we positively know the values are
; distinct (rather than just not knowing that they're equal). The
; given `known-discrete?` indicates whether knowing that two values
; are distinct means we positively know that they aren't related by
; ordering either. (Otherwise, their results for those failed ordered
; comparisons are unknown.) These situations only apply for failures
; of shallow comparison; a known failure of comparison of elements
; still results in a known failure of comparison overall.
;
; The given `==?` function (usually `equal-always?`) should be an
; equivalence comparison at least as strong as `equal-always?`, in the
; sense that when `==?` is true, `equal-always?` is true.
;
; Level 0:
;   <=, >=, path-related, join, meet:
;     If the operands do not both have known results for the given
;     `inhabitant?` predicate, which may be an instance of
;     `prop:expressly-knowable-predicate?`, or if neither of them has
;     a known true result for it, then unknown.
;     
;     Otherwise, if at least one operand has a known true result for
;     `inhabitant?` and at least one has a known false result for it,
;     then a known nothing (or, for a check, `#f`).
;     
;     Otherwise, if the operands pass the given `==?` function, the
;     first operand (or, for a check, `#t`).
;     
;     Otherwise, if (`known-distinct?` and `known-discrete?`) is true,
;     a known nothing (or, for a check, `#f`).
;     
;     Otherwise, unknown.
;   ==:
;     If the operands do not both have known results for the given
;     `inhabitant?` predicate, which may be an instance of
;     `prop:expressly-knowable-predicate?`, or if neither of them has
;     a known true result for it, then unknown.
;     
;     Otherwise, if at least one operand has a known true result for
;     `inhabitant?` and at least one has a known false result for it,
;     then a known nothing.
;     
;     Otherwise, if the operands pass the given `==?` function, the
;     first operand.
;     
;     Otherwise, if `known-distinct?` is true, a known nothing.
;     
;     Otherwise, unknown.
; Level 1+:
;   path-related, join, meet, ==:
;     Same as the description of level 0 ==.
;   <=, >=:
;     Same as the description of level 0 == as a check.
;
(define/own-contract
  (make-expressly-smooshable-dynamic-type-impl-for-equal-always-atom
    #:known-distinct? [known-distinct? #t]
    #:known-discrete? [known-discrete? #f]
    #:inhabitant? inhabitant?
    #:==? [==? (fn a b /equal-always? a b)])
  (->*
    (#:inhabitant? (-> any/c boolean?))
    (
      #:known-distinct? boolean?
      #:known-discrete? boolean?
      #:==? (-> any/c any/c boolean?))
    expressly-smooshable-dynamic-type-impl?)
  (make-expressly-smooshable-dynamic-type-impl
    
    #:get-smoosh-of-zero-reports
    (fn self
      (uninformative-smoosh-reports))
    
    #:get-smoosh-of-one-reports
    (fn self a
      (constant-smoosh-reports /delay
        (knowable-if (inhabitant? a) /fn
          (just /delay/strict /known a))))
    
    #:get-smoosh-and-comparison-of-two-reports
    (fn self a b
      (expect (call-knowable inhabitant? a) (known a-inhabits?)
        (uninformative-smoosh-and-comparison-of-two-reports)
      /expect (call-knowable inhabitant? b) (known b-inhabits?)
        (uninformative-smoosh-and-comparison-of-two-reports)
      /expect (or a-inhabits? b-inhabits?) #t
        (uninformative-smoosh-and-comparison-of-two-reports)
      /expect (and a-inhabits? b-inhabits?) #t
        (smoosh-and-comparison-of-two-reports-with-hesitation-at-discrepancies
          (false-smoosh-and-comparison-of-two-reports))
      /smoosh-and-comparison-of-two-reports-with-hesitation-at-discrepancies
        #:known-distinct? known-distinct?
        #:known-discrete? known-discrete?
        (constant-smoosh-and-comparison-of-two-reports /delay
          (known /maybe-if (==? a b) /fn /delay/strict /known a))))
    
    ))

; This is an appropriate `prop:expressly-smooshable-dynamic-type`
; implementation for mutable tuple data structures and their
; chaperones, information-ordered in a way that's consistent with
; `chaperone-of?`.
;
; The given `known-distinct?` indicates whether a false result of
; the underlying comparison means we positively know the values are
; distinct (rather than just not knowing that they're equal). The
; given `known-discrete?` indicates whether knowing that two values
; are distinct means we positively know that they aren't related by
; ordering either. (Otherwise, their results for those failed ordered
; comparisons are unknown.)
;
; Level 0:
;   <=, >=, path-related, join, meet:
;     Same as the description of level 1 path-related (as a check when
;     applicable), except that if `known-discrete?` is false, a result
;     that would be a known nothing is instead unknown.
;   ==:
;     Same as the description of level 1 path-related.
; Level 1:
;   path-related, join, meet, ==:
;     If the operands do not both have known results for the given
;     `inhabitant?` predicate, which may be an instance of
;     `prop:expressly-knowable-predicate?`, or if neither of them has
;     a known true result for it, then unknown.
;     
;     Otherwise, if at least one operand has a known true result for
;     `inhabitant?` and at least one has a known false result for it,
;     then a known nothing.
;     
;     Otherwise, if the operands are not `equal-always?` and
;     `known-distinct?` is true, then a known nothing.
;     
;     Otherwise, if the operands are not `equal-always?`, then
;     unknown.
;     
;     Otherwise, if any operand counts as an acceptable result, then
;     the first such operand.
;     
;     Otherwise, unknown.
;     
;     Where "acceptable result" means:
;       If we're doing path-related:
;         Every result is acceptable.
;       If we're doing join:
;         Every result is acceptable if it's `chaperone-of?` every
;         operand.
;         
;         (We'll allow for the possibility that Racket will introduce
;         chaperone wrappers that are chaperone-of each of two
;         preexisting chaperones, so we'll treat some results as
;         unknown even though a more Racket-version-pinned design
;         might treat them as known nothings.)
;       If we're doing meet:
;         Every operand is acceptable if each of the operands is
;         `chaperone-of?` it.
;         
;         (Note that the meet is often an object the program can refer
;         to, but unless an acceptable result is found among the
;         operands, there's no way to obtain it from the operands, so
;         the result will be unknown.)
;       If we're doing ==:
;         Every result is acceptable if it and the operands are all
;         `chaperone-of?` each other.
;   <=, >=:
;     If the operands do not both have known results for the given
;     `inhabitant?` predicate, which may be an instance of
;     `prop:expressly-knowable-predicate?`, or if neither of them has
;     a known true result for it, then unknown.
;     
;     Otherwise, if at least one operand has a known true result for
;     `inhabitant?` and at least one has a known false result for it,
;     then a known `#f`.
;     
;     Otherwise, if the operands are not `equal-always?` and
;     `known-distinct?` is true, then a known `#f`.
;     
;     Otherwise, if the operands are not `equal-always?`, then
;     unknown.
;     
;     Otherwise, if the element we're proposing to be greater is
;     `chaperone-of?` the other one, then a known `#t`.
;     
;     Otherwise, unknown.
;     
;     (We'll allow for the possibility that values which differ only
;     in the failure of a chaperone-of check could actually pass the
;     chaperone-of check in the future when they're written to reuse
;     chaperone-wrapped values more diligently. This kind of rewrite
;     may be more feasible if and when Racket introduces
;     chaperone-wrapping operations that create equivalent wrappers in
;     some reliable way. As such, we'll treat some results as unknown
;     even though a more Racket-version-pinned design might treat them
;     as known nothings.)
; Level 2+:
;   path-related, join, meet, ==:
;     Same as the description of level 1 ==.
;   <=, >=:
;     Same as the description of level 1 == as a check.
;
(define/own-contract
  (make-expressly-smooshable-dynamic-type-impl-for-chaperone-of-atom
    #:known-distinct? [known-distinct? #t]
    #:known-discrete? [known-discrete? #f]
    #:inhabitant? inhabitant?)
  (->*
    (#:inhabitant? (-> any/c boolean?))
    (#:known-distinct? boolean? #:known-discrete? boolean?)
    expressly-smooshable-dynamic-type-impl?)
  (make-expressly-smooshable-dynamic-type-impl
    
    #:get-smoosh-of-zero-reports
    (fn self
      (uninformative-smoosh-reports))
    
    #:get-smoosh-of-one-reports
    (fn self a
      (constant-smoosh-reports /delay
        (knowable-if (inhabitant? a) /fn
          (just /delay/strict /known a))))
    
    #:get-smoosh-and-comparison-of-two-reports
    (fn self a b
      (expect (call-knowable inhabitant? a) (known a-inhabits?)
        (uninformative-smoosh-and-comparison-of-two-reports)
      /expect (call-knowable inhabitant? b) (known b-inhabits?)
        (uninformative-smoosh-and-comparison-of-two-reports)
      /expect (or a-inhabits? b-inhabits?) #t
        (uninformative-smoosh-and-comparison-of-two-reports)
      /expect (and a-inhabits? b-inhabits?) #t
        (smoosh-and-comparison-of-two-reports-with-hesitation-at-discrepancies
          (false-smoosh-and-comparison-of-two-reports))
      /if (not /equal-always? a b)
        (smoosh-and-comparison-of-two-reports-with-hesitation-at-discrepancies
          #:known-distinct? known-distinct?
          #:known-discrete? known-discrete?
          (false-smoosh-and-comparison-of-two-reports))
      /w- a-chaperone-of-b?-promise (delay /chaperone-of? a b)
      /w- b-chaperone-of-a?-promise (delay /chaperone-of? b a)
      ; Given two `equal-always?` inhabitants, this checks whether
      ; they're `chaperone-of?`. Like `chaperone-of?`, this takes
      ; constant time if the inhabitants are `eq?` themselves.
      /w- inhabitant-chaperone-of?
        (fn s t
          (or (eq? s t)
          /if (and (eq? s b) (eq? t a))
            (force b-chaperone-of-a?-promise)
          /if (and (eq? s a) (eq? t b))
            (force a-chaperone-of-b?-promise)
          /chaperone-of? s t))
      /w- on-check-result-knowable-promise
        (fn should-a-be-small? should-b-be-small?
          (dissectfn (list)
            (delay
              (boolean-and-knowable-thunk-zip* /list
                (fn /boolean-or-knowable-thunk-zip* /list
                  (fn /known /not should-a-be-small?)
                  (fn /falsable->uninformative-knowable
                    (inhabitant-chaperone-of? b a)))
                (fn /boolean-or-knowable-thunk-zip* /list
                  (fn /known /not should-b-be-small?)
                  (fn /falsable->uninformative-knowable
                    (inhabitant-chaperone-of? a b)))))))
      /w- on-smoosh-result-knowable-promise-maybe-knowable-promise
        (fn acceptable-result?
          (dissectfn (list)
            (delay/strict /known /just /delay
              (if (acceptable-result? a)
                (known a)
              /if (acceptable-result? b)
                (known b)
              /unknown))))
      /w- chaperone=?-promise
        (delay
          (and
            (inhabitant-chaperone-of? b a)
            (inhabitant-chaperone-of? a b)))
      /w- ==-acceptable-result?
        (fn v
          (force chaperone=?-promise))
      /w- path-related-acceptable-result?
        (fn v
          #t)
      /stream*
        (smoosh-and-comparison-of-two-report-zip*-map (list)
          #:on-check-result-knowable-promise
          (on-check-result-knowable-promise #f #f)
          #:on-smoosh-result-knowable-promise-maybe-knowable-promise
          (on-smoosh-result-knowable-promise-maybe-knowable-promise
            path-related-acceptable-result?))
        (smoosh-and-comparison-of-two-report-zip*-map (list)
          #:on-<=?-knowable-promise
          (on-check-result-knowable-promise #t #f)
          #:on->=?-knowable-promise
          (on-check-result-knowable-promise #f #t)
          #:on-join-knowable-promise-maybe-knowable-promise
          (on-smoosh-result-knowable-promise-maybe-knowable-promise
            (fn v
              (and
                (inhabitant-chaperone-of? v a)
                (inhabitant-chaperone-of? v b))))
          #:on-meet-knowable-promise-maybe-knowable-promise
          (on-smoosh-result-knowable-promise-maybe-knowable-promise
            (fn v
              (and
                (inhabitant-chaperone-of? a v)
                (inhabitant-chaperone-of? b v))))
          #:on-==-knowable-promise-maybe-knowable-promise
          (on-smoosh-result-knowable-promise-maybe-knowable-promise
            ==-acceptable-result?)
          #:on-path-related-knowable-promise-maybe-knowable-promise
          (on-smoosh-result-knowable-promise-maybe-knowable-promise
            path-related-acceptable-result?))
        (smoosh-and-comparison-of-two-reports-zip*-map (list)
          #:on-check-result-knowable-promise
          (on-check-result-knowable-promise #t #t)
          #:on-smoosh-result-knowable-promise-maybe-knowable-promise
          (on-smoosh-result-knowable-promise-maybe-knowable-promise
            ==-acceptable-result?))))
    
    ))

(define/own-contract
  (make-expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type-impl-for-atom
    #:hash-code [hash-code (fn a /equal-always-hash-code a)]
    #:hash-code-0 [hash-code-0 hash-code]
    #:hash-code-1+ [hash-code-1+ hash-code])
  (->*
    ()
    (
      #:hash-code (-> any/c fixnum?)
      #:hash-code-0 (-> any/c fixnum?)
      #:hash-code-1+ (-> any/c fixnum?))
    expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type-impl?)
  (make-expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type-impl
    
    #:get-smoosh-equal-hash-code-support-reports
    (fn self a
      (stream*
        (constant-smoosh-equal-hash-code-support-report
          (delay /hash-code-0 a))
        (constant-smoosh-equal-hash-code-support-reports
          (delay /hash-code-1+ a))))
    
    ))

(define/own-contract
  (make-expressly-custom-gloss-key-dynamic-type-impl-for-atom
    #:eq-matters? [eq-matters? #f]
    #:ignore-chaperones? [ignore-chaperones? eq-matters?]
    #:known-distinct? [known-distinct? #t]
    #:known-discrete? [known-discrete? #f]
    #:inhabitant? inhabitant?)
  (->*
    (#:inhabitant? (-> any/c boolean?))
    (
      #:ignore-chaperones? boolean?
      #:known-distinct? boolean?
      #:known-discrete? boolean?)
    expressly-custom-gloss-key-dynamic-type-impl?)
  (make-expressly-custom-gloss-key-dynamic-type-impl
    
    #:get-custom-gloss-key-reports
    (fn self a
      (expect (inhabitant? a) #t
        (uninformative-custom-gloss-key-reports)
      /w- distinct-0-tgs-k
        (if eq-matters?
          (known /tagged-glossesque-sys
            ; TODO SMOOSH: See if we can encompass a wider range of
            ; inhabitants with this, as an optimization.
            inhabitant?
            (eq-atom-glossesque-sys))
          (known /tagged-glossesque-sys
            inhabitant?
            (equal-always-atom-glossesque-sys)))
      /w- distinct-1+-tgs-k
        (if (or eq-matters? ignore-chaperones?)
          distinct-0-tgs-k
          (known /tagged-glossesque-sys
            inhabitant?
            (chaperone=-atom-glossesque-sys)))
      /w- indistinct-0-tgs-k
        (known /tagged-glossesque-sys
          inhabitant?
          (if eq-matters?
            (eq-indistinct-atom-glossesque-sys)
            (equal-always-indistinct-atom-glossesque-sys)))
      /w- indistinct-1+-tgs-k
        (if (or eq-matters? ignore-chaperones?)
          indistinct-0-tgs-k
          (known /tagged-glossesque-sys
            inhabitant?
            (chaperone=-indistinct-atom-glossesque-sys)))
      /if (and known-distinct? known-discrete?)
        (stream*
          (constant-custom-gloss-key-report
            #:tagged-glossesque-sys-knowable distinct-0-tgs-k)
          (constant-custom-gloss-key-reports
            #:tagged-glossesque-sys-knowable distinct-1+-tgs-k))
      /if (not known-distinct?)
        (stream*
          (constant-custom-gloss-key-report
            #:tagged-glossesque-sys-knowable indistinct-0-tgs-k)
          (constant-custom-gloss-key-reports
            #:tagged-glossesque-sys-knowable indistinct-1+-tgs-k))
      /stream*
        (custom-gloss-key-report-map
          (constant-custom-gloss-key-report
            #:tagged-glossesque-sys-knowable indistinct-0-tgs-k)
          #:on-==-tagged-glossesque-sys-knowable
          (fn tgs-k
            distinct-0-tgs-k))
        (constant-custom-gloss-key-reports
          #:tagged-glossesque-sys-knowable distinct-1+-tgs-k)))
    
    ))

(define/own-contract
  (make-expressly-smooshable-bundle-property-for-atom
    #:eq-matters? [eq-matters? #f]
    #:ignore-chaperones? [ignore-chaperones? eq-matters?]
    #:known-distinct? [known-distinct? #t]
    #:known-discrete? [known-discrete? #f]
    #:inhabitant? inhabitant?
    
    #:==?
    [ ==?
      (if eq-matters?
        (fn a b /eq? a b)
        (fn a b /equal-always? a b))]
    
    #:hash-code
    [ hash-code
      (if eq-matters?
        (fn a /eq-hash-code a)
        (fn a /equal-always-hash-code a))]
    
    #:hash-code-0 [hash-code-0 hash-code]
    #:hash-code-1+ [hash-code-1+ hash-code])
  (->*
    (#:inhabitant? (-> any/c boolean?))
    (
      #:eq-matters? boolean?
      #:ignore-chaperones? boolean?
      #:known-distinct? boolean?
      #:known-discrete? boolean?
      #:==? (-> any/c any/c boolean?)
      #:hash-code (-> any/c fixnum?)
      #:hash-code-0 (-> any/c fixnum?)
      #:hash-code-1+ (-> any/c fixnum?))
    (struct-type-property/c trivial?))
  (define-values (prop:bundle bundle? bundle-ref)
    (make-struct-type-property
      'expressly-smooshable-bundle-property-for-atom
      (fn value info
        (expect value (trivial)
          (raise-arguments-error 'make-expressly-smooshable-bundle-property-for-atom
            "expected the property value to be a trivial? value"
            "value" value)
          value))
      (list
        (cons
          prop:expressly-smooshable-dynamic-type
          (dissectfn (trivial)
            (if ignore-chaperones?
              (make-expressly-smooshable-dynamic-type-impl-for-equal-always-atom
                #:known-distinct? known-distinct?
                #:known-discrete? known-discrete?
                #:inhabitant? inhabitant?
                #:==? ==?)
              (make-expressly-smooshable-dynamic-type-impl-for-chaperone-of-atom
                #:known-distinct? known-distinct?
                #:known-discrete? known-discrete?
                #:inhabitant? inhabitant?))))
        (cons
          prop:expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type
          (dissectfn (trivial)
            (make-expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type-impl-for-atom
              #:hash-code-0 hash-code-0
              #:hash-code-1+ hash-code-1+)))
        (cons
          prop:expressly-custom-gloss-key-dynamic-type
          (dissectfn (trivial)
            (make-expressly-custom-gloss-key-dynamic-type-impl-for-atom
              #:eq-matters? eq-matters?
              #:ignore-chaperones? ignore-chaperones?
              #:known-distinct? known-distinct?
              #:known-discrete? known-discrete?
              #:inhabitant? inhabitant?))))))
  prop:bundle)

; Level 0:
;   <=, >=, path-related, join, meet:
;     If the operands are not both `base-readable?` values, or if
;     neither of them is an `flvector?` value, then unknown.
;     
;     Otherwise, if at least one operand is an `flvector?` value and
;     at least one isn't, then a known nothing (or, for a check,
;     `#f`).
;     
;     Otherwise, if the operands are `eq?`, then the first operand
;     (or, for a check, `#t`).
;     
;     Otherwise, unknown.
;   ==:
;     If the operands are not both `base-readable?` values, or if
;     neither of them is an `flvector?` value, then unknown.
;     
;     Otherwise, if at least one operand is an `flvector?` value and
;     at least one isn't, then a known nothing.
;     
;     Otherwise, if the operands are `eq?`, then the first operand.
;     
;     Otherwise, a known nothing.
; Level 1+:
;   path-related, join, meet, ==:
;     Same as the description of level 0 ==.
;   <=, >=:
;     Same as the description of level 0 == as a check.
;
(define-imitation-simple-struct (flvector-dynamic-type?)
  flvector-dynamic-type
  'flvector-dynamic-type (current-inspector) (auto-write)
  (#:prop
    (make-expressly-smooshable-bundle-property-for-atom
      #:eq-matters? #t
      
      #:inhabitant?
      (makeshift-knowable-predicate /fn v
        ; TODO FORWARD: This use of `base-readable?` is a forward
        ; reference. See if we can untangle it.
        (knowable-if (base-readable? v) /fn /flvector? v))
      
      )
    (trivial)))

; Level 0:
;   <=, >=, path-related, join, meet:
;     If the operands are not both `base-readable?` values, or if
;     neither of them is an `fxvector?` value, then unknown.
;     
;     Otherwise, if at least one operand is an `fxvector?` value and
;     at least one isn't, then a known nothing (or, for a check,
;     `#f`).
;     
;     Otherwise, if the operands are `eq?`, then the first operand
;     (or, for a check, `#t`).
;     
;     Otherwise, unknown.
;   ==:
;     If the operands are not both `base-readable?` values, or if
;     neither of them is an `fxvector?` value, then unknown.
;     
;     Otherwise, if at least one operand is an `fxvector?` value and
;     at least one isn't, then a known nothing.
;     
;     Otherwise, if the operands are `eq?`, then the first operand.
;     
;     Otherwise, a known nothing.
; Level 1+:
;   path-related, join, meet, ==:
;     Same as the description of level 0 ==.
;   <=, >=:
;     Same as the description of level 0 == as a check.
;
(define-imitation-simple-struct (fxvector-dynamic-type?)
  fxvector-dynamic-type
  'fxvector-dynamic-type (current-inspector) (auto-write)
  (#:prop
    (make-expressly-smooshable-bundle-property-for-atom
      #:eq-matters? #t
      
      #:inhabitant?
      (makeshift-knowable-predicate /fn v
        ; TODO FORWARD: This use of `base-readable?` is a forward
        ; reference. See if we can untangle it.
        (knowable-if (base-readable? v) /fn /fxvector? v))
      
      )
    (trivial)))

(define/own-contract (base-syntactic-atom? v)
  (-> any/c boolean?)
  (or (symbol? v) (keyword? v) (null? v)))

; Level 0:
;   <=, >=, path-related, join, meet:
;     If the operands are not both `base-readable?` values, or if
;     neither of them is a `base-syntactic-atom?` value, then
;     unknown.
;     
;     Otherwise, if at least one operand is a `base-syntactic-atom?`
;     value and at least one isn't, then a known nothing (or, for a
;     check, `#f`).
;     
;     If the operands are `equal-always?`, then the first operand (or,
;     for a check, `#t`).
;     
;     Otherwise, unknown.
;   ==:
;     If the operands are not both `base-readable?` values, or if
;     neither of them is a `base-syntactic-atom?` value, then
;     unknown.
;     
;     Otherwise, if at least one operand is a `base-syntactic-atom?`
;     value and at least one isn't, then a known nothing.
;     
;     If the operands are `equal-always?`, then the first operand.
;     
;     Otherwise, a known nothing.
; Level 1+:
;   path-related, join, meet, ==:
;     Same as the description of level 0 ==.
;   <=, >=:
;     Same as the description of level 0 == as a check.
;
(define-imitation-simple-struct (base-syntactic-atom-dynamic-type?)
  base-syntactic-atom-dynamic-type
  'base-syntactic-atom-dynamic-type (current-inspector) (auto-write)
  
  (#:prop
    (make-expressly-smooshable-bundle-property-for-atom
      #:ignore-chaperones? #t
      
      #:inhabitant?
      (makeshift-knowable-predicate /fn v
        ; TODO FORWARD: This use of `base-readable?` is a forward
        ; reference. See if we can untangle it.
        (knowable-if (base-readable? v) /fn /base-syntactic-atom? v))
      
      )
    (trivial)))

; Level 0:
;   <=, >=, path-related, join, meet:
;     If the operands are not both `boolean?` values, then unknown.
;     
;     If the operands are `equal-always?`, then the first operand (or,
;     for a check, `#t`).
;     
;     Otherwise, unknown.
;   ==:
;     If the operands are not both `boolean?` values, then unknown.
;     
;     If the operands are `equal-always?`, then the first operand.
;     
;     Otherwise, a known nothing.
; Level 1+:
;   path-related, join, meet, ==:
;     Same as the description of level 0 ==.
;   <=, >=:
;     Same as the description of level 0 == as a check.
;
(define-imitation-simple-struct (boolean-dynamic-type?)
  boolean-dynamic-type
  'boolean-dynamic-type (current-inspector) (auto-write)
  
  (#:prop
    (make-expressly-smooshable-bundle-property-for-atom
      #:ignore-chaperones? #t
      #:inhabitant? boolean?)
    (trivial)))

; Level 0+:
;   <=, >=, path-related, join, meet, ==:
;     If the operands are not both characters, then unknown.
;     
;     If the operands are `equal-always?`, then the first operand (or,
;     for a check, `#t`).
;     
;     Otherwise, unknown.
;
(define-imitation-simple-struct (char-dynamic-type?) char-dynamic-type
  'char-dynamic-type (current-inspector) (auto-write)
  
  (#:prop
    (make-expressly-smooshable-bundle-property-for-atom
      #:ignore-chaperones? #t
      #:known-distinct? #f
      #:inhabitant? char?)
    (trivial)))

; Level 0+:
;   <=, >=, path-related, join, meet, ==:
;     If the operands are not both immutable strings, then unknown.
;     
;     Otherwise, if the operands are `equal-always?`, then the first
;     operand (or, for a check, `#t`).
;     
;     Otherwise, unknown.
;
(define-imitation-simple-struct (immutable-string-dynamic-type?)
  immutable-string-dynamic-type
  'immutable-string-dynamic-type (current-inspector) (auto-write)
  
  (#:prop
    (make-expressly-smooshable-bundle-property-for-atom
      #:ignore-chaperones? #t
      #:known-distinct? #f
      #:inhabitant? immutable-string?)
    (trivial)))

; Level 0+:
;   <=, >=, path-related, join, meet, ==:
;     If the operands are not both immutable byte strings, then
;     unknown.
;     
;     Otherwise, if the operands are `equal-always?`, then the first
;     operand (or, for a check, `#t`).
;     
;     Otherwise, unknown.
;
(define-imitation-simple-struct (immutable-bytes-dynamic-type?)
  immutable-bytes-dynamic-type
  'immutable-bytes-dynamic-type (current-inspector) (auto-write)
  
  (#:prop
    (make-expressly-smooshable-bundle-property-for-atom
      #:ignore-chaperones? #t
      #:known-distinct? #f
      #:inhabitant? (fn v /and (bytes? v) (immutable? v)))
    (trivial)))

(define (normalize-non-nan-number a)
  (define (normalize-real a)
    (if (not /rational? a)
      ; If the real number to normalize is infinity or negative
      ; infinity, we return it unchanged.
      a
    /inexact->exact a))
  (make-rectangular
    (normalize-real /real-part a)
    (normalize-real /imag-part a)))

(define/own-contract (non-nan-number-glossesque-sys)
  (-> glossesque-sys?)
  (normalized-glossesque-sys /fn k /normalize-non-nan-number k))

(define/own-contract (nan-number? v)
  (-> any/c boolean?)
  (and
    (number? v)
    (or
      (nan? /real-part v)
      (nan? /imag-part v))))

(define/own-contract (non-nan-number? v)
  (-> any/c boolean?)
  (and (number? v) (not /nan-number? v)))

; Level 0:
;   path-related:
;     If the operands are not both `number?` values without NaN parts,
;     then unknown.
;     
;     Otherwise, if the operands are `=` or both have `imag-part`s `=`
;     to `0`, the first operand.
;     
;     Otherwise, unknown.
;   join (resp. meet):
;     If the operands are not both `number?` values without NaN parts,
;     then unknown.
;     
;     Otherwise, if the operands are `=`, the first operand.
;     
;     Otherwise, if the operands both have `imag-part`s `=` to `0`,
;     the one with the greater (resp. lesser) `real-part` according to
;     `<=`.
;     
;     Otherwise, unknown.
;   ==:
;     If the operands are not both `number?` values without NaN parts,
;     then unknown.
;     
;     Otherwise, if the operands are `=`, the first operand.
;     
;     Otherwise, a known nothing.
;   <= (resp. >=):
;     If the operands are not both `number?` values without NaN parts,
;     then unknown.
;     
;     Otherwise, if the operands are `=`, `#t`.
;     
;     Otherwise, if the operands both have `imag-part`s `=` to `0`,
;     the `<=` (resp. `>=`) result on their `real-part`s.
;     
;     Otherwise, unknown.
; Level 1+:
;   <=, >=, path-related, join, meet, ==:
;     If the operands are not both `number?` values without NaN parts,
;     then unknown.
;     
;     Otherwise, if the operands are `equal-always?`, the first
;     operand (or, for a check, `#t`).
;     
;     Otherwise, a known nothing (or, for a check, `#f`).
;
(define-imitation-simple-struct (non-nan-number-dynamic-type?)
  non-nan-number-dynamic-type
  'non-nan-number-dynamic-type (current-inspector) (auto-write)
  
  (#:prop prop:expressly-smooshable-dynamic-type
    (make-expressly-smooshable-dynamic-type-impl
      
      #:get-smoosh-of-zero-reports
      (fn self
        (uninformative-smoosh-reports))
      
      #:get-smoosh-of-one-reports
      (fn self a
        (expect (non-nan-number? a) #t (uninformative-smoosh-reports)
        /constant-smoosh-reports
          (delay/strict /known /just /delay/strict /known a)))
      
      #:get-smoosh-and-comparison-of-two-reports
      (fn self a b
        (expect (non-nan-number? a) #t
          (uninformative-smoosh-and-comparison-of-two-reports)
        /expect (non-nan-number? b) #t
          (uninformative-smoosh-and-comparison-of-two-reports)
        /w- report-1+
          (constant-smoosh-and-comparison-of-two-reports /delay /known
            (maybe-if (equal-always? a b) /fn /delay/strict /known a))
        /if (= a b)
          (stream*
            (constant-smoosh-and-comparison-of-two-report
              (delay/strict /known /just /delay/strict /known a))
            report-1+)
        /w- real?-promise
          (delay /and (zero? /imag-part a) (zero? /imag-part b))
        /w- <=?-knowable-promise
          (promise-map real?-promise /fn real?
            (knowable-if real? /fn /<= (real-part a) (real-part b)))
        /w- >=?-knowable-promise
          (promise-map real?-promise /fn real?
            (knowable-if real? /fn />= (real-part a) (real-part b)))
        /w- join-knowable-promise-maybe-knowable-promise
          (promise-map <=?-knowable-promise /fn knowable
            (knowable-map knowable /fn result
              (just /delay/strict /known /if result b a)))
        /w- meet-knowable-promise-maybe-knowable-promise
          (promise-map <=?-knowable-promise /fn knowable
            (knowable-map knowable /fn result
              (just /delay/strict /known /if result a b)))
        /stream*
          (smoosh-and-comparison-of-two-reports-zip*-map (list)
            #:on-<=?-knowable-promise
            (dissectfn (list)
              <=?-knowable-promise)
            #:on->=?-knowable-promise
            (dissectfn (list)
              >=?-knowable-promise)
            #:on-join-knowable-promise-maybe-knowable-promise
            (dissectfn (list)
              join-knowable-promise-maybe-knowable-promise)
            #:on-meet-knowable-promise-maybe-knowable-promise
            (dissectfn (list)
              meet-knowable-promise-maybe-knowable-promise)
            #:on-==-knowable-promise-maybe-knowable-promise
            (dissectfn (list)
              (delay/strict /known /nothing))
            #:on-path-related-knowable-promise-maybe-knowable-promise
            (dissectfn (list)
              (delay/strict /known /just /delay/strict /known a)))
          report-1+))
      
      ))
  
  (#:prop prop:expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type
    (make-expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type-impl-for-atom
      
      #:hash-code-0
      (fn a
        (expect (non-nan-number? a) #t (uninformative-hash-code)
        /equal-always-hash-code /normalize-non-nan-number a))
      
      #:hash-code-1+ (fn a /equal-always-hash-code a)))
  
  (#:prop prop:expressly-custom-gloss-key-dynamic-type
    (make-expressly-custom-gloss-key-dynamic-type-impl
      
      #:get-custom-gloss-key-reports
      (fn self a
        (expect (non-nan-number? a) #t
          (uninformative-custom-gloss-key-reports)
        /stream*
          (custom-gloss-key-report-zip*-map (list)
            #:on-==-tagged-glossesque-sys-knowable
            (dissectfn (list)
              (known /tagged-glossesque-sys
                non-nan-number?
                (non-nan-number-glossesque-sys)))
            #:on-path-related-tagged-glossesque-sys-knowable
            (dissectfn (list)
              (if (zero? /imag-part a)
                (known /tagged-glossesque-sys
                  (makeshift-knowable-predicate /fn v
                    (knowable-if (non-nan-number? v) /fn
                      (zero? /imag-part v)))
                  (terminal-glossesque-sys))
                (known /tagged-glossesque-sys
                  (makeshift-knowable-predicate /fn v
                    (knowable-if (non-nan-number? v) /fn /= a v))
                  (terminal-glossesque-sys)))))
          (constant-custom-gloss-key-reports
            #:tagged-glossesque-sys-knowable
            (known /tagged-glossesque-sys
              non-nan-number?
              (equal-always-atom-glossesque-sys)))))
      
      ))
  
  )

(define/own-contract (non-nan-extflonum? v)
  (-> any/c boolean?)
  (and (extflonum? v) (extfl= v v)))

(define/own-contract (nan-extflonum? v)
  (-> any/c boolean?)
  (and (extflonum? v) (not /non-nan-extflonum? v)))

; Level 0:
;   path-related:
;     If the operands are not both non-NaN `extflonum?` values, then
;     unknown.
;     
;     Otherwise, the first operand.
;   join (resp. meet):
;     If the operands are not both non-NaN `extflonum?` values, then
;     unknown.
;     
;     Otherwise, if the operands are `extfl=`, the first operand.
;     
;     Otherwise, the greater (resp. lesser) operand according to
;     `extfl<=`.
;   ==:
;     If the operands are not both non-NaN `extflonum?` values, then
;     unknown.
;     
;     Otherwise, if the operands are `extfl=`, the first operand.
;     
;     Otherwise, a known nothing.
;   <= (resp. >=):
;     If the operands are not both `extflonum?` values without NaN
;     parts, then unknown.
;     
;     Otherwise, the `extfl<=` (resp. `extfl>=`) result on the
;     operands.
; Level 1+:
;   <=, >=, path-related, join, meet, ==:
;     If the operands are not both non-NaN `extflonum?` values, then
;     unknown.
;     
;     Otherwise, if the operands are `equal-always?`, the first
;     operand (or, for a check, `#t`).
;     
;     Otherwise, a known nothing (or, for a check, `#f`).
;
(define-imitation-simple-struct (non-nan-extflonum-dynamic-type?)
  non-nan-extflonum-dynamic-type
  'non-nan-extflonum-dynamic-type (current-inspector) (auto-write)
  
  (#:prop prop:expressly-smooshable-dynamic-type
    (make-expressly-smooshable-dynamic-type-impl
      
      #:get-smoosh-of-zero-reports
      (fn self
        (uninformative-smoosh-reports))
      
      #:get-smoosh-of-one-reports
      (fn self a
        (expect (non-nan-extflonum? a) #t
          (uninformative-smoosh-reports)
        /constant-smoosh-reports
          (delay/strict /known /just /delay/strict /known a)))
      
      #:get-smoosh-and-comparison-of-two-reports
      (fn self a b
        (expect (non-nan-extflonum? a) #t
          (uninformative-smoosh-and-comparison-of-two-reports)
        /expect (non-nan-extflonum? b) #t
          (uninformative-smoosh-and-comparison-of-two-reports)
        /w- report-1+
          (constant-smoosh-and-comparison-of-two-reports /delay /known
            (maybe-if (equal-always? a b) /fn /delay/strict /known a))
        /if (extfl= a b)
          (stream*
            (constant-smoosh-and-comparison-of-two-report
              (delay/strict /known /just /delay/strict /known a))
            report-1+)
        /w- <=?-knowable-promise (delay /known /extfl<= a b)
        /w- >=?-knowable-promise (delay /known /extfl>= a b)
        /w- join-knowable-promise-maybe-knowable-promise
          (promise-map <=?-knowable-promise /fn knowable
            (knowable-map knowable /fn <=?
              (just /delay/strict /known /if <=? b a)))
        /w- meet-knowable-promise-maybe-knowable-promise
          (promise-map <=?-knowable-promise /fn knowable
            (knowable-map knowable /fn <=?
              (just /delay/strict /known /if <=? a b)))
        /stream*
          (smoosh-and-comparison-of-two-reports-zip*-map (list)
            #:on-<=?-knowable-promise
            (dissectfn (list)
              <=?-knowable-promise)
            #:on->=?-knowable-promise
            (dissectfn (list)
              >=?-knowable-promise)
            #:on-join-knowable-promise-maybe-knowable-promise
            (dissectfn (list)
              join-knowable-promise-maybe-knowable-promise)
            #:on-meet-knowable-promise-maybe-knowable-promise
            (dissectfn (list)
              meet-knowable-promise-maybe-knowable-promise)
            #:on-==-knowable-promise-maybe-knowable-promise
            (dissectfn (list)
              (delay/strict /known /nothing))
            #:on-path-related-knowable-promise-maybe-knowable-promise
            (dissectfn (list)
              (delay/strict /known /just /delay/strict /known a)))
          report-1+))
      
      ))
  
  (#:prop prop:expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type
    ; TODO: According to the `extflonum?` documentation, the
    ; `equal-always-hash-code` we're using here should actually work
    ; properly for `extflonum?` values, even -0.0t0. Make sure it
    ; does. If it doesn't, normalizing -0.0t0 seems to be the only
    ; thing we'll need to worry about.
    (make-expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type-impl-for-atom))
  
  (#:prop prop:expressly-custom-gloss-key-dynamic-type
    (make-expressly-custom-gloss-key-dynamic-type-impl
      
      #:get-custom-gloss-key-reports
      (fn self a
        (expect (non-nan-extflonum? a) #t
          (uninformative-custom-gloss-key-reports)
        /constant-custom-gloss-key-reports
          #:tagged-glossesque-sys-knowable
          (known /tagged-glossesque-sys
            non-nan-extflonum?
            (equal-always-atom-glossesque-sys))))
      
      ))
  
  )

(define/own-contract
  (on-cons-smoosh-result-knowable-promise-maybe-knowable-promise
    kpmkp-list)
  (->
    (list/c
      (promise/c
        (knowable/c (maybe/c (promise/c (knowable/c pair?)))))
      (promise/c
        (knowable/c (maybe/c (promise/c (knowable/c pair?))))))
    (promise/c (knowable/c (maybe/c (promise/c (knowable/c pair?))))))
  (maybe-min-knowable-promise-zip*-map kpmkp-list /fn kp-list
    (knowable-promise-zip*-map kp-list /dissectfn
      (list result-car result-cdr)
      (cons result-car result-cdr))))

; NOTE: This would be used like so:
;
#;
(#:prop prop:expressly-has-dynamic-type
  (make-expressly-has-dynamic-type-impl /fn bindings self
    (expect
      (known-value /gloss-ref-maybe-knowable bindings
        (dynamic-type-var-for-any-dynamic-type))
      (just any-dt)
      (raise-arguments-error 'get-dynamic-type
        "tried to get the dynamic type of a cons cell without giving a binding for (dynamic-type-var-for-any-dynamic-type)"
        "bindings" bindings
        "inhabitant" self)
    /cons-dynamic-type any-dt)))
;
(define-imitation-simple-struct
  (cons-dynamic-type? cons-dynamic-type-get-any-dynamic-type)
  cons-dynamic-type
  'cons-dynamic-type (current-inspector) (auto-write)
  
  (#:prop
    (make-expressly-smooshable-bundle-property-from-list-isomorphism
      #:ignore-chaperones? #t
      
      #:self-get-any-dynamic-type
      (dissectfn (cons-dynamic-type any-dt)
        any-dt)
      
      #:inhabitant?
      (makeshift-knowable-predicate /fn v
        ; TODO FORWARD: This use of `base-readable?` is a forward
        ; reference. See if we can untangle it.
        (knowable-if (base-readable? v) /fn /pair? v))
      
      #:->list (dissectfn (cons first rest) /list first rest)
      
      #:example-and-list->
      (fn example lst
        (dissect lst (list first rest)
        /cons first rest))
      
      #:get-smoosh-of-zero-reports
      (fn self
        (dissect self (cons-dynamic-type any-dt)
        /smoosh-reports-map
          (dynamic-type-get-smoosh-of-zero-reports any-dt)
          #:on-smoosh-result-knowable-promise-maybe-knowable-promise
          (fn kpmkp
            (promise-map kpmkp /fn kpmk
              (knowable-map kpmk /fn kpm
                (maybe-map kpm /fn kp
                  (promise-map kp /fn k
                    (knowable-map k /fn result
                      (cons result result)))))))))
      
      )
    (trivial))
  
  )

; This is an appropriate dynamic type of immutable vectors and their
; chaperones, information-ordered in a way that's consistent with
; `chaperone-of?` as long as the elements' information orderings are.
; This is an instance of
; `make-expressly-smooshable-dynamic-type-impl-from-chaperone-of-list-isomorphism`.
;
; NOTE: This would be used like so:
;
#;
(#:prop prop:expressly-has-dynamic-type
  (make-expressly-has-dynamic-type-impl /fn bindings self
    (expect
      (known-value /gloss-ref-maybe-knowable bindings
        (dynamic-type-var-for-any-dynamic-type))
      (just any-dt)
      (raise-arguments-error 'get-dynamic-type
        "tried to get the dynamic type of an immutable vector without giving a binding for (dynamic-type-var-for-any-dynamic-type)"
        "bindings" bindings
        "inhabitant" self)
    /immutable-vector-dynamic-type any-dt)))
;
(define-imitation-simple-struct
  (immutable-vector-dynamic-type?
    immutable-vector-dynamic-type-get-any-dynamic-type)
  immutable-vector-dynamic-type
  'immutable-vector-dynamic-type (current-inspector) (auto-write)
  
  (#:prop
    (make-expressly-smooshable-bundle-property-from-list-isomorphism
      
      #:self-get-any-dynamic-type
      (dissectfn (immutable-vector-dynamic-type any-dt)
        any-dt)
      
      #:inhabitant? (fn v /and (vector? v) (immutable? v))
      #:->list (fn v /vector->list v)
      
      #:example-and-list->
      (fn example lst
        (vector->immutable-vector /list->vector lst))
      
      #:copy (fn v /vector->immutable-vector /vector-copy v)
      
      )
    (trivial))
  
  )

(define/own-contract (base-mutable-readable? v)
  (-> any/c boolean?)
  (or
    (and (string? v) (not /immutable? v))
    (and (bytes? v) (not /immutable? v))
    (and (box? v) (not /immutable? v))
    (and (vector? v) (not /immutable? v))
    (mutable-prefab-struct? v)
    (and (hash? v) (not /immutable? v))))

; This is an appropriate dynamic type of mutable strings, mutable byte
; strings, mutable boxes, mutable vectors, prefab structs with mutable
; fields, mutable hash tables, and their chaperones, distinguishable
; from each other and information-ordered in a way that's consistent
; with `chaperone-of?`. This is an instance of
; `make-expressly-smooshable-dynamic-type-impl-for-chaperone-of-atom`.
;
; Note that while most such instances return known results only when
; all the operands pass their `#:inhabitant?` predicate, this one
; considers `base-mutable-readable?` values to be known inhabitants
; and other `base-readable?` values to be known non-inhabitants, and
; it reports a known inhabitant and a known non-inhabitant as being
; known to be distinct.
;
(define-imitation-simple-struct (base-mutable-readable-dynamic-type?)
  base-mutable-readable-dynamic-type
  'base-mutable-readable-dynamic-type (current-inspector) (auto-write)
  
  (#:prop
    (make-expressly-smooshable-bundle-property-for-atom
      
      #:inhabitant?
      (makeshift-knowable-predicate /fn v
        ; TODO FORWARD: This use of `base-readable?` is a forward
        ; reference. See if we can untangle it.
        (knowable-if (base-readable? v) /fn /base-mutable-readable? v))
      
      )
    (trivial)))

; This is an appropriate dynamic type of immutable boxes and their
; chaperones, information-ordered in a way that's consistent with
; `chaperone-of?` as long as the elements' information orderings are.
; This is an instance of
; `make-expressly-smooshable-dynamic-type-impl-from-chaperone-of-list-isomorphism`.
;
(define-imitation-simple-struct
  (immutable-box-dynamic-type?
    immutable-box-dynamic-type-get-any-dynamic-type)
  immutable-box-dynamic-type
  'immutable-box-dynamic-type (current-inspector) (auto-write)
  
  (#:prop
    (make-expressly-smooshable-bundle-property-from-list-isomorphism
      
      #:self-get-any-dynamic-type
      (dissectfn (immutable-box-dynamic-type any-dt)
        any-dt)
      
      #:inhabitant? (fn v /and (box? v) (immutable? v))
      #:->list (fn b /list /unbox b)
      
      #:example-and-list->
      (fn example lst
        (dissect lst (list e)
        /box-immutable e))
      
      #:copy (fn v /box-immutable /unbox v)
      
      #:get-smoosh-of-zero-reports
      (fn self
        (dissect self (immutable-box-dynamic-type any-dt)
        /smoosh-reports-map
          (dynamic-type-get-smoosh-of-zero-reports any-dt)
          #:on-smoosh-result-knowable-promise-maybe-knowable-promise
          (fn kpmkp
            (promise-map kpmkp /fn kpmk
              (knowable-map kpmk /fn kpm
                (maybe-map kpm /fn kp
                  (promise-map kp /fn k
                    (knowable-map k /fn result
                      (box-immutable result)))))))))
      
      )
    (trivial))
  
  )

; This is an appropriate dynamic type of immutable prefab structs and
; their chaperones, information-ordered in a way that's consistent
; with `chaperone-of?` as long as the elements' information orderings
; are. This is an instance of
; `make-expressly-smooshable-dynamic-type-impl-from-chaperone-of-list-isomorphism`.
;
(define-imitation-simple-struct
  (immutable-prefab-struct-dynamic-type?
    immutable-prefab-struct-dynamic-type-get-any-dynamic-type)
  immutable-prefab-struct-dynamic-type
  'immutable-prefab-struct-dynamic-type (current-inspector)
  (auto-write)
  
  (#:prop
    (make-expressly-smooshable-bundle-property-from-list-isomorphism
      
      #:self-get-any-dynamic-type
      (dissectfn (immutable-prefab-struct-dynamic-type any-dt)
        any-dt)
      
      #:inhabitant? immutable-prefab-struct?
      #:->list (fn s /cdr /vector->list /struct->vector s)
      #:example-and-list->
      (fn example lst
        (apply make-prefab-struct (prefab-struct-key example) lst)))
    (trivial))
  
  )

; This is an appropriate dynamic type of immutable hash tables and
; their chaperones, information-ordered in a way that's consistent
; with `chaperone-of?` as long as the keys' and values' information
; orderings are. This is an instance of
; `make-expressly-smooshable-dynamic-type-impl-from-chaperone-of-list-isomorphism`.
;
(define-imitation-simple-struct
  (immutable-hash-dynamic-type?
    immutable-hash-dynamic-type-get-any-dynamic-type)
  immutable-hash-dynamic-type
  'immutable-hash-dynamic-type (current-inspector) (auto-write)
  
  (#:prop
    (make-expressly-smooshable-bundle-property-from-list-isomorphism
      
      #:self-get-any-dynamic-type
      (dissectfn (immutable-hash-dynamic-type any-dt)
        any-dt)
      
      #:inhabitant? (fn v /and (hash? v) (immutable? v))
      
      #:->->list
      (fn a
        (w- keys (hash-keys a)
        /fn b
          (append* /for/list ([k (in-list keys)])
            (list k (hash-ref b k)))))
      
      #:example-and-list->
      (fn example lst
        (make-similar-hash example
          (for/list ([entry (in-slice 2 (in-list lst))])
            (dissect entry (list k v)
            /cons k v))))
      
      #:copy (fn v /hash-v-map v /fn v v)
      
      #:combine-element-hash-codes
      (fn element-hash-codes
        (hash-code-combine-unordered*
          (for/list ([entry (in-slice 2 (in-list element-hash-codes))])
            (dissect entry (list k v)
            /hash-code-combine k v))))
      
      )
    (trivial))
  
  )

(define/own-contract
  (dynamic-type-case-by-cases
    name
    #:known-distinct? [known-distinct? #t]
    #:known-discrete? [known-discrete? #f]
    cases)
  (->*
    (symbol? (listof (list/c (-> any/c boolean?) (-> any/c any/c))))
    (#:known-distinct? boolean? #:known-discrete? boolean?)
    (list/c (-> any/c boolean?) (-> any/c any/c)))
  (define distinct-cases-smoosh-and-comparison-of-two-reports
    (smoosh-and-comparison-of-two-reports-with-hesitation-at-discrepancies
      #:known-distinct? known-distinct?
      #:known-discrete? known-discrete?
      (false-smoosh-and-comparison-of-two-reports)))
  (define (inhabitant? v)
    (list-any cases /dissectfn (list check? dt)
      (check? v)))
  (define-imitation-simple-struct
    (case-dynamic-type? case-dynamic-type-get-any-dynamic-type)
    case-dynamic-type
    name
    (current-inspector)
    (auto-write)
    
    (#:prop prop:expressly-smooshable-dynamic-type
      (make-expressly-smooshable-dynamic-type-impl
        
        #:get-smoosh-of-zero-reports
        (fn self
          (uninformative-smoosh-reports))
        
        #:get-smoosh-of-one-reports
        (fn self a
          (dissect self (case-dynamic-type any-dt)
          /w-loop next cases cases
            (expect cases (cons case cases)
              (uninformative-smoosh-reports)
            /dissect case (list check? dt)
            /if (check? a)
              (dynamic-type-get-smoosh-of-one-reports (dt any-dt) a)
            /next cases)))
        
        #:get-smoosh-and-comparison-of-two-reports
        (fn self a b
          (dissect self (case-dynamic-type any-dt)
          /w-loop next cases cases
            (expect cases (cons case cases)
              (uninformative-smoosh-and-comparison-of-two-reports)
            /dissect case (list check? dt)
            /match (list (check? a) (check? b))
              [ (list #t #t)
                (w- a-dt (dt any-dt)
                /dynamic-type-get-smoosh-and-comparison-of-two-reports
                  a-dt a b)]
              [ (list #t #f)
                (if
                  (list-any cases /dissectfn (list check? dt)
                    (check? b))
                  distinct-cases-smoosh-and-comparison-of-two-reports
                  (uninformative-smoosh-and-comparison-of-two-reports))]
              [ (list #f #t)
                (if
                  (list-any cases /dissectfn (list check? dt)
                    (check? a))
                  distinct-cases-smoosh-and-comparison-of-two-reports
                  (uninformative-smoosh-and-comparison-of-two-reports))]
              [(list #f #f) (next cases)])))
        
        ))
    
    (#:prop prop:expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type
      (make-expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type-impl
        
        #:get-smoosh-equal-hash-code-support-reports
        (fn self a
          (dissect self (case-dynamic-type any-dt)
          /w-loop next cases cases
            (expect cases (cons case cases)
              (uninformative-smoosh-equal-hash-code-support-reports)
            /dissect case (list check? dt)
            /if (check? a)
              (dynamic-type-get-smoosh-equal-hash-code-support-reports
                (dt any-dt)
                a)
            /next cases)))
        
        ))
    
    (#:prop prop:expressly-custom-gloss-key-dynamic-type
      (make-expressly-custom-gloss-key-dynamic-type-impl
        
        #:get-custom-gloss-key-reports
        (fn self a
          (dissect self (case-dynamic-type any-dt)
          /w-loop next cases cases
            (expect cases (cons case cases)
              (uninformative-custom-gloss-key-reports)
            /dissect case (list check? dt)
            /if (check? a)
              (dynamic-type-get-custom-gloss-key-reports (dt any-dt)
                a)
            /next cases)))
        
        ))
    
    )
  (list inhabitant? case-dynamic-type))

(define base-literal-dynamic-type-case
  (dynamic-type-case-by-cases 'base-literal-dynamic-type
    #:known-distinct? #f
    (list
      (list
        (fn v
          (or
            (nan-number? v)
            (nan-extflonum? v)
            (regexp? v)
            (compiled-expression? v)))
        (fn any-dt /uninformative-dynamic-type))
      (list boolean? (fn any-dt /boolean-dynamic-type))
      (list char? (fn any-dt /char-dynamic-type))
      (list
        immutable-string?
        (fn any-dt /immutable-string-dynamic-type))
      (list
        (fn v /and (bytes? v) (immutable? v))
        (fn any-dt /immutable-bytes-dynamic-type))
      (list non-nan-number? (fn any-dt /non-nan-number-dynamic-type))
      (list
        non-nan-extflonum?
        (fn any-dt /non-nan-extflonum-dynamic-type))
      (list
        (fn v /and (vector? v) (immutable? v))
        (fn any-dt /immutable-vector-dynamic-type any-dt))
      (list
        (fn v /and (box? v) (immutable? v))
        (fn any-dt /immutable-box-dynamic-type any-dt))
      (list
        immutable-prefab-struct?
        (fn any-dt /immutable-prefab-struct-dynamic-type any-dt))
      (list
        (fn v /and (hash? v) (immutable? v))
        (fn any-dt /immutable-hash-dynamic-type any-dt)))))

(match-define (list base-readable? base-readable-dynamic-type)
  (dynamic-type-case-by-cases 'base-readable-dynamic-type /list
    (list
      base-mutable-readable?
      (fn any-dt /base-mutable-readable-dynamic-type))
    (list flvector? (fn any-dt /flvector-dynamic-type))
    (list fxvector? (fn any-dt /fxvector-dynamic-type))
    (list
      base-syntactic-atom?
      (fn any-dt /base-syntactic-atom-dynamic-type))
    (list pair? (fn any-dt /cons-dynamic-type any-dt))
    base-literal-dynamic-type-case))

; Level 0+:
;   <=, >=, path-related, join, meet, ==:
;     If one operand is a `nothing?` value and another is a `just?`
;     value, then a known nothing (or, for a check, `#f`).
;     
;     Otherwise, if the operands are not both `nothing?` values, then
;     unknown.
;     
;     Otherwise, the first operand (or, for a check, `#t`).
;
(define-imitation-simple-struct (nothing-dynamic-type?)
  nothing-dynamic-type
  'nothing-dynamic-type (current-inspector) (auto-write)
  
  (#:prop
    (make-expressly-smooshable-bundle-property-for-atom
      #:ignore-chaperones? #t
      
      #:inhabitant?
      (makeshift-knowable-predicate /fn v
        (knowable-if (maybe? v) /fn /nothing? v))
      
      )
    (trivial)))

; This is an appropriate dynamic type of `just?` values. This is an
; instance of
; `make-expressly-smooshable-dynamic-type-impl-from-equal-always-list-isomorphism`.
;
; Note that while most such instances return known results only when
; all the operands pass their `#:inhabitant?` predicate, this one
; considers `just?` values to be known inhabitants and `nothing?`
; values to be known non-inhabitants, and it reports a known
; inhabitant and a known non-inhabitant as being known to be distinct.
;
(define-imitation-simple-struct
  (just-dynamic-type? just-dynamic-type-get-any-dynamic-type)
  just-dynamic-type
  'just-dynamic-type (current-inspector) (auto-write)
  
  (#:prop
    (make-expressly-smooshable-bundle-property-from-list-isomorphism
      #:ignore-chaperones? #t
      
      #:self-get-any-dynamic-type
      (dissectfn (just-dynamic-type any-dt)
        any-dt)
      
      #:inhabitant?
      (makeshift-knowable-predicate /fn v
        (knowable-if (maybe? v) /fn /just? v))
      
      #:->list (dissectfn (just e) /list e)
      
      #:example-and-list->
      (fn example lst
        (dissect lst (list e)
        /just e))
      
      #:get-smoosh-of-zero-reports
      (dissectfn (just-dynamic-type any-dt)
        (smoosh-reports-map
          (dynamic-type-get-smoosh-of-zero-reports any-dt)
          #:on-smoosh-result-knowable-promise-maybe-knowable-promise
          (fn kpmkp
            (promise-map kpmkp /fn kpmk
              (knowable-map kpmk /fn kpm
                (maybe-map kpm /fn kp
                  (promise-map kp /fn k
                    (knowable-map k /fn result-value
                      (just result-value)))))))))
      
      )
    (trivial))
  
  )

; Level 0+:
;   <=, >=, path-related, join, meet, ==:
;     If the operands are not both `trivial?` values, then unknown.
;     
;     Otherwise, the first operand (or, for a check, `#t`).
;
(define-imitation-simple-struct (trivial-dynamic-type?)
  trivial-dynamic-type
  'trivial-dynamic-type (current-inspector) (auto-write)
  
  (#:prop
    (make-expressly-smooshable-bundle-property-for-atom
      #:ignore-chaperones? #t
      #:inhabitant? trivial?)
    (trivial)))

(define/own-contract
  (on-known-smoosh-result-knowable-promise-maybe-knowable-promise
    kpmkp)
  (->
    (promise/c (knowable/c (maybe/c (promise/c (knowable/c pair?)))))
    (promise/c (knowable/c (maybe/c (promise/c (knowable/c pair?))))))
  (promise-map kpmkp /fn kpmk
    (knowable-map kpmk /fn kpm
      (maybe-map kpm /fn kp
        (promise-map kp /fn k
          (knowable-map k /fn result-value
            (known result-value)))))))

; This is an appropriate dynamic type of `known?` and
; `example-unknown?` values, with handling of their interactions with
; other `unknown?` values. These are information-ordered so that
; `unknown?` values are considered to represent less information than
; `known?` values.
;
; Level 0:
;   path-related, join, meet, ==:
;     If the operands are not both `known?` values, then unknown.
;     
;     Otherwise, the result of performing the same smoosh on their
;     values, then wrapping the result in a `known?` if it's
;     successful.
;   <=, >=:
;     If the operands are not both `known?` values, then unknown.
;     
;     Otherwise, the result of performing the same check on their
;     values.
; Level 1:
;   path-related:
;     If none of the operands is a `known?` value or an
;     `example-unknown?` value, then unknown.
;     
;     Otherwise, if the operands are not both `knowable?` values, then
;     unknown.
;     
;     Otherwise, the first operand (which, if it's an `unknown?`
;     value, is a known unknown).
;   join, meet, ==:
;     If none of the operands is a `known?` value or an
;     `example-unknown?` value, then unknown. (This means this type
;     won't smoosh zero operands, and it won't smoosh
;     non-`example-unknown?` `unknown?` values with each other. The
;     result of smooshing other `unknown?` values should be another
;     `unknown?` value, but which one it is is unknown as far as this
;     type is concerned (an unknown unknown).)
;     
;     Otherwise, if the operands are not both `knowable?` values, then
;     unknown.
;     
;     Otherwise, if the operands are both `example-unknown?` values,
;     then the first operand (a known unknown).
;     
;     Otherwise, if the operands are both `unknown?` values, then the
;     non-`example-unknown?` operand (a known unknown).
;     
;     Otherwise, if the operands are both `known?` values, then the
;     result of performing the same smoosh on their values, then
;     wrapping the result in a `known?` if it's successful.
;     
;     Otherwise, for:
;       join:
;         The `known?` operand.
;       meet:
;         The `unknown?` operand.
;       ==:
;         A known nothing.
;   <=, >=:
;     If none of the operands is a `known?` value or an
;     `example-unknown?` value, then unknown.
;     
;     Otherwise, if the operands are both `known?` values, then the
;     result of performing the same check on their values.
;     
;     Otherwise, a boolean indicating whether the element we're
;     proposing to be lesser is `unknown?`.
; Level 2+:
;   path-related, join, meet, ==:
;     Same as the description of level 1 ==.
;   <=, >=:
;     Same as the description of level 1 == as a check.
;
(define-imitation-simple-struct
  (knowable-dynamic-type? knowable-dynamic-type-get-any-dynamic-type)
  knowable-dynamic-type
  'knowable-dynamic-type (current-inspector) (auto-write)
  
  (#:prop prop:expressly-smooshable-dynamic-type
    (make-expressly-smooshable-dynamic-type-impl
      
      #:get-smoosh-of-zero-reports
      (fn self
        (uninformative-smoosh-reports))
      
      #:get-smoosh-of-one-reports
      (fn self a
        (dissect self (knowable-dynamic-type any-dt)
        /expect (or (known? a) (example-unknown? a)) #t
          (uninformative-smoosh-reports)
        /mat a (known a-value)
          (smoosh-reports-map
            (dynamic-type-get-smoosh-of-one-reports any-dt a-value)
            #:on-result-knowable-promise-maybe-knowable-promise
            on-known-smoosh-result-knowable-promise-maybe-knowable-promise)
        /stream* (uninformative-smoosh-report)
          (constant-smoosh-reports
            (delay/strict /known /just /delay/strict /known a))))
      
      #:get-smoosh-and-comparison-of-two-reports
      (fn self a b
        (dissect self (knowable-dynamic-type any-dt)
        /expect (knowable? a) #t
          (uninformative-smoosh-and-comparison-of-two-reports)
        /expect (knowable? b) #t
          (uninformative-smoosh-and-comparison-of-two-reports)
        /if (and (unknown? a) (unknown? b))
          (if (example-unknown? b)
            (constant-smoosh-and-comparison-of-two-reports
              (delay/strict /known /just /delay/strict /known a))
          /if (example-unknown? a)
            (constant-smoosh-and-comparison-of-two-reports
              (delay/strict /known /just /delay/strict /known b))
          /uninformative-smoosh-and-comparison-of-two-reports)
        /mat (list a b) (list (known a-value) (known b-value))
          (smoosh-reports-map
            (dynamic-type-get-smoosh-and-comparison-of-two-reports
              any-dt a-value b-value)
            #:on-result-knowable-promise-maybe-knowable-promise
            on-known-smoosh-result-knowable-promise-maybe-knowable-promise)
        /stream* (uninformative-smoosh-and-comparison-of-two-report)
          (smoosh-and-comparison-of-two-report-zip*-map (list)
            
            #:on-<=?-knowable-promise
            (w- result (unknown? a)
              (dissectfn (list)
                (delay/strict /known result)))
            
            #:on->=?-knowable-promise
            (w- result (unknown? b)
              (dissectfn (list)
                (delay/strict /known result)))
            
            #:on-join-knowable-promise-maybe-knowable-promise
            (w- result (if (known? a) a b)
              (dissectfn (list)
                (delay/strict /known /just /delay/strict /known
                  result)))
            
            #:on-meet-knowable-promise-maybe-knowable-promise
            (w- result (if (known? a) b a)
              (dissectfn (list)
                (delay/strict /known /just /delay/strict /known
                  result)))
            
            #:on-==-knowable-promise-maybe-knowable-promise
            (dissectfn (list)
              (delay/strict /known /nothing))
            
            #:on-path-related-knowable-promise-maybe-knowable-promise
            (dissectfn (list)
              (delay/strict /known /just /delay/strict /known a))
            
            )
          (false-smoosh-and-comparison-of-two-reports)))
      
      ))
  
  (#:prop prop:expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type
    (make-expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type-impl
      
      #:get-smoosh-equal-hash-code-support-reports
      (fn self a
        (dissect self (knowable-dynamic-type any-dt)
        /expect (or (known? a) (example-unknown? a)) #t
          (uninformative-smoosh-equal-hash-code-support-reports)
        /mat a (known a-value)
          (smoosh-equal-hash-code-support-reports-map
            (dynamic-type-get-smoosh-equal-hash-code-support-reports
              any-dt a-value)
            #:on-hash-code-promise
            (fn p
              (promise-map p /fn a-value-hash-code
                (hash-code-combine
                  (equal-always-hash-code known?)
                  a-value-hash-code))))
        /stream* (uninformative-smoosh-equal-hash-code-support-report)
          (constant-smoosh-equal-hash-code-support-reports
            (delay
              (hash-code-combine
                (equal-always-hash-code example-unknown?))))))
      
      ))
  
  (#:prop prop:expressly-custom-gloss-key-dynamic-type
    (make-expressly-custom-gloss-key-dynamic-type-impl
      
      #:get-custom-gloss-key-reports
      (fn self a
        (dissect self (path-related-wrapper-dynamic-type any-dt)
        /if (example-unknown? a)
          (stream*
            (uninformative-custom-gloss-key-report)
            (custom-gloss-key-report-zip*-map (list)
              
              #:on-path-related-tagged-glossesque-sys-knowable
              (dissectfn (list)
                ; NOTE PATH-RELATED KNOWABLE: This case is shared with
                ; the other PATH-RELATED KNOWABLE case. Whichever one
                ; of these glossesques is used in a gloss first will
                ; take care of the other case.
                (known /tagged-glossesque-sys
                  knowable?
                  (terminal-glossesque-sys)))
              
              #:on-==-tagged-glossesque-sys-knowable
              (dissectfn (list)
                (known /tagged-glossesque-sys
                  (makeshift-knowable-predicate /fn v
                    (knowable-if (knowable? v) /fn /unknown? v))
                  (terminal-glossesque-sys)))
              
              )
            (constant-custom-gloss-key-reports
              #:tagged-glossesque-sys-knowable
              (known /tagged-glossesque-sys
                (makeshift-knowable-predicate /fn v
                  (knowable-if (knowable? v) /fn /unknown? v))
                (terminal-glossesque-sys))))
        /expect a (known a-value)
          (uninformative-custom-gloss-key-reports)
        /dissect
          (custom-gloss-key-reports-map
            (dynamic-type-get-custom-gloss-key-reports any-dt a-value)
            #:on-tagged-glossesque-sys-knowable
            (fn tgs-k
              (knowable-map tgs-k
                (dissectfn (tagged-glossesque-sys inhabitant? gs)
                  (tagged-glossesque-sys
                    (makeshift-knowable-predicate /fn v
                      (expect v (path-related-wrapper v) (unknown)
                      /call-knowable inhabitant? v))
                    (glossesque-sys-map-key gs #:granted-key-knowable /fn k
                      (expect k (path-related-wrapper k) (unknown)
                      /known k)))))))
          (app sequence->stream /stream* report-0 report-1 report-2+)
        /w- tgs-k-uninhabited-by-unknown
          (fn tgs-k
            (knowable-map tgs-k
              (dissectfn (tagged-glossesque-sys inhabitant? gs)
                (tagged-glossesque-sys
                  (makeshift-knowable-predicate /fn v
                    (if (unknown? v) (known #f)
                    /call-knowable inhabitant? v))
                  gs))))
        /stream*
          report-0
          (custom-gloss-key-report-map report-1
            
            #:on-path-related-tagged-glossesque-sys-knowable
            (fn tgs-k
              ; NOTE PATH-RELATED KNOWABLE: This case is shared with
              ; the other PATH-RELATED KNOWABLE case. Whichever one of
              ; these glossesques is used in a gloss first will take
              ; care of the other case.
              (known /tagged-glossesque-sys
                knowable?
                (terminal-glossesque-sys)))
            
            #:on-==-tagged-glossesque-sys-knowable
            tgs-k-uninhabited-by-unknown
            
            )
          (custom-gloss-key-reports-map report-1
            #:on-tagged-glossesque-sys-knowable
            tgs-k-uninhabited-by-unknown)))
      
      ))
  
  )

(define/own-contract
  (on-path-related-wrapper-smoosh-result-knowable-promise-maybe-knowable-promise
    kpmkp)
  (->
    (promise/c (knowable/c (maybe/c (promise/c (knowable/c pair?)))))
    (promise/c (knowable/c (maybe/c (promise/c (knowable/c pair?))))))
  (promise-map kpmkp /fn kpmk
    (knowable-map kpmk /fn kpm
      (maybe-map kpm /fn kp
        (promise-map kp /fn k
          (knowable-map k /fn result-value
            (path-related-wrapper result-value)))))))

(define/own-contract (on-path-related-wrapper-hash-code-promise p)
  (-> (promise/c fixnum?) (promise/c fixnum?))
  (promise-map p /fn value-hash-code
    (hash-code-combine
      (equal-always-hash-code path-related-wrapper?)
      value-hash-code)))

(define/own-contract
  (path-related-wrapper-smoosh-reports-from-value-reports
    value-reports)
  (-> (sequence/c smoosh-report?) (sequence/c smoosh-report?))
  (dissect
    (smoosh-reports-map value-reports
      #:on-smoosh-result-knowable-promise-maybe-knowable-promise
      on-path-related-wrapper-smoosh-result-knowable-promise-maybe-knowable-promise)
    (app sequence->stream /stream* report-0 report-1+)
  /stream*
    (constant-smoosh-report
      (smoosh-report-path-related-knowable-promise-maybe-knowable-promise
        report-0))
    report-1+))

(define/own-contract
  (path-related-wrapper-smoosh-and-comparison-of-two-reports-from-value-reports
    value-reports)
  (-> (sequence/c smoosh-and-comparison-of-two-report?)
    (sequence/c smoosh-and-comparison-of-two-report?))
  (dissect
    (smoosh-and-comparison-of-two-reports-map value-reports
      #:on-smoosh-result-knowable-promise-maybe-knowable-promise
      on-path-related-wrapper-smoosh-result-knowable-promise-maybe-knowable-promise)
    (app sequence->stream /stream* report-0 report-1+)
  /stream*
    (constant-smoosh-report
      (smoosh-report-path-related-knowable-promise-maybe-knowable-promise
        (smoosh-and-comparison-of-two-report-get-smoosh-report
          report-0)))
    report-1+))

(define/own-contract
  (path-related-wrapper-smoosh-equal-hash-code-support-reports-from-value-reports
    value-reports)
  (-> (sequence/c smoosh-equal-hash-code-support-report?)
    (sequence/c smoosh-equal-hash-code-support-report?))
  (dissect
    (smoosh-equal-hash-code-support-reports-map value-reports
      #:on-hash-code-promise
      on-path-related-wrapper-hash-code-promise)
    (app sequence->stream /stream* report-0 report-1+)
  /stream*
    (constant-smoosh-equal-hash-code-support-report
      (smoosh-equal-hash-code-support-report-path-related-hash-code-promise
        report-0))
    report-1+))

(define/own-contract
  (path-related-wrapper-custom-gloss-key-reports-from-value-reports
    value-reports)
  (-> (sequence/c custom-gloss-key-report?)
    (sequence/c custom-gloss-key-report?))
  (dissect
    (custom-gloss-key-reports-map value-reports
      #:on-tagged-glossesque-sys-knowable
      (fn tgs-k
        (knowable-map tgs-k
          (dissectfn (tagged-glossesque-sys inhabitant? gs)
            (tagged-glossesque-sys
              (makeshift-knowable-predicate /fn v
                (expect v (path-related-wrapper v) (unknown)
                /call-knowable inhabitant? v))
              (glossesque-sys-map-key gs #:granted-key-knowable /fn k
                (expect k (path-related-wrapper k) (unknown)
                /known k)))))))
    (app sequence->stream /stream* report-0 report-1+)
  /stream*
    (constant-custom-gloss-key-report
      (custom-gloss-key-report-get-path-related-tagged-glossesque-sys-knowable
        report-0))
    report-1+))

; This is an appropriate dynamic type of `path-related-wrapper`
; values, ordered so that one value is less than or equal to another
; if the wrapped values are path-related (related by some sequence of
; two or more values, beginning with one of them and ending with the
; other, such that each pair of successive elements is related either
; by <= or by >=).
;
; Level 0:
;   path-related, join, meet, ==:
;     If the operands are not both `path-related-wrapper?` values,
;     then unknown.
;     
;     Otherwise, the result of performing a level-0 path-related
;     smoosh on their values, then wrapping the result in a
;     `path-related-wrapper?` if it's successful.
;   <=, >=:
;     If the operands are not both `path-related-wrapper?` values,
;     then unknown.
;     
;     Otherwise, the boolean result of whether or not performing a
;     level-0 path-related smoosh on their values succeeds.
; Level 1+:
;   <=, >=, path-related, join, meet, ==:
;     If the operands are not both `path-related-wrapper?` values,
;     then unknown.
;     
;     Otherwise, the result of performing the same smoosh or check on
;     their values, then wrapping any successful smoosh result in a
;     `path-related-wrapper?`.
;
(define-imitation-simple-struct
  (path-related-wrapper-dynamic-type?
    path-related-wrapper-dynamic-type-get-any-dynamic-type)
  path-related-wrapper-dynamic-type
  'path-related-wrapper-dynamic-type (current-inspector) (auto-write)
  
  (#:prop prop:expressly-smooshable-dynamic-type
    (make-expressly-smooshable-dynamic-type-impl
      
      #:get-smoosh-of-zero-reports
      (fn self
        (dissect self (path-related-wrapper-dynamic-type any-dt)
        /path-related-wrapper-smoosh-reports-from-value-reports
          (dynamic-type-get-smoosh-of-zero-reports any-dt)))
      
      #:get-smoosh-of-one-reports
      (fn self a
        (dissect self (path-related-wrapper-dynamic-type any-dt)
        /expect a (path-related-wrapper a-value)
          (uninformative-smoosh-reports)
        /path-related-wrapper-smoosh-reports-from-value-reports
          (dynamic-type-get-smoosh-of-one-reports any-dt a-value)))
      
      #:get-smoosh-and-comparison-of-two-reports
      (fn self a b
        (dissect self (path-related-wrapper-dynamic-type any-dt)
        /expect a (path-related-wrapper a-value)
          (uninformative-smoosh-and-comparison-of-two-reports)
        /expect b (path-related-wrapper b-value)
          (uninformative-smoosh-and-comparison-of-two-reports)
        /path-related-wrapper-smoosh-and-comparison-of-two-reports-from-value-reports
          (dynamic-type-get-smoosh-and-comparison-of-two-reports
            any-dt a-value b-value)))
      
      #:get-smoosh-and-comparison-of-two-reports-via-second
      (fn self a b
        (dissect self (path-related-wrapper-dynamic-type any-dt)
        /expect a (path-related-wrapper a-value)
          (uninformative-smoosh-and-comparison-of-two-reports)
        /expect b (path-related-wrapper b-value)
          (uninformative-smoosh-and-comparison-of-two-reports)
        /path-related-wrapper-smoosh-and-comparison-of-two-reports-from-value-reports
          (dynamic-type-get-smoosh-and-comparison-of-two-reports-via-second
            any-dt a-value b-value)))
      
      ))
  
  (#:prop prop:expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type
    (make-expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type-impl
      
      #:get-smoosh-equal-hash-code-support-reports
      (fn self a
        (dissect self (path-related-wrapper-dynamic-type any-dt)
        /expect a (path-related-wrapper a-value)
          (uninformative-smoosh-equal-hash-code-support-reports)
        /path-related-wrapper-smoosh-equal-hash-code-support-reports-from-value-reports
          (dynamic-type-get-smoosh-equal-hash-code-support-reports
            any-dt a-value)))
      
      ))
  
  (#:prop prop:expressly-custom-gloss-key-dynamic-type
    (make-expressly-custom-gloss-key-dynamic-type-impl
      
      #:get-custom-gloss-key-reports
      (fn self a
        (dissect self (path-related-wrapper-dynamic-type any-dt)
        /expect a (path-related-wrapper a-value)
          (uninformative-custom-gloss-key-reports)
        /path-related-wrapper-custom-gloss-key-reports-from-value-reports
          (dynamic-type-get-custom-gloss-key-reports
            any-dt a-value)))
      
      ))
  
  )

(define/own-contract
  (on-info-wrapper-smoosh-result-knowable-promise-maybe-knowable-promise
    kpmkp)
  (->
    (promise/c (knowable/c (maybe/c (promise/c (knowable/c pair?)))))
    (promise/c (knowable/c (maybe/c (promise/c (knowable/c pair?))))))
  (promise-map kpmkp /fn kpmk
    (knowable-map kpmk /fn kpm
      (maybe-map kpm /fn kp
        (promise-map kp /fn k
          (knowable-map k /fn result-value
            (info-wrapper result-value)))))))

(define/own-contract (on-info-wrapper-hash-code-promise p)
  (-> (promise/c fixnum?) (promise/c fixnum?))
  (promise-map p /fn value-hash-code
    (hash-code-combine
      (equal-always-hash-code info-wrapper?)
      value-hash-code)))

(define/own-contract
  (info-wrapper-smoosh-reports-from-value-reports value-reports)
  (-> (sequence/c smoosh-report?) (sequence/c smoosh-report?))
  (dissect
    (smoosh-reports-map value-reports
      #:on-smoosh-result-knowable-promise-maybe-knowable-promise
      on-info-wrapper-smoosh-result-knowable-promise-maybe-knowable-promise)
    (app sequence->stream /stream* report-0 report-1+)
    report-1+))

(define/own-contract
  (info-wrapper-smoosh-and-comparison-of-two-reports-from-value-reports
    value-reports)
  (-> (sequence/c smoosh-and-comparison-of-two-report?)
    (sequence/c smoosh-and-comparison-of-two-report?))
  (dissect
    (smoosh-and-comparison-of-two-reports-map value-reports
      #:on-smoosh-result-knowable-promise-maybe-knowable-promise
      on-info-wrapper-smoosh-result-knowable-promise-maybe-knowable-promise)
    (app sequence->stream /stream* report-0 report-1+)
    report-1+))

(define/own-contract
  (info-wrapper-smoosh-equal-hash-code-support-reports-from-value-reports
    value-reports)
  (-> (sequence/c smoosh-equal-hash-code-support-report?)
    (sequence/c smoosh-equal-hash-code-support-report?))
  (dissect
    (smoosh-equal-hash-code-support-reports-map value-reports
      #:on-hash-code-promise on-info-wrapper-hash-code-promise)
    (app sequence->stream /stream* report-0 report-1+)
    report-1+))

(define/own-contract
  (info-wrapper-custom-gloss-key-reports-from-value-reports
    value-reports)
  (-> (sequence/c custom-gloss-key-report?)
    (sequence/c custom-gloss-key-report?))
  (dissect
    (custom-gloss-key-reports-map value-reports
      #:on-tagged-glossesque-sys-knowable
      (fn tgs-k
        (knowable-map tgs-k
          (dissectfn (tagged-glossesque-sys inhabitant? gs)
            (tagged-glossesque-sys
              (makeshift-knowable-predicate /fn v
                (expect v (info-wrapper v) (unknown)
                /call-knowable inhabitant? v))
              (glossesque-sys-map-key gs #:granted-key-knowable /fn k
                (expect k (info-wrapper k) (unknown)
                /known k)))))))
    (app sequence->stream /stream* report-0 report-1+)
    report-1+))

; This is an appropriate dynamic type of `info-wrapper` values,
; ordered so that one value is less than or equal to another if the
; wrapped values are path-related (related by some sequence of two or
; more values, beginning with one of them and ending with the other,
; such that each pair of successive elements is related either by <=
; or by >=).
;
; Level 0+:
;   <=, >=, path-related, join, meet, ==:
;     If the operands are not both `info-wrapper?` values, then
;     unknown.
;     
;     Otherwise, the result of performing the same smoosh or check 1
;     level up on their values, then wrapping any successful smoosh
;     result in an `info-wrapper?`.
;
(define-imitation-simple-struct
  (info-wrapper-dynamic-type?
    info-wrapper-dynamic-type-get-any-dynamic-type)
  info-wrapper-dynamic-type
  'info-wrapper-dynamic-type (current-inspector) (auto-write)
  
  (#:prop prop:expressly-smooshable-dynamic-type
    (make-expressly-smooshable-dynamic-type-impl
      
      #:get-smoosh-of-zero-reports
      (fn self
        (dissect self (info-wrapper-dynamic-type any-dt)
        /info-wrapper-smoosh-reports-from-value-reports
          (dynamic-type-get-smoosh-of-zero-reports any-dt)))
      
      #:get-smoosh-of-one-reports
      (fn self a
        (dissect self (info-wrapper-dynamic-type any-dt)
        /expect a (info-wrapper a-value)
          (uninformative-smoosh-reports)
        /info-wrapper-smoosh-reports-from-value-reports
          (dynamic-type-get-smoosh-of-one-reports any-dt a-value)))
      
      #:get-smoosh-and-comparison-of-two-reports
      (fn self a b
        (dissect self (info-wrapper-dynamic-type any-dt)
        /expect a (info-wrapper a-value)
          (uninformative-smoosh-and-comparison-of-two-reports)
        /expect b (info-wrapper b-value)
          (uninformative-smoosh-and-comparison-of-two-reports)
        /info-wrapper-smoosh-and-comparison-of-two-reports-from-value-reports
          (dynamic-type-get-smoosh-and-comparison-of-two-reports
            any-dt a-value b-value)))
      
      #:get-smoosh-and-comparison-of-two-reports-via-second
      (fn self a b
        (dissect self (info-wrapper-dynamic-type any-dt)
        /expect a (info-wrapper a-value)
          (uninformative-smoosh-and-comparison-of-two-reports)
        /expect b (info-wrapper b-value)
          (uninformative-smoosh-and-comparison-of-two-reports)
        /info-wrapper-smoosh-and-comparison-of-two-reports-from-value-reports
          (dynamic-type-get-smoosh-and-comparison-of-two-reports-via-second
            any-dt a-value b-value)))
      
      ))
  
  (#:prop prop:expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type
    (make-expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type-impl
      
      #:get-smoosh-equal-hash-code-support-reports
      (fn self a
        (dissect self (info-wrapper-dynamic-type any-dt)
        /expect a (info-wrapper a-value)
          (uninformative-smoosh-equal-hash-code-support-reports)
        /info-wrapper-smoosh-equal-hash-code-support-reports-from-value-reports
          (dynamic-type-get-smoosh-equal-hash-code-support-reports
            any-dt a-value)))
      
      ))
  
  (#:prop prop:expressly-custom-gloss-key-dynamic-type
    (make-expressly-custom-gloss-key-dynamic-type-impl
      
      #:get-custom-gloss-key-reports
      (fn self a
        (dissect self (info-wrapper-dynamic-type any-dt)
        /expect a (info-wrapper a-value)
          (uninformative-custom-gloss-key-reports)
        /info-wrapper-custom-gloss-key-reports-from-value-reports
          (dynamic-type-get-custom-gloss-key-reports
            any-dt a-value)))
      
      ))
  
  )

(define/own-contract (gloss-ref g k)
  (-> gloss? any/c any/c)
  (expect (gloss-ref-maybe-knowable g k) (known result)
    (raise-arguments-error 'gloss-ref
      "tried to get a key that couldn't be verified equivalent to or distinct from all the existing keys"
      "gloss" g
      "key" k)
  /expect result (just result)
    (raise-arguments-error 'gloss-ref
      "no value found for key"
      "gloss" g
      "key" k)
    result))

(define/own-contract (gloss-set g k v)
  (-> gloss? any/c any/c gloss?)
  (expect (gloss-set-maybe-knowable g k (just v)) (known result)
    (raise-arguments-error 'gloss-set
      "tried to set a key that couldn't be verified distinct from all the existing keys"
      "gloss" g
      "key" k
      "value" v)
    result))

(define/own-contract (make-gloss assocs)
  (-> (listof pair?) gloss?)
  (list-foldl (gloss-union-of-zero) assocs /fn g assoc
    (dissect assoc (cons k v)
    /gloss-set g k v)))

(define/own-contract (gloss-keys g)
  (-> gloss? (sequence/c any/c))
  (sequence-map (fn k v k) /gloss-iteration-sequence g))

; This is an appropriate dynamic type of `gloss?` values,
; information-ordered in a way that's consistent with `chaperone-of?`
; as long as the keys' and values' information orderings are. This is
; an instance of
; `make-expressly-smooshable-dynamic-type-impl-from-equal-always-list-isomorphism`.
; Note that this instance's
; `inhabitant-shallowly-equal-always?-knowable` can result in a
; non-`known?` value if any key comparison does.
;
(define-imitation-simple-struct
  (gloss-dynamic-type? gloss-dynamic-type-get-any-dynamic-type)
  gloss-dynamic-type
  'gloss-dynamic-type (current-inspector) (auto-write)
  
  (#:prop
    (make-expressly-smooshable-bundle-property-from-list-isomorphism
      #:ignore-chaperones? #t
      
      #:self-get-any-dynamic-type
      (dissectfn (gloss-dynamic-type any-dt)
        any-dt)
      
      #:inhabitant? gloss?
      
      #:->->list
      (fn a
        (w- keys (sequence->list /gloss-keys a)
        /fn b
          (append* /for/list ([k (in-list keys)])
            (list k (gloss-ref b k)))))
      
      #:example-and-list->
      (fn example lst
        (make-gloss
          (for/list ([entry (in-slice 2 (in-list lst))])
            (dissect entry (list k v)
            /cons k v))))
      
      #:combine-element-hash-codes
      (fn element-hash-codes
        (hash-code-combine-unordered*
          (for/list ([entry (in-slice 2 (in-list element-hash-codes))])
            (dissect entry (list k v)
            /hash-code-combine k v))))
      
      #:inhabitant-shallowly-equal-always?-knowable
      (fn a b
        (gloss-equal-always?-knowable a b /fn a b /known #t))
      
      )
    (trivial))
  
  )

; Level 0+:
;   <=, >=, path-related, join, meet, ==:
;     If the operands are not both
;     `dynamic-type-var-for-any-dynamic-type?` values, then
;     unknown.
;     
;     Otherwise, the first operand (or, for a check, `#t`).
;
(define-imitation-simple-struct
  (dynamic-type-for-dynamic-type-var-for-any-dynamic-type?)
  dynamic-type-for-dynamic-type-var-for-any-dynamic-type
  'dynamic-type-for-dynamic-type-var-for-any-dynamic-type
  (current-inspector)
  (auto-write)
  
  (#:prop
    (make-expressly-smooshable-bundle-property-for-atom
      #:ignore-chaperones? #t
      #:inhabitant? dynamic-type-var-for-any-dynamic-type?)
    (trivial)))

; Level 0:
;   <=, >=, path-related, join, meet:
;     If the operands are not both `equal-always-wrapper?` values,
;     then unknown.
;     
;     Otherwise, if the operands are `equal-always?`, then the first
;     operand (or, for a check, `#t`).
;     
;     Otherwise, unknown.
;   ==:
;     If the operands are not both `equal-always-wrapper?` values,
;     then unknown.
;     
;     Otherwise, if the operands are `equal-always?`, then the first
;     operand.
;     
;     Otherwise, a known nothing.
; Level 1+:
;   path-related, join, meet, ==:
;     Same as the description of level 0 ==.
;   <=, >=:
;     Same as the description of level 0 == as a check.
;
(define-imitation-simple-struct (equal-always-wrapper-dynamic-type?)
  equal-always-wrapper-dynamic-type
  'equal-always-wrapper-dynamic-type (current-inspector) (auto-write)
  
  (#:prop
    (make-expressly-smooshable-bundle-property-for-atom
      #:ignore-chaperones? #t
      #:inhabitant? equal-always-wrapper?)
    (trivial)))

(define/own-contract
  (on-indistinct-wrapper-smoosh-result-knowable-promise-maybe-knowable-promise
    kpmkp)
  (->
    (promise/c (knowable/c (maybe/c (promise/c (knowable/c pair?)))))
    (promise/c (knowable/c (maybe/c (promise/c (knowable/c pair?))))))
  (promise-map kpmkp /fn kpmk
    (knowable-map kpmk /fn kpm
      (maybe-map kpm /fn kp
        (promise-map kp /fn k
          (knowable-map k /fn result-value
            (indistinct-wrapper result-value)))))))

(define/own-contract (on-indistinct-wrapper-hash-code-promise p)
  (-> (promise/c fixnum?) (promise/c fixnum?))
  (promise-map p /fn value-hash-code
    (hash-code-combine
      (equal-always-hash-code indistinct-wrapper?)
      value-hash-code)))

(define/own-contract
  (indistinct-wrapper-smoosh-reports-from-value-reports value-reports)
  (-> (sequence/c smoosh-report?) (sequence/c smoosh-report?))
  (smoosh-reports-with-hesitation-at-discrepancies
    #:known-distinct? #f
    (smoosh-reports-map value-reports
      #:on-smoosh-result-knowable-promise-maybe-knowable-promise
      on-indistinct-wrapper-smoosh-result-knowable-promise-maybe-knowable-promise)))

(define/own-contract
  (indistinct-wrapper-smoosh-and-comparison-of-two-reports-from-value-reports
    value-reports)
  (-> (sequence/c smoosh-and-comparison-of-two-report?)
    (sequence/c smoosh-and-comparison-of-two-report?))
  (smoosh-and-comparison-of-two-reports-with-hesitation-at-discrepancies
    #:known-distinct? #f
    (smoosh-and-comparison-of-two-reports-map value-reports
      #:on-smoosh-result-knowable-promise-maybe-knowable-promise
      on-indistinct-wrapper-smoosh-result-knowable-promise-maybe-knowable-promise)))

(define/own-contract
  (indistinct-wrapper-smoosh-equal-hash-code-support-reports-from-value-reports
    value-reports)
  (-> (sequence/c smoosh-equal-hash-code-support-report?)
    (sequence/c smoosh-equal-hash-code-support-report?))
  (smoosh-equal-hash-code-support-reports-map value-reports
    #:on-hash-code-promise on-indistinct-wrapper-hash-code-promise))

(define/own-contract
  (indistinct-wrapper-custom-gloss-key-reports-from-value-reports
    value-reports)
  (-> (sequence/c custom-gloss-key-report?)
    (sequence/c custom-gloss-key-report?))
  (custom-gloss-key-reports-map value-reports
    #:on-tagged-glossesque-sys-knowable
    (fn tgs-k
      (knowable-map tgs-k
        (dissectfn (tagged-glossesque-sys inhabitant? gs)
          (tagged-glossesque-sys
            (makeshift-knowable-predicate /fn v
              (expect v (indistinct-wrapper v) (unknown)
              /call-knowable inhabitant? v))
            (indistinct-glossesque-sys
              (glossesque-sys-map-key gs #:granted-key-knowable /fn k
                (expect k (indistinct-wrapper k) (unknown)
                /known k)))))))))

; Level 0+:
;   <=, >=, path-related, join, meet, ==:
;     If the operands are not both `indistinct-wrapper?` values, then
;     unknown.
;     
;     Otherwise, if the result of processing the unwrapped values with
;     the same smoosh or check is a known success and any operand's
;     unwrapped value is `eq?` to it, then that operand (or, for a
;     check, `#t`).
;     
;     Otherwise, if it's a known success, that value wrapped up as an
;     `indistinct-wrapper?` value (or, for a check, `#t`).
;     
;     Otherwise, unknown.
;
(define-imitation-simple-struct
  (indistinct-wrapper-dynamic-type?
    indistinct-wrapper-dynamic-type-any-dynamic-type)
  indistinct-wrapper-dynamic-type
  'indistinct-wrapper-dynamic-type (current-inspector)
  (auto-write)
  (auto-equal)
  
  (#:prop prop:expressly-smooshable-dynamic-type
    (make-expressly-smooshable-dynamic-type-impl
      
      #:get-smoosh-of-zero-reports
      (fn self
        (dissect self (indistinct-wrapper-dynamic-type any-dt)
        /indistinct-wrapper-smoosh-reports-from-value-reports
          (dynamic-type-get-smoosh-of-zero-reports any-dt)))
      
      #:get-smoosh-of-one-reports
      (fn self a
        (dissect self (indistinct-wrapper-dynamic-type any-dt)
        /expect a (indistinct-wrapper a-value)
          (uninformative-smoosh-reports)
        /indistinct-wrapper-smoosh-reports-from-value-reports
          (dynamic-type-get-smoosh-of-one-reports any-dt a-value)))
      
      #:get-smoosh-and-comparison-of-two-reports
      (fn self a b
        (dissect self (indistinct-wrapper-dynamic-type any-dt)
        /expect a (indistinct-wrapper a-value)
          (uninformative-smoosh-and-comparison-of-two-reports)
        /expect b (indistinct-wrapper b-value)
          (uninformative-smoosh-and-comparison-of-two-reports)
        /indistinct-wrapper-smoosh-and-comparison-of-two-reports-from-value-reports
          (dynamic-type-get-smoosh-and-comparison-of-two-reports
            any-dt a-value b-value)))
      
      #:get-smoosh-and-comparison-of-two-reports-via-second
      (fn self a b
        (dissect self (indistinct-wrapper-dynamic-type any-dt)
        /expect a (indistinct-wrapper a-value)
          (uninformative-smoosh-and-comparison-of-two-reports)
        /expect b (indistinct-wrapper b-value)
          (uninformative-smoosh-and-comparison-of-two-reports)
        /indistinct-wrapper-smoosh-and-comparison-of-two-reports-from-value-reports
          (dynamic-type-get-smoosh-and-comparison-of-two-reports-via-second
            any-dt a-value b-value)))
      
      ))
  
  (#:prop prop:expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type
    (make-expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type-impl
      
      #:get-smoosh-equal-hash-code-support-reports
      (fn self a
        (dissect self (indistinct-wrapper-dynamic-type any-dt)
        /expect a (indistinct-wrapper a-value)
          (uninformative-smoosh-equal-hash-code-support-reports)
        /indistinct-wrapper-smoosh-equal-hash-code-support-reports-from-value-reports
          (dynamic-type-get-smoosh-equal-hash-code-support-reports
            any-dt a-value)))
      
      ))
  
  (#:prop prop:expressly-custom-gloss-key-dynamic-type
    (make-expressly-custom-gloss-key-dynamic-type-impl
      
      #:get-custom-gloss-key-reports
      (fn self a
        (dissect self (indistinct-wrapper-dynamic-type any-dt)
        /expect a (indistinct-wrapper a-value)
          (uninformative-custom-gloss-key-reports)
        /indistinct-wrapper-custom-gloss-key-reports-from-value-reports
          (dynamic-type-get-custom-gloss-key-reports
            any-dt a-value)))
      
      ))
  
  )

(match-define
  (list
    known-to-lathe-comforts-data?
    known-to-lathe-comforts-data-dynamic-type)
  (dynamic-type-case-by-cases
    'known-to-lathe-comforts-data-dynamic-type
    #:known-distinct? #f
    (list
      (list
        base-readable?
        (fn any-dt /base-readable-dynamic-type any-dt))
      (dynamic-type-case-by-cases 'maybe-dynamic-type /list
        (list nothing? (fn any-dt /nothing-dynamic-type))
        (list just? (fn any-dt /just-dynamic-type any-dt)))
      (list trivial? (fn any-dt /trivial-dynamic-type))
      (list
        (fn v /or (known? v) (example-unknown? v))
        (fn any-dt /knowable-dynamic-type any-dt))
      (list
        path-related-wrapper?
        (fn any-dt /path-related-wrapper-dynamic-type any-dt))
      (list
        info-wrapper?
        (fn any-dt /info-wrapper-dynamic-type any-dt))
      (list gloss? (fn any-dt /gloss-dynamic-type any-dt))
      (list
        dynamic-type-var-for-any-dynamic-type?
        (fn any-dt
          (dynamic-type-for-dynamic-type-var-for-any-dynamic-type)))
      (list
        equal-always-wrapper?
        (fn any-dt /equal-always-wrapper-dynamic-type))
      (list
        indistinct-wrapper?
        (fn any-dt /indistinct-wrapper-dynamic-type any-dt)))))

(define-imitation-simple-struct (any-dynamic-type?) any-dynamic-type
  'any-dynamic-type (current-inspector) (auto-write)
  
  (#:prop prop:expressly-smooshable-dynamic-type
    (make-expressly-smooshable-dynamic-type-impl
      
      #:get-smoosh-of-zero-reports
      (fn self
        (uninformative-smoosh-reports))
      
      #:get-smoosh-of-one-reports
      (fn self a
        (w- a-dt (get-dynamic-type-with-default-bindings a)
        /dynamic-type-get-smoosh-of-one-reports a-dt a))
      
      #:get-smoosh-and-comparison-of-two-reports
      (fn self a b
        (w- a-dt (get-dynamic-type-with-default-bindings a)
        /smoosh-and-comparison-of-two-reports-join /list
          (dynamic-type-get-smoosh-and-comparison-of-two-reports
            a-dt a b)
          (dynamic-type-get-smoosh-and-comparison-of-two-reports-via-second
            self a b)))
      
      #:get-smoosh-and-comparison-of-two-reports-via-second
      (fn self a b
        (w- b-dt (get-dynamic-type-with-default-bindings b)
        /dynamic-type-get-smoosh-and-comparison-of-two-reports-via-second
          b-dt a b))
      
      ))
  
  (#:prop prop:expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type
    (make-expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type-impl
      
      #:get-smoosh-equal-hash-code-support-reports
      (fn self a
        (w- a-dt (get-dynamic-type-with-default-bindings a)
        /dynamic-type-get-smoosh-equal-hash-code-support-reports a-dt a))
      
      ))
  
  (#:prop prop:expressly-custom-gloss-key-dynamic-type
    (make-expressly-custom-gloss-key-dynamic-type-impl
      
      #:get-custom-gloss-key-reports
      (fn self a
        (w- a-dt (get-dynamic-type-with-default-bindings a)
        /dynamic-type-get-custom-gloss-key-reports a-dt a))
      
      ))
  
  )
(ascribe-own-contract any-dynamic-type? (-> any/c boolean?))


; TODO SMOOSH: Implement a `glossesque-sys?` based on AVL trees, for
; the sake of types which are hard to write hash code behavior for but
; which do admit a total ordering. This would justify some of the
; generality of the design here; without types like this, it's a
; wonder we don't just hash everything.

; TODO SMOOSH: Implement smooshing, better `gen:equal-mode+hash`
; equality,
; `prop:expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type`
; hashing, and `prop:expressly-custom-gloss-key-dynamic-type` behavior
; for these types:
;
;   - Various types that can result from the default Racket reader, as
;     well as their corresponding mutable types where these exist.
;     We're referring to these as `base-readable?` values. The
;     following are distinct from each other, a choice which aims to
;     clarify which parts of an s-expression's data should be
;     inspectable by a typical parser and which parts should be
;     considered subject to more unsensational changes (such as
;     replacing strings with normalized strings):
;
;     - (Done) Mutable strings, mutable byte strings, mutable boxes,
;       mutable vectors, prefab structs with mutable fields, and
;       mutable hash tables, all equatable and distinguishable in a
;       way consistent with `equal-always?` and information-ordered in
;       a way consistent with `chaperone-of?`. (TODO SMOOSH: Is there
;       a way we can define these to be non-overlapping even with
;       user-defined types?)
;
;     - (Done) Flvectors and fxvectors, all equatable and
;       distinguishable in a way consistent with `eq?`. (TODO: As of
;       Racket 8.12 [cs], the implementation of `equal-always?` for
;       flvectors and fxvectors is incorrect. Once we're on 8.13 or
;       so, simplify the design by grouping these with the other
;       mutable data structures.)
;
;     - (Done) Symbols and keywords, which are all known to be
;       distinct from each other. They're not known to be ordered, not
;       even by `symbol<?` or `keyword<?`; symbols and keywords are
;       usually used as though they're entirely separate formal
;       concepts given meaning by potentially separate programmers who
;       haven't coordinated to know whether their respective
;       vocabularies have concepts in common with different spellings.
;       However, symbols and keywords are used specifically for
;       syntax, so their spellings are a primary aspect of their
;       meaning, and the comparison of those spellings is well-defined
;       (a comparison of Unicode scalar sequences).
;
;     - (Done) Empty lists.
;
;     - (Done) Cons cells, ordered according to the elements'
;       orderings and in a way that's consistent with a
;       `chaperone-of?` information ordering if the elements'
;       orderings are.
;
;     - Other literal datums. The following cases are not necessarily
;       nonoverlapping, and smooshing one with another will have
;       unknown results:
;
;       - (Done) Booleans, which are known to be distinct from each
;         other. They're not known to be ordered, not even by the
;         convention from abstract algebra that `#f` < `#t`; booleans
;         are often used in programming as a convenient stand-in for a
;         two-valued enum type which has no defined ordering.
;
;       - (Done) Numbers with NaN parts, not even known to be equal to
;         themselves.
;
;       - (Done) Numbers with no NaN parts, with the ones whose
;         imaginary parts are `=` zero being ordered in a way that's
;         consistent with `<=` and `=` on their real parts.
;
;       - (Done) NaN extflonums, not even known to be equal to
;         themselves.
;
;       - (Done) Non-NaN extflonums, ordered in a way consistent with
;         `extfl<=` and `extfl=`, and information-ordered in a way
;         consistent with `chaperone-of?` (which, perhaps bizarrely,
;         is consistent with `extfl=` and thus does not distinguish
;         between `-0.0t0` and `0.0t0` on the extflonum-supporting BC
;         platform, if the documentation is to be believed). (TODO:
;         Investigate this.)
;
;       - (Done) Characters, immutable strings, and immutable byte
;         strings, which are known to be equal to themselves when
;         they're `equal-always?` but not known to be distinct from
;         each other. There are many possible ways to normalize and
;         collate Unicode strings, and the only thing that's
;         necessarily obvious across all of those is that identical
;         Unicode scalar sequences are equal. Byte strings may seem
;         easier to justify distinguishing from each other according
;         to a byte-by-byte comparison, but the reader gives them a
;         text representation that a programmer may refactor into
;         similar Unicode text without realizing they've made a
;         change.
;
;       - (Done) Regular expressions (`regexp?`) and compiled code
;         expressions (`compiled-expression?`), not even known to be
;         equal to themselves. Comparing them by their exact source
;         code isn't necessarily consistent with the intent of the
;         programmer who wrote that code, and comparing them by their
;         behavior isn't necessarily feasible.
;
;       - (Done) Immutable boxes, ordered according to the elements'
;         orderings and in a way that's consistent with a
;         `chaperone-of?` information ordering if the elements'
;         orderings are.
;
;       - (Done) Immutable vectors, ordered according to the elements'
;         orderings and in a way that's consistent with a
;         `chaperone-of?` information ordering if the elements'
;         orderings are. Vectors of different lengths are known to be
;         distinct from each other.
;
;       - (Done) Prefab structs with no mutable fields, ordered
;         according to the elements' orderings and in a way that's
;         consistent with a `chaperone-of?` information ordering if
;         the elements' orderings are. Prefab structs with different
;         keys and/or different numbers of fields are known to be
;         distinct from each other.
;
;       - (Done) Immutable hash tables with various comparison
;         functions, ordered according to the keys' and values'
;         orderings and in a way that's consistent with a 
;         `chaperone-of?` information ordering if the elements'
;         orderings are. Hash tables which use different comparison
;         functions or which have different sets of keys according to
;         their comparison functions are known to be distinct from
;         each other.
;
;     - Potentially others in future versions of Racket. The above
;       list is up-to-date as of Racket 8.12.
;
;   - Types defined by Lathe Comforts that this smooshing framework
;     uses. The following are indistinct from each other and from the
;     other types listed above and below:
;
;     - (Done) `maybe?` values, ordered according to the elements'
;       orderings and in a way that's consistent with a
;       `chaperone-of?` information ordering if the elements'
;       orderings are. A `just?` value and a `nothing?` value are
;       known to be distinct from each other.
;
;     - (Done) `trivial?` values.
;
;   - Types defined here in smoosh.rkt. The following are indistinct
;     from each other and from the other types listed above and below:
;
;     - (Done) `known?` values, `example-unknown?` values, and their
;       interactions with other `unknown?` values. The `known?` values
;       are ordered according to the elements' orderings and in a way
;       that's consistent with a `chaperone-of?` information ordering
;       if the elements' orderings are. In their level-0 ordering, any
;       smoosh involving an `unknown?` value is unknown. In their
;       information ordering, any `known?` value is considered greater
;       than any `unknown?` value, and any `example-unknown?` value is
;       considered equal to any `unknown?` value.
;
;     - (Done) `path-related-wrapper?` values, ordered according to
;       whether elements are path-related according to the "any"
;       type's smoosh ordering.
;
;     - (Done) `info-wrapper?` values, ordered according to whether
;       elements are related according to the "any" type's information
;       ordering.
;
;     - (Done) `gloss?` values, ordered according to the keys' and
;       values' smoosh orderings. `gloss?` values which have
;       known-different sets of keys according to smoosh-ordering are
;       known to be distinct from each other.
;
;     - (Done) `dynamic-type-var-for-any-dynamic-type?`
;
;     - (Done) `equal-always-wrapper?` values, all equatable and
;       distinguishable with each other according to the
;       `equal-always?` behavior of their wrapped value.
;
;     - (Done) `indistinct-wrapper?` values, ordered and
;       information-ordered in a way that's consistent with the
;       ordering and information ordering on the wrapped value but
;       treats known false results as though they were unknown
;       results.
;
;     - Perhaps the types of types, ideally allowing an expressive
;       subset of types of types to be related by subtyping, namely
;       when they don't have identities with meaningful details
;       independent of the set of inhabitants they have. This
;       smooshability would be relevant mainly when types appear as
;       elements of data structures that would be otherwise
;       smooshable.
;
;       - `uninformative-dynamic-type?`
;
;       - `flvector-dynamic-type?`
;
;       - `fxvector-dynamic-type?`
;
;       - `base-syntactic-atom-dynamic-type?`
;
;       - `boolean-dynamic-type?`
;
;       - `char-dynamic-type?`
;
;       - `immutable-string-dynamic-type?`
;
;       - `immutable-bytes-dynamic-type?`
;
;       - `non-nan-number-dynamic-type?`
;
;       - `non-nan-extflonum-dynamic-type?`
;
;       - `cons-dynamic-type?`
;
;       - `immutable-vector-dynamic-type?`
;
;       - `base-mutable-readable-dynamic-type?`
;
;       - `immutable-box-dynamic-type?`
;
;       - `immutable-prefab-struct-dynamic-type?`
;
;       - `immutable-hash-dynamic-type?`
;
;       - `base-literal-dynamic-type?` (the type belonging to
;         `base-literal-dynamic-type-case`)
;
;       - `base-readable-dynamic-type?` (the type constructed by
;         `base-readable-dynamic-type`)
;
;       - `nothing-dynamic-type?`
;
;       - `just-dynamic-type?`
;
;       - `knowable-dynamic-type?`
;
;       - `path-related-wrapper-dynamic-type?`
;
;       - `info-wrapper-dynamic-type?`
;
;       - `gloss-dynamic-type?`
;
;       - `dynamic-type-for-dynamic-type-var-for-any-dynamic-type?`
;
;       - `equal-always-wrapper-dynamic-type?`
;
;       - `indistinct-wrapper-dynamic-type?`
;
;       - `maybe-dynamic-type?` (a type used in an intermediate way in
;         the definition of
;         `known-to-lathe-comforts-data-dynamic-type`)
;
;       - `known-to-lathe-comforts-data-dynamic-type?` (the type
;         constructed by `known-to-lathe-comforts-data-dynamic-type`)
;
;       - `any-dynamic-type?`
;
;   - Types defined by Lathe Comforts even if this smooshing framework
;     doesn't use them. The following are indistinct from each other
;     and from the other types listed above:
;
;     - `obstinacy?`, for instance. Potentially others; we haven't
;       made a comprehensive list here yet.

; NOTE:
;
; We have been implementing smooshability as though values return
; unknown results when smooshed with values they don't recognize.
;
; The
; `prop:expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type`
; interface basically requires specifying hash code results associated
; with `==` and `path-related` smooshes at all information-ordering
; levels of a smoosh report sequence, all just so that `gloss?` values
; can have useful `equal-always-hash-code` results.
;
; Since a lot of values have unknown equality with each other as far
; as smooshability is concerned, the way we expect to treat these hash
; codes is that they'll only try to distinguish values from other
; values their author knows them to be distinct from. If the user
; extends this knowledge with the additional knowledge that certain
; values from different authors are actually equal after all, then the
; user will have to *opt out* of those authors' supplied hash code
; behaviors, or the user will have to carefully keep their hash table
; entries separated across multiple hash tables depending on details
; about the key's provenance, which the user might prefer to forget.
;
; We don't go to the trouble to allow users to opt out of the
; `prop:expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type`
; hash codes we specify yet; we just implement those hash codes the
; same way we would if all involved types were known to be distinct.
; That's because these are hash codes meant for reasonable coexistence
; with Racket's `equal?`- and `equal-always?`-based hashes, where the
; notion that two values may have an unknown comparison result doesn't
; really exist. When a user wants to work with the possibility that
; comparison results are unknown, we offer `gloss?` values as our
; recommended replacement for `hash?` values.

; TODO SMOOSH: Consider getting rid of the
; `glossesque-sys-glossesque-skm-union-of-two-knowable` method because
; it doesn't seem like it would help make merging glosses more
; efficient.
