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

(require /only-in lathe-comforts dissect expect fn mat w-)
(require /only-in lathe-comforts/hash
  hash-kv-map hash-ref-maybe hash-set-maybe)
(require /only-in lathe-comforts/struct
  auto-equal auto-write define-imitation-simple-generics
  define-imitation-simple-struct)
(require /only-in lathe-comforts/match match/c)
(require /only-in lathe-comforts/maybe just maybe? maybe/c nothing)


(provide /own-contract-out
  possibly-unknown-impl?
  prop:possibly-unknown
  make-possibly-unknown-impl
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
  knowable->falsable
  falsable->uninformative-knowable
  knowable-predicate-impl?
  prop:knowable-predicate
  make-knowable-predicate-impl
  call-knowable
  make-knowable-predicate-procedure-impl
  glossesque-sys?
  glossesque-sys-impl?
  glossesque-sys-glossesque-union-of-zero
  glossesque-sys-glossesque-km-union-of-two
  glossesque-sys-glossesque-ref-maybe-knowable
  glossesque-sys-glossesque-set-maybe-knowable
  glossesque-sys-glossesque-count
  glossesque-sys-glossesque-iteration-sequence
  prop:glossesque-sys
  ; TODO: When we document this, make sure we document its keyword
  ; arguments.
  make-glossesque-sys-impl
  equalw-gloss-key?
  equalw-gloss-key-impl?
  prop:equalw-gloss-key
  make-equalw-gloss-key-impl
  custom-gloss-key?
  custom-gloss-key-impl?
  custom-gloss-key-variant
  custom-gloss-key-glossesque-sys
  prop:custom-gloss-key
  make-custom-gloss-key-impl
  gloss?
  gloss-union-of-zero
  gloss-km-union-of-two
  gloss-ref-maybe-knowable
  gloss-set-maybe-knowable
  gloss-count
  gloss-iteration-sequence
  make-gloss-glossesque-sys
  expressly-has-dynamic-type-impl?
  prop:expressly-has-dynamic-type
  make-expressly-has-dynamic-type-impl
  make-uninformative-dynamic-type
  get-dynamic-type
  smoosh-report?
  smoosh-report-impl?
  smoosh-report-join-knowable-promise-maybe-knowable-promise
  smoosh-report-meet-knowable-promise-maybe-knowable-promise
  smoosh-report-==-knowable-promise-maybe-knowable-promise
  smoosh-report-path-related-knowable-promise-maybe-knowable-promise
  prop:smoosh-report
  make-smoosh-report-impl
  smoosh-and-comparison-of-two-report?
  smoosh-and-comparison-of-two-report-impl?
  smoosh-and-comparison-of-two-report-<=?-knowable-promise
  smoosh-and-comparison-of-two-report->=?-knowable-promise
  smoosh-and-comparison-of-two-report-get-smoosh-report
  prop:smoosh-and-comparison-of-two-report
  make-smoosh-and-comparison-of-two-report-impl)


(define-imitation-simple-generics
  possibly-unknown? possibly-unknown-impl?
  (#:method possibly-unknown-is-indeed? (#:this))
  prop:possibly-unknown make-possibly-unknown-impl
  'possibly-unknown 'possibly-unknown-impl (list))
(ascribe-own-contract possibly-unknown-impl? (-> any/c boolean?))
(ascribe-own-contract prop:possibly-unknown
  (struct-type-property/c possibly-unknown-impl?))
(ascribe-own-contract make-possibly-unknown-impl
  (-> (-> any/c boolean?) possibly-unknown-impl?))


(define/own-contract (unknown? v)
  (-> any/c boolean?)
  (and (possibly-unknown? v) (possibly-unknown-is-indeed? v)))

(define-imitation-simple-struct (example-unknown?) example-unknown
  'unknown (current-inspector) (auto-write))

(define/own-contract (unknown)
  (-> unknown?)
  (example-unknown))

; TODO: Give this better smooshing behavior once we have an interface
; for smooshable things.
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

(define/own-contract (knowable->falsable kble)
  (-> knowable? any/c)
  (mat kble (known value)
    value
    #f))

(define/own-contract (falsable->uninformative-knowable fble)
  (-> any/c knowable?)
  (if fble
    (known fble)
    (unknown)))


(define-imitation-simple-generics
  knowable-predicate? knowable-predicate-impl?
  (#:method knowable-predicate-get-accepts?-knowable (#:this))
  prop:knowable-predicate make-knowable-predicate-impl
  'knowable-predicate 'knowable-predicate-impl (list))
(ascribe-own-contract knowable-predicate-impl? (-> any/c boolean?))
(ascribe-own-contract prop:knowable-predicate
  (struct-type-property/c knowable-predicate-impl?))
(ascribe-own-contract make-knowable-predicate-impl
  (-> (-> knowable-predicate? (unconstrained-domain-> knowable?))
    knowable-predicate-impl?))

(define/own-contract call-knowable
  (unconstrained-domain-> knowable?)
  (procedure-reduce-arity
    (make-keyword-procedure
      (lambda (ks vs f . positional-args)
        (if (knowable-predicate? f)
          (w- accepts?-knowable
            (knowable-predicate-get-accepts?-knowable f)
          /keyword-apply accepts?-knowable ks vs positional-args)
        /if (procedure? f)
          (falsable->uninformative-knowable
            (keyword-apply f ks vs positional-args))
          (raise-arguments-error 'call-knowable
            "expected the called value to be prop:knowable-predicate instance or a procedure"
            "f" f)))
      (lambda (f . positional-args)
        (if (knowable-predicate? f)
          (w- accepts?-knowable
            (knowable-predicate-get-accepts?-knowable f)
          /apply accepts?-knowable positional-args)
        /if (procedure? f)
          (falsable->uninformative-knowable /apply f positional-args)
          (raise-arguments-error 'call-knowable
            "expected the called value to be prop:knowable-predicate instance or a procedure"
            "f" f))))
    (arity-at-least 1)))

; Returns a value that makes an appropriate `prop:procedure`
; implementation for a structure type that implements
; `prop:knowable-predicate`. It will often be preferable to pass this
; result through `procedure-reduce-arity`.
;
(define/own-contract (make-knowable-predicate-procedure-impl)
  (-> (unconstrained-domain-> any/c))
  (compose knowable->falsable call-knowable))

; TODO SMOOSH: Define at least a couple of examples of
; `knowable-predicate?` values, so that we know if we need another
; utility.


(define-imitation-simple-generics
  glossesque-sys? glossesque-sys-impl?
  (#:method glossesque-sys-glossesque-union-of-zero (#:this))
  (#:method
    glossesque-sys-glossesque-km-union-of-two (#:this) () () ())
  (#:method
    glossesque-sys-glossesque-ref-maybe-knowable (#:this) () ())
  (#:method
    glossesque-sys-glossesque-set-maybe-knowable (#:this) () () ())
  (#:method glossesque-sys-glossesque-count (#:this) ())
  (#:method glossesque-sys-glossesque-iteration-sequence (#:this) ())
  prop:glossesque-sys
  make-glossesque-sys-impl-from-various-unkeyworded
  'glossesque-sys 'glossesque-sys-impl (list))
(ascribe-own-contract glossesque-sys? (-> any/c boolean?))
(ascribe-own-contract glossesque-sys-impl? (-> any/c boolean?))
(ascribe-own-contract glossesque-sys-glossesque-union-of-zero
  (-> glossesque-sys? any/c))
(ascribe-own-contract glossesque-sys-glossesque-km-union-of-two
  (-> glossesque-sys? any/c any/c (-> any/c maybe? maybe? maybe?)
    any/c))
(ascribe-own-contract glossesque-sys-glossesque-ref-maybe-knowable
  (-> glossesque-sys? any/c any/c (knowable/c maybe?)))
(ascribe-own-contract glossesque-sys-glossesque-set-maybe-knowable
  (-> glossesque-sys? any/c any/c maybe? (knowable/c any/c)))
(ascribe-own-contract glossesque-sys-glossesque-count
  (-> glossesque-sys? any/c natural?))
(ascribe-own-contract glossesque-sys-glossesque-iteration-sequence
  (-> glossesque-sys? any/c (sequence/c any/c any/c)))
(ascribe-own-contract prop:glossesque-sys
  (struct-type-property/c glossesque-sys-impl?))

(define/own-contract
  (make-glossesque-sys-impl
    #:glossesque-union-of-zero glossesque-union-of-zero
    #:glossesque-km-union-of-two glossesque-km-union-of-two
    #:glossesque-ref-maybe-knowable glossesque-ref-maybe-knowable
    #:glossesque-set-maybe-knowable glossesque-set-maybe-knowable
    #:glossesque-count glossesque-count
    #:glossesque-iteration-sequence glossesque-iteration-sequence)
  (->
    #:glossesque-union-of-zero (-> glossesque-sys? any/c)
    
    #:glossesque-km-union-of-two
    (-> glossesque-sys? any/c any/c (-> any/c maybe? maybe? maybe?))
    
    #:glossesque-ref-maybe-knowable
    (-> glossesque-sys? any/c any/c (knowable/c maybe?))
    
    #:glossesque-set-maybe-knowable
    (-> glossesque-sys? any/c any/c maybe? (knowable/c any/c))
    
    #:glossesque-count (-> glossesque-sys? any/c natural?)
    
    #:glossesque-iteration-sequence
    (-> glossesque-sys? any/c (sequence/c any/c any/c))
    
    glossesque-sys-impl?)
  (make-glossesque-sys-impl-from-various-unkeyworded
    glossesque-union-of-zero
    glossesque-km-union-of-two
    glossesque-ref-maybe-knowable
    glossesque-set-maybe-knowable
    glossesque-count
    glossesque-iteration-sequence))


(define-imitation-simple-generics
  equalw-gloss-key? equalw-gloss-key-impl?
  prop:equalw-gloss-key make-equalw-gloss-key-impl
  'equalw-gloss-key 'equalw-gloss-key-impl (list))
(ascribe-own-contract equalw-gloss-key? (-> any/c boolean?))
(ascribe-own-contract equalw-gloss-key-impl? (-> any/c boolean?))
(ascribe-own-contract prop:equalw-gloss-key
  (struct-type-property/c equalw-gloss-key-impl?))
(ascribe-own-contract make-equalw-gloss-key-impl
  (-> equalw-gloss-key-impl?))

(define-imitation-simple-generics
  custom-gloss-key? custom-gloss-key-impl?
  (#:method custom-gloss-key-variant (#:this))
  (#:method custom-gloss-key-glossesque-sys (#:this))
  prop:custom-gloss-key
  make-custom-gloss-key-impl-from-various-unkeyworded
  'custom-gloss-key 'custom-gloss-key-impl (list))
(ascribe-own-contract custom-gloss-key? (-> any/c boolean?))
(ascribe-own-contract custom-gloss-key-impl? (-> any/c boolean?))
(ascribe-own-contract custom-gloss-key-variant
  (-> custom-gloss-key? any/c))
(ascribe-own-contract custom-gloss-key-glossesque-sys
  (-> custom-gloss-key? glossesque-sys?))
(ascribe-own-contract prop:custom-gloss-key
  (struct-type-property/c custom-gloss-key-impl?))

(define/own-contract
  (make-custom-gloss-key-impl
    #:variant variant
    #:glossesque-sys glossesque-sys)
  (->
    #:variant (-> custom-gloss-key? any/c)
    #:glossesque-sys (-> custom-gloss-key? glossesque-sys?)
    glossesque-sys-impl?)
  (make-custom-gloss-key-impl-from-various-unkeyworded
    variant
    glossesque-sys))


; TODO: Give this better smooshing behavior once we have an interface
; for smooshable things.
(define-imitation-simple-struct
  (gloss? gloss-count-field gloss-atomic-entries gloss-custom-entries)
  gloss
  'gloss (current-inspector)
  ; TODO SMOOSH: Stop using `auto-write` and `auto-equal` for this.
  (auto-write) (auto-equal))
(ascribe-own-contract gloss? (-> any/c boolean?))

(define (hash-km-union-of-two a b km-union)
  (hash-kv-map (hash-union a b #:combine /fn a b a) /fn k v
    (km-union k (hash-ref-maybe a k) (hash-ref-maybe b k))))

(define (maybe-m-union-of-two a b m-union)
  (mat a (just _) (m-union a b)
  /mat b (just _) (m-union a b)
  /nothing))

(define/own-contract (gloss-union-of-zero)
  (-> gloss?)
  (gloss 0 (hashalw) (nothing)))

(define/own-contract (gloss-km-union-of-two a b km-union)
  (-> gloss? gloss? (-> any/c maybe? maybe? maybe?) gloss?)
  (dissect a (gloss a-count a-atomic a-custom)
  /dissect b (gloss b-count b-atomic b-custom)
  /w- atomic (hash-km-union-of-two a-atomic b-atomic km-union)
  /w- custom
    (maybe-m-union-of-two a-custom b-custom /fn a b
      (gloss-km-union-of-two
        (mat a (just a) a (gloss-union-of-zero))
        (mat b (just b) b (gloss-union-of-zero))
        (fn a b
          (w- gs
            (mat a (just /list gs _) gs
            /dissect b (just /list gs _) gs)
          /w- a
            (mat a (just /list _ a) a
            /glossesque-sys-glossesque-union-of-zero gs)
          /w- b
            (mat b (just /list _ b) b
            /glossesque-sys-glossesque-union-of-zero gs)
          /just /list gs
            (glossesque-sys-glossesque-km-union-of-two gs a b
              km-union)))))
  /gloss
    (+ (hash-count atomic)
      (expect custom (just custom) 0
        (for/sum
          (
            [ (variant custom-entry)
              (in-sequences /gloss-iteration-sequence custom)])
          (dissect custom-entry (list gs g)
          /glossesque-sys-glossesque-count gs g))))
    atomic
    custom))

(define/own-contract (gloss-ref-maybe-knowable g k)
  (-> gloss? any/c (knowable/c maybe?))
  (dissect g (gloss _ atomic custom)
  /if (equalw-gloss-key? k) (known /hash-ref-maybe atomic k)
  /expect (custom-gloss-key? k) #t (unknown)
  /expect custom (just custom) (known /nothing)
  /knowable-bind
    (gloss-ref-maybe-knowable custom (custom-gloss-key-variant k))
  /fn custom-entry
  /expect custom-entry (just custom-entry) (known /nothing)
  /dissect custom-entry (list custom-gs custom-g)
  /glossesque-sys-glossesque-ref-maybe-knowable custom-gs custom-g k))

(define/own-contract (gloss-set-maybe-knowable g k m)
  (-> gloss? any/c maybe? (knowable/c gloss?))
  (dissect g (gloss count atomic custom)
  /if (equalw-gloss-key? k)
    (known /gloss count (hash-set-maybe atomic k m) custom)
  /expect (custom-gloss-key? k) #t (unknown)
  /expect custom (just custom)
    (gloss-set-maybe-knowable
      (gloss count atomic (just /gloss-union-of-zero))
      k
      m)
  /w- variant (custom-gloss-key-variant k)
  /knowable-bind (gloss-ref-maybe-knowable custom variant)
  /fn custom-entry
  /expect custom-entry (just custom-entry)
    (w- custom-gs (custom-gloss-key-glossesque-sys k)
    /knowable-bind
      (gloss-set-maybe-knowable custom variant
        (just /list custom-gs
          (glossesque-sys-glossesque-union-of-zero custom-gs)))
    /fn custom
    /gloss-set-maybe-knowable (gloss count atomic (just custom)) k m)
  /dissect custom-entry (list custom-gs custom-g)
  /w- old-custom-g-count
    (glossesque-sys-glossesque-count custom-gs custom-g)
  /knowable-bind
    (glossesque-sys-glossesque-set-maybe-knowable
      custom-gs custom-g k m)
  /fn custom-g
  /w- new-custom-g-count
    (glossesque-sys-glossesque-count custom-gs custom-g)
  /knowable-bind
    (gloss-set-maybe-knowable custom k
      (just /list custom-gs custom-g))
  /fn custom
  /known /gloss
    (+ count (- new-custom-g-count old-custom-g-count))
    atomic
    (just custom)))

(define/own-contract (gloss-count g)
  (-> gloss? natural?)
  (dissect g (gloss count _ _)
    count))

(define/own-contract (gloss-iteration-sequence g)
  (-> gloss? (sequence/c any/c any/c))
  (dissect g (gloss _ atomic custom)
  /apply in-sequences (in-hash atomic)
    (expect custom (just custom) (list)
    /for/list
      (
        [ (variant custom-entry)
          (in-sequences /gloss-iteration-sequence custom)])
      (dissect custom-entry (list gs g)
      /glossesque-sys-glossesque-iteration-sequence gs g))))

(define-imitation-simple-struct
  (gloss-glossesque-sys?)
  gloss-glossesque-sys
  'gloss-glossesque-sys (current-inspector) (auto-write) (auto-equal)
  
  (#:prop prop:glossesque-sys /make-glossesque-sys-impl
    
    #:glossesque-union-of-zero (fn gs /gloss-union-of-zero)
    
    #:glossesque-km-union-of-two
    (fn gs a b km-union /gloss-km-union-of-two a b km-union)
    
    #:glossesque-ref-maybe-knowable
    (fn gs g k /gloss-ref-maybe-knowable g k)
    
    #:glossesque-set-maybe-knowable
    (fn gs g k m /gloss-set-maybe-knowable g k m)
    
    #:glossesque-count (fn gs g /gloss-count g)
    
    #:glossesque-iteration-sequence
    (fn gs g /gloss-iteration-sequence g)
    
    )
  
  )

(define/own-contract (make-gloss-glossesque-sys)
  (-> glossesque-sys?)
  (gloss-glossesque-sys))


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

(define-imitation-simple-struct (uninformative-dynamic-type?)
  uninformative-dynamic-type
  'uninformative-dynamic-type (current-inspector) (auto-write))

(define/own-contract (make-uninformative-dynamic-type)
  (-> any/c)
  (uninformative-dynamic-type))

(define/own-contract (get-dynamic-type bindings v)
  (-> gloss? any/c any/c)
  (if (expressly-has-dynamic-type? v)
    (expressly-has-dynamic-type-get-dynamic-type bindings v)
    (make-uninformative-dynamic-type)))


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

(define-imitation-simple-generics smooshable-sys? smooshable-sys-impl?
  (#:method smooshable-sys-get-smoosh-of-zero-report (#:this))
  (#:method smooshable-sys-get-smoosh-of-one-report (#:this) ())
  (#:method smooshable-sys-get-smoosh-and-comparison-of-two-report
    (#:this)
    ()
    ()
    ())
  prop:smooshable-sys make-smooshable-sys-from-various-unkeyworded
  'smooshable-sys 'smooshable-sys-impl (list))
(ascribe-own-contract smooshable-sys? (-> any/c boolean?))
(ascribe-own-contract smooshable-sys-impl? (-> any/c boolean?))
(ascribe-own-contract smooshable-sys-get-smoosh-of-zero-report
  (-> smooshable-sys?
    ; Each report in the infinite sequence gives the smoosh identity
    ; elements, first for the type's bespoke notion of ordering, then
    ; for the information ordering, then for the information ordering
    ; of the information ordering representatives, and so on.
    (sequence/c smoosh-report?)))
(ascribe-own-contract smooshable-sys-get-smoosh-of-one-report
  (-> smooshable-sys? any/c
    ; Each report in the infinite sequence gives the smoosh identity
    ; elements, first for the type's bespoke notion of ordering, then
    ; for the information ordering, then for the information ordering
    ; of the information ordering representatives, and so on.
    (sequence/c smoosh-report?)))
(ascribe-own-contract
  smooshable-sys-get-smoosh-and-comparison-of-two-report
  (->
    ; lhs type
    smooshable-sys?
    ; rhs type (usually dispatched to next, if this one can't fully
    ; determine the results)
    smooshable-sys?
    ; lhs
    any/c
    ; rhs
    any/c
    ; For each report in the infinite sequence, the next report says
    ; not only whether they smoosh along that one's == but also, only
    ; if they do, how their information ordering representatives
    ; smoosh along their information ordering.
    (sequence/c smoosh-and-comparison-of-two-report?)))
(ascribe-own-contract prop:smooshable-sys
  (struct-type-property/c smooshable-sys-impl?))

(define/own-contract
  (make-smooshable-sys-impl
    #:get-smoosh-of-zero-report get-smoosh-of-zero-report
    #:get-smoosh-of-one-report get-smoosh-of-one-report
    
    #:get-smoosh-and-comparison-of-two-report
    get-smoosh-and-comparison-of-two-report)
  (->
    #:get-smoosh-of-zero-report
    (-> smooshable-sys? (sequence/c smoosh-report?))
    
    #:get-smoosh-of-one-report
    (-> smooshable-sys? any/c (sequence/c smoosh-report?))
    
    #:get-smoosh-and-comparison-of-two-report
    (-> smooshable-sys? smooshable-sys? any/c any/c
      (sequence/c smoosh-and-comparison-of-two-report?))
    
    smooshable-sys-impl?)
  (make-smooshable-sys-from-various-unkeyworded
    get-smoosh-of-zero-report
    get-smoosh-of-one-report
    get-smoosh-and-comparison-of-two-report))


; TODO SMOOSH: Implement the following parts of the API outlined in
; the last part of notes/2024-03-20-squashable-object-system.txt:
;
; dynamic-type-var-for-any-dynamic-type
; smooshable-sys-get-info-smooshable-sys
; key-report?
; key-of-immutable-dict-sys?
; any-dynamic-type
; any-dynamic-type?
; make-empty-immutable-total-order-<=-based-dict
; make-empty-immutable-trie-dict
;
; We're not implementing the mutable dicts. (If we need them, we're
; just going to model them as mutable boxes containing immutable
; dicts, or tack on the mutable dict stuff as an afterthought.)
;
; We need to figure out what we're doing with
; information-ordering-indexed data structures. Since we've ended up
; having only one concrete `gloss?` type, it seems we need wrapper
; types for our key values to indicate when they should be compared
; according to path-relatedness or according to their information
; ordering. The `key-of-immutable-dict-sys-get-key-report` approach
; outlined in the notes gives each of those different forms of
; comparison its own empty dictionary constructor, which in our case
; would now be expressed as a distinct `glossesque-sys?`. Let's update
; `prop:custom-gloss-key` to return a `key-report?`. Maybe we should
; update the notes to call `key-of-immutable-dict-sys?`
; `custom-gloss-key?` and to call `label?` `equalw-gloss-key?`.
; However, `key-of-immutable-dict-sys?` is a quality of a dynamic
; type, while `custom-gloss-key?` is a quality of the value itself.
