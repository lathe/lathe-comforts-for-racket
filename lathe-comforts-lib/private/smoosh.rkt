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
  hash-kv-map-maybe hash-ref-maybe hash-set-maybe)
(require /only-in lathe-comforts/list list-any list-map list-zip-map)
(require /only-in lathe-comforts/struct
  auto-equal auto-write define-imitation-simple-generics
  define-imitation-simple-struct immutable-prefab-struct?
  mutable-prefab-struct?)
(require /only-in lathe-comforts/match match/c)
(require /only-in lathe-comforts/maybe
  just just? just-value maybe? maybe/c maybe-if maybe-map nothing)


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
  custom-gloss-key-report?
  custom-gloss-key-report-impl?
  custom-gloss-key-report-get-==-glossesque-sys
  custom-gloss-key-report-get-path-related-glossesque-sys
  prop:custom-gloss-key-report
  make-custom-gloss-key-report-impl
  path-related-wrapper
  info-wrapper
  equalw-gloss-key?
  equalw-gloss-key-impl?
  prop:equalw-gloss-key
  make-equalw-gloss-key-impl
  custom-gloss-key-dynamic-type?
  custom-gloss-key-dynamic-type-impl?
  custom-gloss-key-dynamic-type-variant-knowable
  custom-gloss-key-dynamic-type-get-reports-knowable
  prop:custom-gloss-key-dynamic-type
  make-custom-gloss-key-dynamic-type-impl
  get-dynamic-type-with-default-bindings
  gloss?
  gloss-union-of-zero
  gloss-km-union-of-two
  gloss-ref-maybe-knowable
  gloss-set-maybe-knowable
  gloss-count
  gloss-iteration-sequence
  make-gloss-glossesque-sys
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
  dynamic-type-get-smoosh-of-zero-report
  dynamic-type-get-smoosh-of-one-report
  dynamic-type-get-smoosh-and-comparison-of-two-report
  smoosh-and-comparison-of-two-report-flip
  smoosh-and-comparison-of-two-reports-flip
  smoosh-report-map
  smoosh-reports-map
  smoosh-report-zip-map
  smoosh-reports-zip-map
  smoosh-and-comparison-of-two-report-map
  smoosh-and-comparison-of-two-reports-map
  smoosh-and-comparison-of-two-report-zip-map
  smoosh-and-comparison-of-two-reports-zip-map
  false-smoosh-and-comparison-of-two-reports
  dead-end-dynamic-type
  constant-smoosh-report
  constant-smoosh-reports
  promise-map
  constant-smoosh-and-comparison-of-two-report
  constant-smoosh-and-comparison-of-two-reports
  compare-by-predicates-dynamic-type
  equalw-gloss-key-wrapper
  knowable-promise-zip-map
  boolean-and-knowable-promise-zip-map
  boolean-and-knowable-thunk-zip
  boolean-or-knowable-thunk-zip
  maybe-min-knowable-promise-zip-map
  sequence-zip-map
  knowable-promise-or
  smoosh-and-comparison-of-two-report-join
  smoosh-and-comparison-of-two-reports-join
  any-dynamic-type?)
(provide
  any-dynamic-type)


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
  custom-gloss-key-report? custom-gloss-key-report-impl?
  ; Returns a `glossesque-sys?` which compares keys by smooshing them
  ; along ==, i.e. (<= and >=).
  (#:method custom-gloss-key-report-get-==-glossesque-sys (#:this))
  ; Returns a `glossesque-sys?` which compares keys by smooshing them
  ; along the transitive closure of (<= or >=).
  (#:method custom-gloss-key-report-get-path-related-glossesque-sys
    (#:this))
  prop:custom-gloss-key-report
  make-custom-gloss-key-report-impl-from-various-unkeyworded
  'custom-gloss-key-report 'custom-gloss-key-report-impl (list))
(ascribe-own-contract custom-gloss-key-report? (-> any/c boolean?))
(ascribe-own-contract custom-gloss-key-report-impl? (-> any/c boolean?))
(ascribe-own-contract custom-gloss-key-report-get-==-glossesque-sys
  (-> custom-gloss-key-report? glossesque-sys?))
(ascribe-own-contract
  custom-gloss-key-report-get-path-related-glossesque-sys
  (-> custom-gloss-key-report? glossesque-sys?))
(ascribe-own-contract prop:custom-gloss-key-report
  (struct-type-property/c custom-gloss-key-report-impl?))

(define/own-contract
  (make-custom-gloss-key-report-impl
    #:get-==-glossesque-sys get-==-glossesque-sys
    
    #:get-path-related-glossesque-sys
    get-path-related-glossesque-sys)
  (->
    #:get-==-glossesque-sys
    (-> custom-gloss-key-report? glossesque-sys?)
    
    #:get-path-related-glossesque-sys
    (-> custom-gloss-key-report? glossesque-sys?)
    
    custom-gloss-key-report-impl?)
  (make-custom-gloss-key-report-impl-from-various-unkeyworded
    get-==-glossesque-sys
    get-path-related-glossesque-sys))

; TODO SMOOSH: Give this better smooshing behavior using
; `prop:expressly-smooshable-dynamic-type`.
(define-imitation-simple-struct
  (path-related-wrapper? path-related-wrapper-value)
  path-related-wrapper-unguarded
  'path-related-wrapper (current-inspector)
  ; TODO SMOOSH: Stop using `auto-write` and `auto-equal` for this.
  (auto-write) (auto-equal))

(define/own-contract (path-related-wrapper v)
  (-> any/c any/c)
  (path-related-wrapper-unguarded v))

; TODO SMOOSH: Give this better smooshing behavior using
; `prop:expressly-smooshable-dynamic-type`.
(define-imitation-simple-struct
  (info-wrapper? info-wrapper-value)
  info-wrapper-unguarded
  'info-wrapper (current-inspector)
  ; TODO SMOOSH: Stop using `auto-write` and `auto-equal` for this.
  (auto-write) (auto-equal))

(define/own-contract (info-wrapper v)
  (-> any/c any/c)
  (info-wrapper-unguarded v))

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
  custom-gloss-key-dynamic-type? custom-gloss-key-dynamic-type-impl?
  (#:method custom-gloss-key-dynamic-type-variant-knowable (#:this))
  (#:method custom-gloss-key-dynamic-type-get-reports-knowable
    (#:this))
  prop:custom-gloss-key-dynamic-type
  make-custom-gloss-key-dynamic-type-impl-from-various-unkeyworded
  'custom-gloss-key-dynamic-type 'custom-gloss-key-dynamic-type-impl
  (list))
(ascribe-own-contract custom-gloss-key-dynamic-type?
  (-> any/c boolean?))
(ascribe-own-contract custom-gloss-key-dynamic-type-impl?
  (-> any/c boolean?))
(ascribe-own-contract custom-gloss-key-dynamic-type-variant-knowable
  (-> custom-gloss-key-dynamic-type? (knowable/c any/c)))
(ascribe-own-contract
  custom-gloss-key-dynamic-type-get-reports-knowable
  ; For each report in the infinite sequence, the next report creates
  ; glossesques that not only compare keys by whether they smoosh
  ; along that one's `==` but also, only if they do, smooshes their
  ; information ordering representatives along their information
  ; ordering.
  (-> custom-gloss-key-dynamic-type?
    (knowable/c (sequence/c custom-gloss-key-report?))))
(ascribe-own-contract prop:custom-gloss-key-dynamic-type
  (struct-type-property/c custom-gloss-key-dynamic-type-impl?))

(define/own-contract
  (make-custom-gloss-key-dynamic-type-impl
    #:variant-knowable variant-knowable
    #:get-reports-knowable get-reports-knowable)
  (->
    #:variant-knowable
    (-> custom-gloss-key-dynamic-type? (knowable/c any/c))
    
    #:get-reports-knowable
    (-> custom-gloss-key-dynamic-type?
      (knowable/c (sequence/c custom-gloss-key-report?)))
    
    custom-gloss-key-dynamic-type-impl?)
  (make-custom-gloss-key-dynamic-type-impl-from-various-unkeyworded
    variant-knowable
    get-reports-knowable))


; TODO SMOOSH: Aren't we going to have the default "any" type depend
; partly on what orphan instances are in scope?
(define/own-contract (get-dynamic-type-with-default-bindings v)
  (-> any/c any/c)
  ; TODO SMOOSH: These uses of `known-to-lathe-comforts-data?`,
  ; `known-to-lathe-comforts-data-dynamic-type`, `any-dynamic-type`,
  ; `get-dynamic-type`, `gloss-set-maybe-knowable`,
  ; `gloss-union-of-zero`, and `dynamic-type-var-for-any-dynamic-type`
  ; are forward references. See if we can untangle them.
  (if (known-to-lathe-comforts-data? v)
    (known-to-lathe-comforts-data-dynamic-type /any-dynamic-type)
  /get-dynamic-type
    (known-value /gloss-set-maybe-knowable (gloss-union-of-zero)
      (dynamic-type-var-for-any-dynamic-type)
      (any-dynamic-type))
    v))

; TODO SMOOSH: Give this better smooshing behavior using
; `prop:expressly-smooshable-dynamic-type`.
(define-imitation-simple-struct
  (gloss?
    
    ; A natural number representing the number of key-value entries in
    ; the gloss.
    ;
    gloss-count-field
    
    ; An `equal-always?`-based `hash?` containing all the key-value
    ; entries in the gloss for which the key is an
    ; `equalw-gloss-key?`.
    ;
    gloss-atomic-entries
    
    ; A `maybe?` possibly containing another `gloss?` that maps a
    ; variant from `custom-gloss-key-dynamic-type-variant-knowable` to
    ; an `equal-always?`-based `hash?` that maps a two-element list
    ; containing a `'path-related` or `'==` symbol and a natural
    ; number (representing the number of iterations by which a key is
    ; compared by the information ordering of its information
    ; ordering, etc.) to a two-element list containing a
    ; `glossesque-sys?` and an `equal-always?`-based `hash?` that maps
    ; a list of `path-related-wrapper` and `'info-wrapper` symbols in
    ; the order they were unwrapped from a key to a glossesque of the
    ; indicated `glossesque-sys?` that maps an unwrapped key to a
    ; value. In code, that's rougly:
    ;
    ; (maybe/c
    ;   (gloss/c any/c
    ;     (hash/c (list/c (or/c 'path-related '==) natural?)
    ;       (and/c (list/c glossesque-sys? any/c)
    ;       /by-own-method/c (list gs _)
    ;       /list/c any/c
    ;         (hash/c
    ;           (listof (or/c 'path-related-wrapper 'info-wrapper))
    ;           (glossesque/c gs any/c any/c))))))
    ;
    ; Here, `gloss/c` and `glossesque/c` are hypothetical contracts
    ; that are analogous to `hash/c`.
    ;
    gloss-custom-entries)
  gloss
  'gloss (current-inspector)
  ; TODO SMOOSH: Stop using `auto-write` and `auto-equal` for this.
  (auto-write) (auto-equal))
(ascribe-own-contract gloss? (-> any/c boolean?))

(define (hash-km-union-of-two a b km-union)
  (hash-kv-map-maybe (hash-union a b #:combine /fn a b a) /fn k v
    (km-union k (hash-ref-maybe a k) (hash-ref-maybe b k))))

(define (maybe-m-union-of-two a b m-union)
  (mat a (just _) (m-union a b)
  /mat b (just _) (m-union a b)
  /nothing))

(define (unwrap-gloss-key v)
  (w-loop next v v path-mode '== depth 0 unwrapped-wrappers (list)
    (mat v (path-related-wrapper-unguarded v)
      (next v 'path-related depth
        (cons 'path-related-wrapper unwrapped-wrappers))
    /mat v (info-wrapper-unguarded v)
      (next v '== (add1 depth)
        (cons 'info-wrapper unwrapped-wrappers))
    /list v path-mode depth unwrapped-wrappers)))

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
      (just /gloss-km-union-of-two
        (mat a (just a) a (gloss-union-of-zero))
        (mat b (just b) b (gloss-union-of-zero))
        (fn a b
          (just /hash-km-union-of-two
            (mat a (just a) a (hashalw))
            (mat b (just b) b (hashalw))
            (fn a b
              (w- gs
                (mat a (just /list gs _) gs
                /dissect b (just /list gs _) gs)
              /just /list gs /hash-km-union-of-two
                (mat a (just /list _ a) a (hashalw))
                (mat b (just /list _ b) b (hashalw))
                (fn a b
                  (just /glossesque-sys-glossesque-km-union-of-two gs
                    (mat a (just /list _ a) a
                      (glossesque-sys-glossesque-union-of-zero gs))
                    (mat b (just /list _ b) b
                      (glossesque-sys-glossesque-union-of-zero gs))
                    km-union))))))))
  /gloss
    (+ (hash-count atomic)
      (expect custom (just custom) 0
        (for*/sum
          (
            [ (variant custom-regress)
              (in-sequences /gloss-iteration-sequence custom)]
            [(mode custom-entry) (in-hash custom-regress)]
            
            #:do
            [(match-define (list gs custom-unwrappings) custom-entry)]
            
            [(unwrapped-wrappers g) custom-unwrappings])
          (glossesque-sys-glossesque-count gs g))))
    atomic
    custom))

(define/own-contract (gloss-ref-maybe-knowable g k)
  (-> gloss? any/c (knowable/c maybe?))
  (dissect g (gloss _ atomic custom)
  /if (equalw-gloss-key? k) (known /hash-ref-maybe atomic k)
  /dissect (unwrap-gloss-key k)
    (list unwrapped-k path-mode depth unwrapped-wrappers)
  /w- unwrapped-k-dt
    (get-dynamic-type-with-default-bindings unwrapped-k)
  /expect (custom-gloss-key-dynamic-type? unwrapped-k-dt) #t (unknown)
  /expect custom (just custom) (known /nothing)
  /knowable-bind
    (custom-gloss-key-dynamic-type-variant-knowable unwrapped-k-dt)
  /fn variant
  /knowable-bind (gloss-ref-maybe-knowable custom variant)
  /fn custom-regress
  /expect custom-regress (just custom-regress) (known /nothing)
  /w- mode (list path-mode depth)
  /expect (hash-ref-maybe custom-regress mode) (just custom-entry)
    (known /nothing)
  /dissect custom-entry (list custom-gs custom-unwrappings)
  /expect (hash-ref-maybe custom-unwrappings unwrapped-wrappers)
    (just custom-g)
    (known /nothing)
  /glossesque-sys-glossesque-ref-maybe-knowable
    custom-gs custom-g unwrapped-k))

(define/own-contract (gloss-set-maybe-knowable g k m)
  (-> gloss? any/c maybe? (knowable/c gloss?))
  (dissect g (gloss count atomic custom)
  /if (equalw-gloss-key? k)
    (known /gloss count (hash-set-maybe atomic k m) custom)
  /dissect (unwrap-gloss-key k)
    (list unwrapped-k path-mode depth unwrapped-wrappers)
  /w- unwrapped-k-dt
    (get-dynamic-type-with-default-bindings unwrapped-k)
  /expect (custom-gloss-key-dynamic-type? unwrapped-k-dt) #t (unknown)
  /expect custom (just custom)
    (gloss-set-maybe-knowable
      (gloss count atomic (just /gloss-union-of-zero))
      k
      m)
  /knowable-bind
    (custom-gloss-key-dynamic-type-variant-knowable unwrapped-k-dt)
  /fn variant
  /knowable-bind (gloss-ref-maybe-knowable custom variant)
  /fn custom-regress
  /expect custom-regress (just custom-regress)
    (knowable-bind
      (gloss-set-maybe-knowable custom variant (just /hashalw))
    /fn custom
    /gloss-set-maybe-knowable (gloss count atomic (just custom)) k m)
  /w- mode (list path-mode depth)
  /expect (hash-ref-maybe custom-regress mode) (just custom-entry)
    (knowable-bind
      (custom-gloss-key-dynamic-type-get-reports-knowable
        unwrapped-k-dt)
    /fn reports
    /w- report (sequence-ref reports depth)
    /w- custom-gs
      (mat path-mode '==
        (custom-gloss-key-report-get-==-glossesque-sys report)
      /dissect path-mode 'path-related
        (custom-gloss-key-report-get-path-related-glossesque-sys report))
    /w- custom-regress
      (hash-set custom-regress mode (list custom-gs (hashalw)))
    /knowable-bind
      (gloss-set-maybe-knowable custom variant
        (just /hash-set custom-regress mode (list custom-gs (hashalw))))
    /fn custom
    /gloss-set-maybe-knowable (gloss count atomic (just custom)) k m)
  /dissect custom-entry (list custom-gs custom-unwrappings)
  /expect (hash-ref-maybe custom-unwrappings unwrapped-wrappers)
    (just custom-g)
    (knowable-bind
      (gloss-set-maybe-knowable custom variant
        (just /hash-set custom-regress mode
          (list custom-gs
            (hash-set custom-unwrappings unwrapped-wrappers
              (glossesque-sys-glossesque-union-of-zero custom-gs)))))
    /fn custom
    /gloss-set-maybe-knowable (gloss count atomic (just custom)) k m)
  /w- old-custom-g-count
    (glossesque-sys-glossesque-count custom-gs custom-g)
  /knowable-bind
    (glossesque-sys-glossesque-set-maybe-knowable
      custom-gs custom-g unwrapped-k m)
  /fn custom-g
  /w- new-custom-g-count
    (glossesque-sys-glossesque-count custom-gs custom-g)
  /knowable-bind
    (gloss-set-maybe-knowable custom variant
      (just /hash-set custom-regress mode
        (list custom-gs
          (hash-set custom-unwrappings unwrapped-wrappers custom-g))))
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
    /for*/list
      (
        [ (variant custom-regress)
          (in-sequences /gloss-iteration-sequence custom)]
        [(mode custom-entry) (in-hash custom-regress)]
        
        #:do
        [(match-define (list gs custom-unwrappings) custom-entry)]
        
        [(unwrapped-wrappers g) custom-unwrappings])
      (glossesque-sys-glossesque-iteration-sequence gs g))))

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


(define-imitation-simple-struct (uninformative-dynamic-type?)
  uninformative-dynamic-type-unguarded
  'uninformative-dynamic-type (current-inspector) (auto-write))

(define/own-contract (uninformative-dynamic-type)
  (-> any/c)
  (uninformative-dynamic-type-unguarded))

; TODO SMOOSH: Give this better smooshing behavior using
; `prop:expressly-smooshable-dynamic-type`.
(define-imitation-simple-struct
  (dynamic-type-var-for-any-dynamic-type?)
  dynamic-type-var-for-any-dynamic-type
  'dynamic-type-var-for-any-dynamic-type (current-inspector)
  (auto-write)
  (auto-equal)
  (#:prop prop:equalw-gloss-key /make-equalw-gloss-key-impl))
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
    expressly-smooshable-dynamic-type-get-smoosh-of-zero-report
    (#:this))
  (#:method expressly-smooshable-dynamic-type-get-smoosh-of-one-report
    (#:this)
    ())
  (#:method
    expressly-smooshable-dynamic-type-get-smoosh-and-comparison-of-two-report
    (#:this)
    ()
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
    #:get-smoosh-of-zero-report get-smoosh-of-zero-report
    #:get-smoosh-of-one-report get-smoosh-of-one-report
    
    #:get-smoosh-and-comparison-of-two-report
    get-smoosh-and-comparison-of-two-report)
  (->
    #:get-smoosh-of-zero-report (-> any/c (sequence/c smoosh-report?))
    
    #:get-smoosh-of-one-report
    (-> any/c any/c (sequence/c smoosh-report?))
    
    #:get-smoosh-and-comparison-of-two-report
    (-> any/c any/c any/c any/c
      (sequence/c smoosh-and-comparison-of-two-report?))
    
    expressly-smooshable-dynamic-type-impl?)
  (make-expressly-smooshable-dynamic-type-impl-from-various-unkeyworded
    get-smoosh-of-zero-report
    get-smoosh-of-one-report
    get-smoosh-and-comparison-of-two-report))

(define/own-contract (dynamic-type-get-smoosh-of-zero-report dt)
  (-> any/c
    ; Each report in the infinite sequence gives the smoosh identity
    ; elements, first for the type's bespoke notion of ordering, then
    ; for the information ordering, then for the information ordering
    ; of the information ordering representatives, and so on.
    (sequence/c smoosh-report?))
  (if (expressly-smooshable-dynamic-type? dt)
    (expressly-smooshable-dynamic-type-get-smoosh-of-zero-report dt)
    (uninformative-smoosh-reports)))

(define/own-contract (dynamic-type-get-smoosh-of-one-report a-dt a)
  (-> any/c any/c (sequence/c smoosh-report?))
  (if (expressly-smooshable-dynamic-type? a-dt)
    (expressly-smooshable-dynamic-type-get-smoosh-of-one-report
      a-dt a)
    (uninformative-smoosh-reports)))

(define/own-contract
  (dynamic-type-get-smoosh-and-comparison-of-two-report a-dt b-dt a b)
  (->
    ; lhs type
    any/c
    ; rhs type (usually dispatched to next, if this one can't fully
    ; determine the results)
    any/c
    ; lhs
    any/c
    ; rhs
    any/c
    ; For each report in the infinite sequence, the next report says
    ; not only whether they smoosh along that one's == but also, only
    ; if they do, how their information ordering representatives
    ; smoosh along their information ordering.
    (sequence/c smoosh-report?))
  (if (expressly-smooshable-dynamic-type? a-dt)
    (expressly-smooshable-dynamic-type-get-smoosh-and-comparison-of-two-report
      a-dt b-dt a b)
    (uninformative-smoosh-and-comparison-of-two-reports)))

(define-imitation-simple-struct
  (flipped-smoosh-and-comparison-of-two-report?
    flipped-smoosh-and-comparison-of-two-report-original)
  flipped-smoosh-and-comparison-of-two-report-unguarded
  'flipped-smoosh-and-comparison-of-two-report (current-inspector)
  (auto-write)
  (#:prop prop:smoosh-and-comparison-of-two-report
    (make-smoosh-and-comparison-of-two-report-impl
      
      #:<=?-knowable-promise
      (dissectfn
        (flipped-smoosh-and-comparison-of-two-report-unguarded
          original)
        (smoosh-and-comparison-of-two-report->=?-knowable-promise
          original))
      
      #:>=?-knowable-promise
      (dissectfn
        (flipped-smoosh-and-comparison-of-two-report-unguarded
          original)
        (smoosh-and-comparison-of-two-report-<=?-knowable-promise
          original))
      
      #:get-smoosh-report
      (dissectfn
        (flipped-smoosh-and-comparison-of-two-report-unguarded
          original)
        (smoosh-and-comparison-of-two-report-get-smoosh-report
          original))
      
      )))

(define/own-contract (smoosh-and-comparison-of-two-report-flip report)
  (-> smoosh-report? smoosh-report?)
  (mat report
    (flipped-smoosh-and-comparison-of-two-report-unguarded original)
    original
  /flipped-smoosh-and-comparison-of-two-report-unguarded report))

(define/own-contract
  (smoosh-and-comparison-of-two-reports-flip reports)
  (-> (sequence/c smoosh-report?) (sequence/c smoosh-report?))
  (sequence-map
    (fn report /smoosh-and-comparison-of-two-report-flip report)
    reports))

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
  (->
    smoosh-report?
    
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
  (->
    (sequence/c smoosh-report?)
    
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
    
    (sequence/c smoosh-report?))
  (sequence-map
    (fn report
      (smoosh-report-map report
        
        #:on-smoosh-result-knowable-promise-maybe-knowable-promise
        on-smoosh-result-knowable-promise-maybe-knowable-promise
        
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
  (zip-mapped-smoosh-report?
    zip-mapped-smoosh-report-on-join-knowable-promise-maybe-knowable-promise
    zip-mapped-smoosh-report-on-meet-knowable-promise-maybe-knowable-promise
    zip-mapped-smoosh-report-on-==-knowable-promise-maybe-knowable-promise
    zip-mapped-smoosh-report-on-path-related-knowable-promise-maybe-knowable-promise
    zip-mapped-smoosh-report-original-list)
  zip-mapped-smoosh-report
  'zip-mapped-smoosh-report (current-inspector)
  (auto-write)
  (#:prop prop:smoosh-report /make-smoosh-report-impl
    
    #:join-knowable-promise-maybe-knowable-promise
    (dissectfn
      (zip-mapped-smoosh-report
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
      (zip-mapped-smoosh-report
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
      (zip-mapped-smoosh-report
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
      (zip-mapped-smoosh-report
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
  (smoosh-report-zip-map report-list
    
    #:on-smoosh-result-knowable-promise-maybe-knowable-promise
    [ on-smoosh-result-knowable-promise-maybe-knowable-promise
      (fn kpmkp-list
        (raise-arguments-error 'smoosh-report-zip-map
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
  (->
    (listof smoosh-report?)
    
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
    
    smoosh-report?)
  (zip-mapped-smoosh-report
    on-join-knowable-promise-maybe-knowable-promise
    on-meet-knowable-promise-maybe-knowable-promise
    on-==-knowable-promise-maybe-knowable-promise
    on-path-related-knowable-promise-maybe-knowable-promise
    report-list))

(define/own-contract
  (smoosh-reports-zip-map reports-list
    
    #:on-smoosh-result-knowable-promise-maybe-knowable-promise
    [ on-smoosh-result-knowable-promise-maybe-knowable-promise
      (fn kpmkp-list
        (raise-arguments-error 'smoosh-reports-zip-map
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
  (->
    (listof (sequence/c smoosh-report?))
    
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
    
    (sequence/c smoosh-report?))
  (sequence-zip-map reports-list /fn report-list
    (smoosh-report-zip-map report-list
      
      #:on-smoosh-result-knowable-promise-maybe-knowable-promise
      on-smoosh-result-knowable-promise-maybe-knowable-promise
      
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
    [ on-<=?-knowable-promise
      on-check-result-knowable-promise]
    
    #:on->=?-knowable-promise
    [ on->=?-knowable-promise
      on-check-result-knowable-promise]
    
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
  (->
    smoosh-and-comparison-of-two-report?
    
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
    [ on-<=?-knowable-promise
      on-check-result-knowable-promise]
    
    #:on->=?-knowable-promise
    [ on->=?-knowable-promise
      on-check-result-knowable-promise]
    
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
  (->
    (sequence/c smoosh-and-comparison-of-two-report?)
    
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
  (zip-mapped-smoosh-and-comparison-of-two-report?
    zip-mapped-smoosh-and-comparison-of-two-report-on-<=?-knowable-promise
    zip-mapped-smoosh-and-comparison-of-two-report-on->=?-knowable-promise
    zip-mapped-smoosh-and-comparison-of-two-report-on-join-knowable-promise-maybe-knowable-promise
    zip-mapped-smoosh-and-comparison-of-two-report-on-meet-knowable-promise-maybe-knowable-promise
    zip-mapped-smoosh-and-comparison-of-two-report-on-==-knowable-promise-maybe-knowable-promise
    zip-mapped-smoosh-and-comparison-of-two-report-on-path-related-knowable-promise-maybe-knowable-promise
    zip-mapped-smoosh-and-comparison-of-two-report-original-list)
  zip-mapped-smoosh-and-comparison-of-two-report
  'zip-mapped-smoosh-and-comparison-of-two-report (current-inspector)
  (auto-write)
  (#:prop prop:smoosh-and-comparison-of-two-report
    (make-smoosh-and-comparison-of-two-report-impl
      
      #:<=?-knowable-promise
      (dissectfn
        (zip-mapped-smoosh-and-comparison-of-two-report
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
        (zip-mapped-smoosh-and-comparison-of-two-report
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
        (zip-mapped-smoosh-and-comparison-of-two-report
          on-<=?-knowable-promise
          on->=?-knowable-promise
          on-join-knowable-promise-maybe-knowable-promise
          on-meet-knowable-promise-maybe-knowable-promise
          on-==-knowable-promise-maybe-knowable-promise
          on-path-related-knowable-promise-maybe-knowable-promise
          original-list)
        (smoosh-report-zip-map
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
  (smoosh-and-comparison-of-two-report-zip-map report-list
    
    #:on-check-result-knowable-promise
    [ on-check-result-knowable-promise
      (fn kp-list
        (raise-arguments-error 'smoosh-and-comparison-of-two-report-zip-map
          "tried to retrieve a check result when its mapping behavior was undefined"
          "check-result-knowable-promise-list" kp-list))]
    
    #:on-<=?-knowable-promise
    [on-<=?-knowable-promise on-check-result-knowable-promise]
    
    #:on->=?-knowable-promise
    [on->=?-knowable-promise on-check-result-knowable-promise]
    
    #:on-smoosh-result-knowable-promise-maybe-knowable-promise
    [ on-smoosh-result-knowable-promise-maybe-knowable-promise
      (fn kpmkp-list
        (raise-arguments-error 'smoosh-and-comparison-of-two-report-zip-map
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
  (->
    (listof smoosh-and-comparison-of-two-report?)
    
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
    
    smoosh-and-comparison-of-two-report?)
  (zip-mapped-smoosh-and-comparison-of-two-report
    on-<=?-knowable-promise
    on->=?-knowable-promise
    on-join-knowable-promise-maybe-knowable-promise
    on-meet-knowable-promise-maybe-knowable-promise
    on-==-knowable-promise-maybe-knowable-promise
    on-path-related-knowable-promise-maybe-knowable-promise
    report-list))

(define/own-contract
  (smoosh-and-comparison-of-two-reports-zip-map reports-list
    
    #:on-check-result-knowable-promise
    [ on-check-result-knowable-promise
      (fn kp-list
        (raise-arguments-error 'smoosh-and-comparison-of-two-report-zip-map
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
        (raise-arguments-error 'smoosh-and-comparison-of-two-report-zip-map
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
  (->
    (listof (sequence/c smoosh-and-comparison-of-two-report?))
    
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
    
    (sequence/c smoosh-and-comparison-of-two-report?))
  (sequence-zip-map reports-list /fn report-list
    (smoosh-and-comparison-of-two-report-zip-map report-list
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

(define/own-contract (false-smoosh-and-comparison-of-two-reports)
  (-> (sequence/c smoosh-and-comparison-of-two-report?))
  (smoosh-and-comparison-of-two-reports-zip-map (list)
    #:on-check-result-knowable-promise
    (dissectfn (list)
      (delay/strict /known #f))
    #:on-smoosh-result-knowable-promise-maybe-knowable-promise
    (dissectfn (list)
      (delay/strict /known /nothing))))

(define-imitation-simple-struct
  (dead-end-dynamic-type?)
  dead-end-dynamic-type-unguarded
  'dead-end-dynamic-type (current-inspector) (auto-write)
  ; NOTE: This specifically should never implement
  ; `prop:expressly-smooshable-dynamic-type`.
  )

(define/own-contract (dead-end-dynamic-type)
  (-> any/c)
  (dead-end-dynamic-type-unguarded))

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

; TODO SMOOSH: See if we still need this.
(define-imitation-simple-struct
  (compare-by-predicates-dynamic-type?
    compare-by-predicates-dynamic-type-getter-for-variant
    compare-by-predicates-dynamic-type-getter-for-predicate
    compare-by-predicates-dynamic-type-getter-for-compare)
  compare-by-predicates-dynamic-type-unguarded
  'compare-by-predicates-dynamic-type (current-inspector) (auto-write)
  (#:prop prop:expressly-smooshable-dynamic-type
    (make-expressly-smooshable-dynamic-type-impl
      
      #:get-smoosh-of-zero-report
      (fn self
        (uninformative-smoosh-reports))
      
      #:get-smoosh-of-one-report
      (fn self a
        (dissect self
          (compare-by-predicates-dynamic-type-unguarded
            get-variant
            get-predicate
            get-compare)
        /w- included? (get-predicate)
        /constant-smoosh-reports /delay
          (if (not /included? a) (unknown)
          /known /just /delay/strict /known a)))
      
      #:get-smoosh-and-comparison-of-two-report
      (fn self b-dt a b
        (dissect self
          (compare-by-predicates-dynamic-type-unguarded
            get-variant
            get-predicate
            get-compare)
        /w- included? (get-predicate)
        /w- compares? (get-compare)
        /constant-smoosh-and-comparison-of-two-reports /delay
          (if (not /included? a) (unknown)
          /if (not /included? b) (unknown)
          /if (not /compares? a b) (known /nothing)
          /known /just /delay/strict /known a)))
      
      )))

(define/own-contract
  (compare-by-predicates-dynamic-type
    #:get-variant get-variant
    #:get-predicate get-predicate
    #:get-compare get-compare)
  (->
    #:get-variant (-> any/c)
    #:get-predicate (-> (-> any/c boolean?))
    #:get-compare (-> (-> any/c any/c boolean?))
    any/c)
  (compare-by-predicates-dynamic-type-unguarded
    get-variant
    get-predicate
    get-compare))

(define-imitation-simple-struct
  (equalw-gloss-key-wrapper? equalw-gloss-key-wrapper-value)
  equalw-gloss-key-wrapper-unguarded
  'equalw-gloss-key-wrapper (current-inspector) (auto-write)
  ; TODO SMOOSH: Instead of using `auto-equal` for this, use a
  ; comparison that consistently compares the value using
  ; `equal-always?`.
  (auto-equal)
  (#:prop prop:equalw-gloss-key /make-equalw-gloss-key-impl))

; TODO SMOOSH: Use this. One way to use it might be like so:
#;
(#:prop prop:expressly-has-dynamic-type
  (make-expressly-has-dynamic-type-impl /fn bindings self
    (compare-by-predicates-dynamic-type
      #:get-variant (fn /equalw-gloss-key-wrapper any-dynamic-type?)
      #:get-predicate (fn any-dynamic-type?)
      #:get-compare (fn /fn a b #t))))
;
(define/own-contract (equalw-gloss-key-wrapper v)
  (-> any/c any/c)
  (equalw-gloss-key-wrapper-unguarded v))

(define/own-contract (knowable-promise-zip-map kp-list on-value)
  (-> (listof (promise/c knowable?)) (-> any/c any/c)
    (promise/c knowable?))
  (delay
    (if (list-any kp-list /fn kp /unknown? /force kp) (unknown)
    /known /on-value /list-map kp-list /fn kp
      (known-value /force kp))))

(define/own-contract
  (boolean-and-knowable-promise-zip-map kp-list on-true)
  (-> (listof (promise/c (knowable/c boolean?))) (-> boolean?)
    (promise/c (knowable/c boolean?)))
  (delay
    (if
      (list-any kp-list /fn kp
        (mat (force kp) (known #f) #t #f))
      (known #f)
    /if (list-any kp-list /fn kp /unknown? /force kp) (unknown)
    /known /on-true)))

(define/own-contract (boolean-and-knowable-thunk-zip kble-thunk-list)
  (-> (listof (-> (knowable/c boolean?))) (knowable/c boolean?))
  (boolean-and-knowable-promise-zip-map
    (list-map kble-thunk-list /fn kble-thunk /delay /kble-thunk)
    (fn #t)))

(define/own-contract (boolean-or-knowable-thunk-zip kble-thunk-list)
  (-> (listof (-> (knowable/c boolean?))) (knowable/c boolean?))
  (not /boolean-and-knowable-thunk-zip
    (list-map kble-thunk-list /fn kble-thunk /fn /not /kble-thunk)))

(define/own-contract
  (maybe-min-knowable-promise-zip-map mkp-list on-value)
  (-> (listof (promise/c (knowable/c maybe?))) (-> list? any/c)
    (promise/c (knowable/c maybe?)))
  (delay
    (if
      (list-any mkp-list /fn mkp
        (mat (force mkp) (known /nothing) #t #f))
      (known /nothing)
    /force /knowable-promise-zip-map mkp-list /fn m-list
      (just /on-value /list-map m-list /fn m /just-value m))))

(define/own-contract (sequence-zip-map sequences on-element)
  (->
    (non-empty-listof (sequence/c any/c))
    (-> (non-empty-listof any/c) any/c)
    (sequence/c any/c))
  (sequence-map
    (fn elements /on-element elements)
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
  (smoosh-and-comparison-of-two-report-join reports-list)
  (-> (listof smoosh-report?) smoosh-report?)
  (smoosh-and-comparison-of-two-report-map
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
  (sequence-zip-map reports-list /fn report-list
    (smoosh-and-comparison-of-two-report-join report-list)))

; TODO SMOOSH: Consider exporting this.
(define/own-contract (base-readable-discrete-atom? v)
  (-> any/c boolean?)
  (or
    (boolean? v)
    (char? v)
    (symbol? v)
    (keyword? v)
    (regexp? v)
    (compiled-expression? v)
    (string? v)
    (bytes? v)
    (null? v)))

; TODO SMOOSH: Consider exporting this. If we export it, consider
; whether we want to give it better smooshing behavior using
; `prop:expressly-smooshable-dynamic-type` and/or implement
; `prop:equal+hash` for it.
;
; Level 0+:
;   <=, >=, path-related, join, meet, ==:
;     If the operands are not both `base-readable-discrete-atom?`
;     values, then unknown.
;     
;     Otherwise, if the operands are not `equal-always?`, then a known
;     nothing (or, for a check, `#f`).
;     
;     Otherwise, the first operand (or, for a check, `#t`).
;
(define-imitation-simple-struct
  (base-readable-discrete-atom-dynamic-type?
    base-readable-discrete-atom-dynamic-type-get-any-dynamic-type)
  base-readable-discrete-atom-dynamic-type
  'base-readable-discrete-atom-dynamic-type (current-inspector)
  (auto-write)
  
  (#:prop prop:expressly-smooshable-dynamic-type
    (make-expressly-smooshable-dynamic-type-impl
      
      #:get-smoosh-of-zero-report
      (fn self
        (uninformative-smoosh-reports))
      
      #:get-smoosh-of-one-report
      (fn self a
        (dissect self
          (base-readable-discrete-atom-dynamic-type any-dt)
        /expect (base-readable-discrete-atom? a) #t
          (uninformative-smoosh-reports)
        /constant-smoosh-reports
          (delay/strict /known /just /delay/strict /known a)))
      
      #:get-smoosh-and-comparison-of-two-report
      (fn self b-dt a b
        (dissect self
          (base-readable-discrete-atom-dynamic-type any-dt)
        /expect (base-readable-discrete-atom? a) #t
          (uninformative-smoosh-and-comparison-of-two-reports)
        /expect (base-readable-discrete-atom? b) #t
          (uninformative-smoosh-and-comparison-of-two-reports)
        /expect (equal-always? a b) #t
          (false-smoosh-and-comparison-of-two-reports)
        /constant-smoosh-reports
          (delay/strict /known /just /delay/strict /known a)))
      
      )))

; TODO SMOOSH: Export this.
(define/own-contract (non-nan-number? v)
  (-> any/c boolean?)
  (and
    (number? v)
    (not /nan? /real-part v)
    (not /nan? /imag-part v)))

; TODO SMOOSH: Consider exporting this. If we export it, consider
; whether we want to give it better smooshing behavior using
; `prop:expressly-smooshable-dynamic-type` and/or implement
; `prop:equal+hash` for it.
;
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
      
      #:get-smoosh-of-zero-report
      (fn self
        (uninformative-smoosh-reports))
      
      #:get-smoosh-of-one-report
      (fn self a
        (expect (non-nan-number? a) #t (uninformative-smoosh-reports)
        /constant-smoosh-reports
          (delay/strict /known /just /delay/strict /known a)))
      
      #:get-smoosh-and-comparison-of-two-report
      (fn self b-dt a b
        (expect (non-nan-number? a) #t
          (uninformative-smoosh-and-comparison-of-two-reports)
        /expect (non-nan-number? b) #t
          (uninformative-smoosh-and-comparison-of-two-reports)
        /w- report-1+
          (constant-smoosh-and-comparison-of-two-reports
            (delay /known
              (maybe-if (equal-always? a b)
                (fn /delay/strict /known a))))
        /if (= a b)
          (stream*
            (constant-smoosh-and-comparison-of-two-report
              (delay/strict /known /just /delay/strict /known a))
            report-1+)
        /w- real?-promise
          (delay /and (zero? /imag-part a) (zero? /imag-part b))
        /w- <=?-knowable-promise
          (promise-map real?-promise /fn real?
            (if real?
              (known /<= (real-part a) (real-part b))
              (unknown)))
        /w- >=?-knowable-promise
          (promise-map real?-promise /fn real?
            (if real?
              (known />= (real-part a) (real-part b))
              (unknown)))
        /w- join-knowable-promise-maybe-knowable-promise
          (promise-map <=?-knowable-promise /fn knowable
            (knowable-map knowable /fn result
              (just /delay/strict /known /if result b a)))
        /w- meet-knowable-promise-maybe-knowable-promise
          (promise-map <=?-knowable-promise /fn knowable
            (knowable-map knowable /fn result
              (just /delay/strict /known /if result a b)))
        /stream*
          (smoosh-and-comparison-of-two-reports-zip-map (list)
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
      
      )))

; TODO SMOOSH: Export this.
(define/own-contract (non-nan-extflonum? v)
  (-> any/c boolean?)
  (and (extflonum? v) (not /extfl= v v)))

; TODO SMOOSH: Consider exporting this. If we export it, consider
; whether we want to give it better smooshing behavior using
; `prop:expressly-smooshable-dynamic-type` and/or implement
; `prop:equal+hash` for it.
;
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
      
      #:get-smoosh-of-zero-report
      (fn self
        (uninformative-smoosh-reports))
      
      #:get-smoosh-of-one-report
      (fn self a
        (expect (non-nan-extflonum? a) #t
          (uninformative-smoosh-reports)
        /constant-smoosh-reports
          (delay/strict /known /just /delay/strict /known a)))
      
      #:get-smoosh-and-comparison-of-two-report
      (fn self b-dt a b
        (expect (non-nan-extflonum? a) #t
          (uninformative-smoosh-and-comparison-of-two-reports)
        /expect (non-nan-extflonum? b) #t
          (uninformative-smoosh-and-comparison-of-two-reports)
        /w- report-1+
          (constant-smoosh-and-comparison-of-two-reports
            (delay /known
              (maybe-if (equal-always? a b)
                (fn /delay/strict /known a))))
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
          (smoosh-and-comparison-of-two-reports-zip-map (list)
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
      
      )))

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
  (maybe-min-knowable-promise-zip-map kpmkp-list /fn kp-list
    (knowable-promise-zip-map kp-list /dissectfn
      (list result-car result-cdr)
      (cons result-car result-cdr))))

; TODO SMOOSH: Consider exporting this. If we export it, consider
; whether we want to give it better smooshing behavior using
; `prop:expressly-smooshable-dynamic-type` and/or implement
; `prop:equal+hash` for it.
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
        "tried to get the dynamic type of a cons cell without giving a binding for (dynamic-type-var-for-any-dynamic-type)"
        "bindings" bindings
        "inhabitant" self)
    /cons-dynamic-type any-dt)))
;
(define-imitation-simple-struct
  (cons-dynamic-type? cons-dynamic-type-get-any-dynamic-type)
  cons-dynamic-type
  'cons-dynamic-type (current-inspector) (auto-write)
  
  (#:prop prop:expressly-smooshable-dynamic-type
    (make-expressly-smooshable-dynamic-type-impl
      
      #:get-smoosh-of-zero-report
      (fn self
        (dissect self (cons-dynamic-type any-dt)
        /smoosh-reports-map
          (dynamic-type-get-smoosh-of-zero-report any-dt)
          #:on-smoosh-result-knowable-promise-maybe-knowable-promise
          (fn kpmkp
            (promise-map kpmkp /fn kpmk
              (knowable-map kpmk /fn kpm
                (maybe-map kpm /fn kp
                  (promise-map kp /fn k
                    (knowable-map k /fn result
                      (cons result result)))))))))
      
      #:get-smoosh-of-one-report
      (fn self a
        (dissect self (cons-dynamic-type any-dt)
        /expect a (cons a-car a-cdr) (uninformative-smoosh-reports)
        /smoosh-reports-zip-map
          (list
            (dynamic-type-get-smoosh-of-one-report any-dt a-car)
            (dynamic-type-get-smoosh-of-one-report any-dt a-cdr))
          #:on-result-knowable-promise-maybe-knowable-promise
          on-cons-smoosh-result-knowable-promise-maybe-knowable-promise))
      
      #:get-smoosh-and-comparison-of-two-report
      (fn self b-dt a b
        (dissect self (cons-dynamic-type any-dt)
        /expect a (cons a-car a-cdr)
          (uninformative-smoosh-and-comparison-of-two-reports)
        /expect b (cons b-car b-cdr)
          (uninformative-smoosh-and-comparison-of-two-reports)
        /smoosh-and-comparison-of-two-reports-zip-map
          (list
            (dynamic-type-get-smoosh-and-comparison-of-two-report
              any-dt a-car b-car)
            (dynamic-type-get-smoosh-and-comparison-of-two-report
              any-dt a-cdr b-cdr))
          #:on-check-result-knowable-promise
          (fn kp-list
            (knowable-promise-zip-map kp-list /dissectfn
              (list check-result-car check-result-cdr)
              (and check-result-car check-result-cdr)))
          #:on-smoosh-result-knowable-promise-maybe-knowable-promise
          on-cons-smoosh-result-knowable-promise-maybe-knowable-promise))
      
      )))

; Given two values, this checks whether they have `eq?` elements (and
; whether they're structurally similar enough according to
; `equal-always?/recur` to get as far as the element comparison). Like
; `chaperone-of?`, this doesn't traverse the elements if the given
; values are `eq?` themselves.
;
; TODO: Consider exporting this.
;
(define/own-contract (elements-eq? a b)
  (-> any/c any/c any/c)
  (equal-always?/recur a b /fn a-elem b-elem /eq? a-elem b-elem))

; This is an appropriate `prop:expressly-smooshable-dynamic-type`
; implementation for immutable tuple data structures and their
; chaperones, information-ordered in a way that's consistent with
; `chaperone-of?` as long as the elements' information orderings are.
;
; In order to be able to check `(chaperone-of? a b)` in a "shallow"
; way without redundant (quadratic-time) work checking all the
; elements, this uses a multi-step process: First it checks
; `(chaperone-of? (copy b) b)` to see if `b` has only non-interposing
; chaperone wrappers. (This step doesn't need to traverse the elements
; because they'll be `eq?`.) If `b` does have only non-interposing
; chaperone wrappers, then we know the chaperone and impersonator
; wrappers on `a` are at least as extant, making `a` shallowly
; chaperone-of `b`. Otherwise, we know that `b` has some impersonator
; wrapper or interposing chaperone wrapper, in which case we know we
; can run `(chaperone-of? a b)` without it having to traverse all the
; way into the elements; once it hits that wrapper on `b`, it'll check
; `a`'s unwrappings for a wrapper that's `eq?` and stop if it's not
; there.
;
; The given `->list` function should return the same list of elements
; that would be passed to the callback of `equal-always?/recur`.
;
; The given `->list` and `example-and-list->` functions should specify
; an isomorphism between inhabitants (those values which pass the
; given `inhabitant?` predicate) and some set of lists. This isn't
; possible for every type, so this is only a suitable abstraction when
; it is possible to satisfy this condition.
;
; The given `copy` function should return an inhabitant that's
; `equal-always?` to its input inhabitant, but that doesn't have any
; impersonator wrappers or interposing chaperone wrappers.
;
; Level 0:
;   path-related, join, meet, ==:
;     Same as the description of level 1 path-related, but with "the
;     same smoosh" referring to this level-0 smoosh.
;   <=, >=:
;     Same as the description of level 1 path-related as a check, but
;     with "the same smoosh" referring to this level-0 check.
; Level 1:
;   path-related, join, meet, ==:
;     If the operands do not both pass the given `inhabitant?`
;     predicate, then unknown.
;     
;     Otherwise, if the operands are different lengths or the results
;     of smooshing corresponding elements under the same smoosh
;     include a known nothing, then a known nothing.
;     
;     Otherwise, if those recursive results include an unknown, then
;     unknown.
;     
;     Otherwise, if those recursive results are `eq?` to the elements
;     of an operand that counts as an acceptable result, then the
;     first such operand.
;     
;     Otherwise, if a new inhabitant (created by the given
;     `example-and-list->` function using the first operand as the
;     example) whose elements are those recursive results is an
;     acceptable result, then that inhabitant.
;     
;     Otherwise, unknown.
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
;         Every result is acceptable if it and the operands are all
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
;     If the operands do not both pass the given `inhabitant?`
;     predicate, then unknown.
;     
;     Otherwise, if the operands are different lengths or the results
;     of smooshing corresponding elements under the same smoosh
;     include a known `#f`, then a known `#f`.
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
;     chaperone-of check in the future when they're written to more
;     diligently reusing chaperone-wrapped values. This kind of
;     rewrite may be more feasible if and when Racket introduces
;     chaperone-wrapping operations that create equivalent wrappers in
;     some reliable way. As such, we'll treat some results as unknown
;     even though a more Racket-version-pinned design might treat them
;     as known nothings.)
;   Where for inhabitants X and Y with equal non-element,
;   non-chaperone-or-impersonator-wrapper details, "X is shallowly
;   chaperone-of Y" means:
;     If Y is shallowly unchaperoned:
;       It's true.
;     Otherwise:
;       It's true iff `(chaperone-of? X Y)` (which in this case will
;       be guaranteed not to perform recursive comparisons).
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
; TODO SMOOSH: Consider exporting this.
;
(define/own-contract
  (make-expressly-smooshable-dynamic-type-impl-from-list-isomorphism
    #:self-get-any-dynamic-type self-get-any-dynamic-type
    #:inhabitant? inhabitant?
    #:->list ->list
    #:example-and-list-> example-and-list->
    #:copy [copy (fn v /example-and-list-> v /->list v)]
    
    #:get-smoosh-of-zero-report
    [ get-smoosh-of-zero-report
      (fn self
        (uninformative-smoosh-reports))])
  (->*
    (
      #:self-get-any-dynamic-type (-> any/c any/c)
      #:inhabitant? (-> any/c boolean?)
      #:->list (-> any/c list?)
      #:example-and-list-> (-> any/c list? any/c))
    (
      #:copy (-> any/c any/c)
      
      #:get-smoosh-of-zero-report
      (-> any/c (sequence/c smoosh-report?))
      
      )
    expressly-smooshable-dynamic-type-impl?)
  (w- inhabitant-shallowly-unchaperoned?
    (fn v
      (chaperone-of? (copy v) v))
  /make-expressly-smooshable-dynamic-type-impl
    #:get-smoosh-of-zero-report get-smoosh-of-zero-report
    
    #:get-smoosh-of-one-report
    (fn self a
      (w- any-dt (self-get-any-dynamic-type self)
      /expect (inhabitant? a) #t (uninformative-smoosh-reports)
      /dissect
        (smoosh-reports-zip-map
          (list-map (->list a) /fn a-elem
            (dynamic-type-get-smoosh-of-one-report any-dt a-elem))
          #:on-result-knowable-promise-maybe-knowable-promise
          (fn kpmkp-list
            (maybe-min-knowable-promise-zip-map kpmkp-list /fn kp-list
              (knowable-promise-zip-map kp-list /fn result-list
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
                      (w- candidate-result
                        (example-and-list-> a result-list)
                      /if (elements-eq? candidate-result a) (known a)
                      ; If we're doing a particularly strict check and
                      ; the operand `a` is wrapped with impersonators
                      ; or interposing chaperones, we have no `known?`
                      ; result.
                      /if
                        (or
                          (not result-needs-to-be-chaperone-of?)
                          (force a-shallowly-unchaperoned?-promise))
                        (known candidate-result)
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
    
    #:get-smoosh-and-comparison-of-two-report
    (fn self b-dt a b
      (w- any-dt (self-get-any-dynamic-type self)
      /expect (inhabitant? a) #t
        (uninformative-smoosh-and-comparison-of-two-reports)
      /expect (inhabitant? b) #t
        (uninformative-smoosh-and-comparison-of-two-reports)
      ; If the operands differ even without comparing their chaperone
      ; wrappers or their elements, we return a known nothing (when
      ; doing a smoosh, or `#f` when doing a check).
      /if (not /equal-always?/recur a b /fn a-elem b-elem #t)
        (false-smoosh-and-comparison-of-two-reports)
      /dissect
        (smoosh-and-comparison-of-two-reports-zip-map
          ; TODO SMOOSH: It's embarrassing that we're calling the rest
          ; of these things `...-zip-map` when they take lists and
          ; list-receiving functions, while `list-zip-map` here takes
          ; two values and a two-value-receiving function.
          (list-zip-map (->list a) (->list b) /fn a-elem b-elem
            (dynamic-type-get-smoosh-and-comparison-of-two-report
              any-dt a-elem b-elem))
          #:on-check-result-knowable-promise
          (fn kp-list
            (boolean-and-knowable-promise-zip-map kp-list /fn #t))
          #:on-smoosh-result-knowable-promise-maybe-knowable-promise
          (fn kpmkp-list
            (maybe-min-knowable-promise-zip-map kpmkp-list /fn kp-list
              (knowable-promise-zip-map kp-list /fn result-list
                result-list))))
        (stream* report-0 report-1 report-2+)
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
      ; Given two inhabitants whose non-element,
      ; non-chaperone-or-impersonator-wrapper details are
      ; `equal-always?/recur` and whose whose elements are known to
      ; pass the kind of smoosh we're doing, this checks whether they
      ; would pass `chaperone-of?` if every recursive element
      ; comparison immediately returned `#t`. Like `chaperone-of?`,
      ; this takes constant time if the inhabitants are `eq?`
      ; themselves.
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
                (boolean-and-knowable-thunk-zip /list
                  (fn /known result)
                  (fn /boolean-or-knowable-thunk-zip /list
                    (fn /known /not should-a-be-small?)
                    (fn /falsable->uninformative-knowable
                      (inhabitant-shallowly-chaperone-of? b a)))
                  (fn /boolean-or-knowable-thunk-zip /list
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
                      (w- noncanonical-result
                        (example-and-list-> a result-list)
                      /if
                        (and
                          (elements-eq? noncanonical-result a)
                          (acceptable-result? a))
                        (known a)
                      /if
                        (and
                          (elements-eq? noncanonical-result b)
                          (acceptable-result? b))
                        (known b)
                      /if (acceptable-result? noncanonical-result)
                        (known noncanonical-result)
                      /unknown))))))))
      /w- equivalent?-promise
        (delay
          (and
            (inhabitant-shallowly-chaperone-of? b a)
            (inhabitant-shallowly-chaperone-of? a b)))
      /w- ==-acceptable-result?
        (fn v
          (and
            (force equivalent?-promise)
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
          #:on-join-smoosh-result-knowable-promise-maybe-knowable-promise
          (on-smoosh-result-knowable-promise-maybe-knowable-promise
            (fn v
              (and
                (inhabitant-shallowly-chaperone-of? v a)
                (inhabitant-shallowly-chaperone-of? v b))))
          #:on-meet-smoosh-result-knowable-promise-maybe-knowable-promise
          (on-smoosh-result-knowable-promise-maybe-knowable-promise
            (fn v
              (and
                (or (eq? v a) (eq? v b))
                (inhabitant-shallowly-chaperone-of? a v)
                (inhabitant-shallowly-chaperone-of? b v))))
          #:on-==-smoosh-result-knowable-promise-maybe-knowable-promise
          (on-smoosh-result-knowable-promise-maybe-knowable-promise
            ==-acceptable-result?)
          #:on-path-related-smoosh-result-knowable-promise-maybe-knowable-promise
          (on-smoosh-result-knowable-promise-maybe-knowable-promise
            path-related-acceptable-result?))
        (smoosh-and-comparison-of-two-reports-map report-2+
          #:on-check-result-knowable-promise
          (on-check-result-knowable-promise #t #t)
          #:on-smoosh-result-knowable-promise-maybe-knowable-promise
          (on-smoosh-result-knowable-promise-maybe-knowable-promise
            ==-acceptable-result?))))
    
    ))

; This is an appropriate `prop:expressly-smooshable-dynamic-type`
; implementation for mutable tuple data structures and their
; chaperones, information-ordered in a way that's consistent with
; `chaperone-of?`.
;
; Level 0:
;   path-related, join, meet, ==:
;     Same as the description of level 1 path-related.
;   <=, >=:
;     Same as the description of level 1 path-related as a check.
; Level 1:
;   path-related, join, meet, ==:
;     If the operands do not both pass the given `inhabitant?`
;     predicate, then unknown.
;     
;     Otherwise, if the operands are not `equal-always?`, then a known
;     nothing.
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
;     If the operands do not both pass the given `inhabitant?`
;     predicate, then unknown.
;     
;     Otherwise, if the operands are not `equal-always?`, then a known
;     `#f`.
;     
;     Otherwise, if the element we're proposing to be greater is
;     `chaperone-of?` the other one, then a known `#t`.
;     
;     Otherwise, unknown.
;     
;     (We'll allow for the possibility that values which differ only
;     in the failure of a chaperone-of check could actually pass the
;     chaperone-of check in the future when they're written to more
;     diligently reusing chaperone-wrapped values. This kind of
;     rewrite may be more feasible if and when Racket introduces
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
; TODO SMOOSH: Consider exporting this.
;
(define/own-contract
  (make-expressly-smooshable-dynamic-type-impl-for-mutable
    #:inhabitant? inhabitant?)
  (-> #:inhabitant? (-> any/c boolean?)
    expressly-smooshable-dynamic-type-impl?)
  (make-expressly-smooshable-dynamic-type-impl
    
    #:get-smoosh-of-zero-report
    (fn self
      (uninformative-smoosh-reports))
    
    #:get-smoosh-of-one-report
    (fn self a
      (expect (inhabitant? a) #t (uninformative-smoosh-reports)
      /constant-smoosh-reports
        (delay/strict /known /just /delay/strict /known a)))
    
    #:get-smoosh-and-comparison-of-two-report
    (fn self b-dt a b
      (expect (inhabitant? a) #t
        (uninformative-smoosh-and-comparison-of-two-reports)
      /expect (inhabitant? b) #t
        (uninformative-smoosh-and-comparison-of-two-reports)
      /if (not /equal-always? a b)
        (false-smoosh-and-comparison-of-two-reports)
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
              (boolean-and-knowable-thunk-zip /list
                (fn /boolean-or-knowable-thunk-zip /list
                  (fn /known /not should-a-be-small?)
                  (fn /falsable->uninformative-knowable
                    (inhabitant-chaperone-of? b a)))
                (fn /boolean-or-knowable-thunk-zip /list
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
      /w- equivalent?-promise
        (delay
          (and
            (inhabitant-chaperone-of? b a)
            (inhabitant-chaperone-of? a b)))
      /w- ==-acceptable-result?
        (fn v
          (force equivalent?-promise))
      /w- path-related-acceptable-result?
        (fn v
          #t)
      /stream*
        (smoosh-and-comparison-of-two-report-zip-map (list)
          #:on-check-result-knowable-promise
          (on-check-result-knowable-promise #f #f)
          #:on-smoosh-result-knowable-promise-maybe-knowable-promise
          (on-smoosh-result-knowable-promise-maybe-knowable-promise
            path-related-acceptable-result?))
        (smoosh-and-comparison-of-two-report-zip-map (list)
          #:on-<=?-knowable-promise
          (on-check-result-knowable-promise #t #f)
          #:on->=?-knowable-promise
          (on-check-result-knowable-promise #f #t)
          #:on-join-smoosh-result-knowable-promise-maybe-knowable-promise
          (on-smoosh-result-knowable-promise-maybe-knowable-promise
            (fn v
              (and
                (inhabitant-chaperone-of? v a)
                (inhabitant-chaperone-of? v b))))
          #:on-meet-smoosh-result-knowable-promise-maybe-knowable-promise
          (on-smoosh-result-knowable-promise-maybe-knowable-promise
            (fn v
              (and
                (inhabitant-chaperone-of? a v)
                (inhabitant-chaperone-of? b v))))
          #:on-==-smoosh-result-knowable-promise-maybe-knowable-promise
          (on-smoosh-result-knowable-promise-maybe-knowable-promise
            ==-acceptable-result?)
          #:on-path-related-smoosh-result-knowable-promise-maybe-knowable-promise
          (on-smoosh-result-knowable-promise-maybe-knowable-promise
            path-related-acceptable-result?))
        (smoosh-and-comparison-of-two-reports-zip-map (list)
          #:on-check-result-knowable-promise
          (on-check-result-knowable-promise #t #t)
          #:on-smoosh-result-knowable-promise-maybe-knowable-promise
          (on-smoosh-result-knowable-promise-maybe-knowable-promise
            ==-acceptable-result?))))
    
    ))

; TODO SMOOSH: Consider exporting this. If we export it, consider
; whether we want to give it better smooshing behavior using
; `prop:expressly-smooshable-dynamic-type` and/or implement
; `prop:equal+hash` for it.
;
; This is an appropriate dynamic type of immutable vectors and their
; chaperones, information-ordered in a way that's consistent with
; `chaperone-of?` as long as the elements' information orderings are.
; This is an instance of
; `make-expressly-smooshable-dynamic-type-impl-from-list-isomorphism`.
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
  
  (#:prop prop:expressly-smooshable-dynamic-type
    (make-expressly-smooshable-dynamic-type-impl-from-list-isomorphism
      
      #:self-get-any-dynamic-type
      (dissectfn (immutable-vector-dynamic-type any-dt)
        any-dt)
      
      #:inhabitant? (fn v /and (vector? v) (immutable? v))
      #:->list (fn v /vector->list v)
      
      #:example-and-list->
      (fn example lst
        (vector->immutable-vector /list->vector lst))
      
      #:copy (fn v /vector->immutable-vector /vector-copy v))))

; TODO SMOOSH: Consider exporting this. If we export it, consider
; whether we want to give it better smooshing behavior using
; `prop:expressly-smooshable-dynamic-type` and/or implement
; `prop:equal+hash` for it.
;
; This is an appropriate dynamic type of mutable vectors and their
; chaperones, information-ordered in a way that's consistent with
; `chaperone-of?`. This is an instance of
; `make-expressly-smooshable-dynamic-type-impl-for-mutable`.
;
(define-imitation-simple-struct (mutable-vector-dynamic-type?)
  mutable-vector-dynamic-type
  'mutable-vector-dynamic-type (current-inspector) (auto-write)
  
  (#:prop prop:expressly-smooshable-dynamic-type
    (make-expressly-smooshable-dynamic-type-impl-for-mutable
      #:inhabitant? (fn v /and (vector? v) (not /immutable? v)))))

; TODO SMOOSH: Consider exporting this. If we export it, consider
; whether we want to give it better smooshing behavior using
; `prop:expressly-smooshable-dynamic-type` and/or implement
; `prop:equal+hash` for it.
;
; This is an appropriate dynamic type of immutable boxes and their
; chaperones, information-ordered in a way that's consistent with
; `chaperone-of?` as long as the elements' information orderings are.
; This is an instance of
; `make-expressly-smooshable-dynamic-type-impl-from-list-isomorphism`.
;
(define-imitation-simple-struct
  (immutable-box-dynamic-type?
    immutable-box-dynamic-type-get-any-dynamic-type)
  immutable-box-dynamic-type
  'immutable-box-dynamic-type (current-inspector) (auto-write)
  
  (#:prop prop:expressly-smooshable-dynamic-type
    (make-expressly-smooshable-dynamic-type-impl-from-list-isomorphism
      
      #:self-get-any-dynamic-type
      (dissectfn (immutable-box-dynamic-type any-dt)
        any-dt)
      
      #:inhabitant? (fn v /and (box? v) (immutable? v))
      #:->list (fn v /list /unbox v)
      
      #:example-and-list->
      (fn example lst
        (dissect lst (list e)
        /box-immutable e))
      
      #:copy (fn v /box-immutable /unbox v)
      
      #:get-smoosh-of-zero-report
      (fn self
        (dissect self (cons-dynamic-type any-dt)
        /smoosh-reports-map
          (dynamic-type-get-smoosh-of-zero-report any-dt)
          #:on-smoosh-result-knowable-promise-maybe-knowable-promise
          (fn kpmkp
            (promise-map kpmkp /fn kpmk
              (knowable-map kpmk /fn kpm
                (maybe-map kpm /fn kp
                  (promise-map kp /fn k
                    (knowable-map k /fn result
                      (box-immutable result)))))))))
      
      )))

; TODO SMOOSH: Consider exporting this. If we export it, consider
; whether we want to give it better smooshing behavior using
; `prop:expressly-smooshable-dynamic-type` and/or implement
; `prop:equal+hash` for it.
;
; This is an appropriate dynamic type of mutable boxes and their
; chaperones, information-ordered in a way that's consistent with
; `chaperone-of?`. This is an instance of
; `make-expressly-smooshable-dynamic-type-impl-for-mutable`.
;
(define-imitation-simple-struct (mutable-box-dynamic-type?)
  mutable-box-dynamic-type
  'mutable-box-dynamic-type (current-inspector) (auto-write)
  
  (#:prop prop:expressly-smooshable-dynamic-type
    (make-expressly-smooshable-dynamic-type-impl-for-mutable
      #:inhabitant? (fn v /and (box? v) (not /immutable? v)))))

; TODO SMOOSH: Consider exporting this. If we export it, consider
; whether we want to give it better smooshing behavior using
; `prop:expressly-smooshable-dynamic-type` and/or implement
; `prop:equal+hash` for it.
;
; This is an appropriate dynamic type of immutable prefab structs and
; their chaperones, information-ordered in a way that's consistent
; with `chaperone-of?` as long as the elements' information orderings
; are. This is an instance of
; `make-expressly-smooshable-dynamic-type-impl-from-list-isomorphism`.
;
(define-imitation-simple-struct
  (immutable-prefab-struct-dynamic-type?
    immutable-prefab-struct-dynamic-type-get-any-dynamic-type)
  immutable-prefab-struct-dynamic-type
  'immutable-prefab-struct-dynamic-type (current-inspector)
  (auto-write)
  
  (#:prop prop:expressly-smooshable-dynamic-type
    (make-expressly-smooshable-dynamic-type-impl-from-list-isomorphism
      
      #:self-get-any-dynamic-type
      (dissectfn (immutable-prefab-struct-dynamic-type any-dt)
        any-dt)
      
      #:inhabitant? immutable-prefab-struct?
      #:->list (fn v /cdr /vector->list /struct->vector v)
      #:example-and-list->
      (fn example lst
        (apply make-prefab-struct (prefab-struct-key example) lst)))))

; TODO SMOOSH: Consider exporting this. If we export it, consider
; whether we want to give it better smooshing behavior using
; `prop:expressly-smooshable-dynamic-type` and/or implement
; `prop:equal+hash` for it.
;
; This is an appropriate dynamic type of mutable prefab structs and
; their chaperones, information-ordered in a way that's consistent
; with `chaperone-of?`. This is an instance of
; `make-expressly-smooshable-dynamic-type-impl-for-mutable`.
;
(define-imitation-simple-struct (mutable-prefab-struct-dynamic-type?)
  mutable-prefab-struct-dynamic-type
  'mutable-prefab-struct-dynamic-type (current-inspector) (auto-write)
  
  (#:prop prop:expressly-smooshable-dynamic-type
    (make-expressly-smooshable-dynamic-type-impl-for-mutable
      #:inhabitant? mutable-prefab-struct?)))

(define/own-contract base-readable-cases
  (listof (list/c (-> any/c boolean?) (-> any/c any/c)))
  (list
    (list
      base-readable-discrete-atom?
      (fn any-dt /base-readable-discrete-atom-dynamic-type any-dt))
    (list non-nan-number? (fn any-dt /non-nan-number-dynamic-type))
    (list
      non-nan-extflonum?
      (fn any-dt /non-nan-extflonum-dynamic-type))
    (list pair? (fn any-dt /cons-dynamic-type any-dt))
    (list
      (fn v /and (vector? v) (immutable? v))
      (fn any-dt /immutable-vector-dynamic-type any-dt))
    (list
      (fn v /and (vector? v) (not /immutable? v))
      (fn any-dt /mutable-vector-dynamic-type))
    (list
      (fn v /and (box? v) (immutable? v))
      (fn any-dt /immutable-box-dynamic-type any-dt))
    (list
      (fn v /and (box? v) (not /immutable? v))
      (fn any-dt /mutable-box-dynamic-type))
    (list
      immutable-prefab-struct?
      (fn any-dt /immutable-prefab-struct-dynamic-type any-dt))
    (list
      mutable-prefab-struct?
      (fn any-dt /mutable-prefab-struct-dynamic-type))
    ; TODO SMOOSH: Add more cases here.
    ))

(define/own-contract (base-readable? v)
  (-> any/c boolean?)
  (list-any base-readable-cases /dissectfn (list check? dt)
    (check? v)))

; TODO SMOOSH: Consider exporting this. If we export it, consider
; whether we want to give it better smooshing behavior using
; `prop:expressly-smooshable-dynamic-type` and/or implement
; `prop:equal+hash` for it.
(define-imitation-simple-struct
  (base-readable-dynamic-type?
    base-readable-dynamic-type-get-any-dynamic-type)
  base-readable-dynamic-type
  'base-readable-dynamic-type (current-inspector) (auto-write)
  
  (#:prop prop:expressly-smooshable-dynamic-type
    (make-expressly-smooshable-dynamic-type-impl
      
      #:get-smoosh-of-zero-report
      (fn self
        (uninformative-smoosh-reports))
      
      #:get-smoosh-of-one-report
      (fn self a
        (dissect self (base-readable-dynamic-type any-dt)
        /w-loop next base-readable-cases base-readable-cases
          (expect base-readable-cases
            (cons base-readable-case base-readable-cases)
            (uninformative-smoosh-reports)
          /dissect base-readable-case (list check? dt)
          /if (check? a)
            (dynamic-type-get-smoosh-of-one-report (dt any-dt) a)
          /next base-readable-cases)))
      
      #:get-smoosh-and-comparison-of-two-report
      (fn self b-dt a b
        (dissect self (base-readable-dynamic-type any-dt)
        /w-loop next base-readable-cases base-readable-cases
          (expect base-readable-cases
            (cons base-readable-case base-readable-cases)
            (uninformative-smoosh-and-comparison-of-two-reports)
          /dissect base-readable-case (list check? dt)
          /match (list (check? a) (check? b))
            [ (list #t #t)
              (w- a-dt (dt any-dt)
              /dynamic-type-get-smoosh-and-comparison-of-two-report
                a-dt a b)]
            [ (list #t #f)
              (if
                (list-any base-readable-cases /dissectfn
                  (list check? dt)
                  (check? b))
                (false-smoosh-and-comparison-of-two-reports)
                (uninformative-smoosh-and-comparison-of-two-reports))]
            [ (list #f #t)
              (if
                (list-any base-readable-cases /dissectfn
                  (list check? dt)
                  (check? a))
                (false-smoosh-and-comparison-of-two-reports)
                (uninformative-smoosh-and-comparison-of-two-reports))]
            [(list #f #f) (next base-readable-cases)])))
      
      )))

(define/own-contract
  (on-knowable-smoosh-result-knowable-promise-maybe-knowable-promise
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

; TODO SMOOSH: Consider exporting this. If we export it, consider
; whether we want to give it better smooshing behavior using
; `prop:expressly-smooshable-dynamic-type` and/or implement
; `prop:equal+hash` for it.
;
; This is an appropriate dynamic type of `knowable?` values,
; information-ordered so that values that aren't `known?` are
; considered to represent less information than values that are.
;
; Level 1:
;   
;
; Level 0:
;   path-related, join, meet, ==:
;     If the operands are not both `known?` values, then unknown.
;     
;     Otherwise, the result of performing the same smoosh on their
;     values, then wrapping the result in a `known?` if it's
;     successful.
;  <=, >=:
;     If the operands are not both `known?` values, then unknown.
;     
;     Otherwise, the result of performing the same check on their
;     values.
; Level 1+:
;   path-related, join, meet, ==:
;     If there are zero operands, then unknown.
;     
;     If the operands are not both `knowable?` values, then unknown.
;     
;     Otherwise, if the operands are both `known?` values, then the
;     result of performing the same smoosh on their values, then
;     wrapping the result in a `known?` if it's successful.
;     
;     Otherwise, if the operands are both `unknown?` values, then the
;     first operand.
;     
;     Otherwise, for:
;       path-related:
;         The first operand.
;       join:
;         The `known?` operand.
;       meet:
;         The `unknown?` operand.
;       ==:
;         A known nothing.
;  <=, >=:
;     If the operands are not both `knowable?` values, then unknown.
;     
;     Otherwise, if the operands are both `known?` values, then the
;     result of performing the same check on their values.
;     
;     Otherwise, a boolean indicating whether the element we're
;     proposing to be lesser is `unknown?`.
;
(define-imitation-simple-struct
  (knowable-dynamic-type? knowable-dynamic-type-get-any-dynamic-type)
  knowable-dynamic-type
  'knowable-dynamic-type (current-inspector) (auto-write)
  
  (#:prop prop:expressly-smooshable-dynamic-type
    (make-expressly-smooshable-dynamic-type-impl
      
      #:get-smoosh-of-zero-report
      (fn self
        (dissect self (knowable-dynamic-type any-dt)
        /dissect
          (smoosh-reports-map
            (dynamic-type-get-smoosh-of-zero-report any-dt)
            #:on-smoosh-result-knowable-promise-maybe-knowable-promise
            on-knowable-smoosh-result-knowable-promise-maybe-knowable-promise)
          (stream* report-0 report-1+)
        /stream* report-0 /uninformative-smoosh-reports))
      
      #:get-smoosh-of-one-report
      (fn self a
        (dissect self (knowable-dynamic-type any-dt)
        /expect (knowable? a) #t (uninformative-smoosh-reports)
        /mat a (known a-value)
          (smoosh-reports-map
            (dynamic-type-get-smoosh-of-one-report any-dt a-value)
            #:on-result-knowable-promise-maybe-knowable-promise
            on-knowable-smoosh-result-knowable-promise-maybe-knowable-promise)
        /stream* (uninformative-smoosh-report)
          (constant-smoosh-reports
            (delay/strict /known /just /delay/strict /known a))))
      
      ; TODO NOW: From here.
      #:get-smoosh-and-comparison-of-two-report
      (fn self b-dt a b
        (dissect self (knowable-dynamic-type any-dt)
        /expect (knowable? a) #t
          (uninformative-smoosh-and-comparison-of-two-reports)
        /expect (knowable? b) #t
          (uninformative-smoosh-and-comparison-of-two-reports)
        /mat (list a b) (list (known a-value) (known b-value))
          (smoosh-reports-map
            (dynamic-type-get-smoosh-and-comparison-of-two-report
              any-dt a-value b-value)
            #:on-result-knowable-promise-maybe-knowable-promise
            on-knowable-smoosh-result-knowable-promise-maybe-knowable-promise)
        /stream* (uninformative-smoosh-and-comparison-of-two-report)
          (if (or (known? a) (known? b))
            (smoosh-reports-zip-map (list)
              
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
                  (delay/strict /known /just /delay/strict /known result)))
              
              #:on-meet-knowable-promise-maybe-knowable-promise
              (w- result (if (known? a) b a)
                (dissectfn (list)
                  (delay/strict /known /just /delay/strict /known b)))
              
              #:on-==-knowable-promise-maybe-knowable-promise
              (dissectfn (list)
                (delay/strict /known /nothing))
              
              #:on-path-related-knowable-promise-maybe-knowable-promise
              (dissectfn (list)
                (delay/strict /known /just /delay/strict /known a))
              
              )
            (smoosh-reports-zip-map (list)
              
              #:on-check-result-knowable-promise
              (dissectfn (list)
                (delay/strict /known #t))
              
              #:on-smoosh-result-knowable-promise-maybe-knowable-promise
              (dissectfn (list)
                (delay/strict /known /just /delay/strict /known a))
              
              ))))
      
      )))

(define/own-contract known-to-lathe-comforts-data-cases
  (listof (list/c (-> any/c boolean?) (-> any/c any/c)))
  (list
    (list
      base-readable?
      (fn any-dt /base-readable-dynamic-type any-dt))
    (list
      knowable?
      (fn any-dt /knowable-dynamic-type any-dt))
    ; TODO SMOOSH: Add more cases here.
    ))

(define/own-contract (known-to-lathe-comforts-data? v)
  (-> any/c boolean?)
  (list-any known-to-lathe-comforts-data-cases /dissectfn
    (list check? dt)
    (check? v)))

; TODO SMOOSH: Consider exporting this. If we export it, consider
; whether we want to give it better smooshing behavior using
; `prop:expressly-smooshable-dynamic-type` and/or implement
; `prop:equal+hash` for it.
(define-imitation-simple-struct
  (known-to-lathe-comforts-data-dynamic-type?
    known-to-lathe-comforts-data-dynamic-type-get-any-dynamic-type)
  known-to-lathe-comforts-data-dynamic-type
  'known-to-lathe-comforts-data-dynamic-type (current-inspector)
  (auto-write)
  
  (#:prop prop:expressly-smooshable-dynamic-type
    (make-expressly-smooshable-dynamic-type-impl
      
      #:get-smoosh-of-zero-report
      (fn self
        (uninformative-smoosh-reports))
      
      #:get-smoosh-of-one-report
      (fn self a
        (dissect self
          (known-to-lathe-comforts-data-dynamic-type any-dt)
        /w-loop next
          known-to-lathe-comforts-data-cases
          known-to-lathe-comforts-data-cases
          (expect known-to-lathe-comforts-data-cases
            (cons known-to-lathe-comforts-data-case
              known-to-lathe-comforts-data-cases)
            (uninformative-smoosh-reports)
          /dissect known-to-lathe-comforts-data-case (list check? dt)
          /if (check? a)
            (dynamic-type-get-smoosh-of-one-report (dt any-dt) a)
          /next known-to-lathe-comforts-data-cases)))
      
      #:get-smoosh-and-comparison-of-two-report
      (fn self b-dt a b
        (dissect self
          (known-to-lathe-comforts-data-dynamic-type any-dt)
        /w-loop next
          known-to-lathe-comforts-data-cases
          known-to-lathe-comforts-data-cases
          (expect known-to-lathe-comforts-data-cases
            (cons known-to-lathe-comforts-data-case
              known-to-lathe-comforts-data-cases)
            (uninformative-smoosh-and-comparison-of-two-reports)
          /dissect known-to-lathe-comforts-data-case (list check? dt)
          /match (list (check? a) (check? b))
            [ (list #t #t)
              (w- a-dt (dt any-dt)
              /dynamic-type-get-smoosh-and-comparison-of-two-report
                a-dt a b)]
            [ (list #t #f)
              (if
                (list-any known-to-lathe-comforts-data-cases
                  (dissectfn (list check? dt)
                    (check? b)))
                (false-smoosh-and-comparison-of-two-reports)
                (uninformative-smoosh-and-comparison-of-two-reports))]
            [ (list #f #t)
              (if
                (list-any known-to-lathe-comforts-data-cases
                  (dissectfn (list check? dt)
                    (check? a)))
                (false-smoosh-and-comparison-of-two-reports)
                (uninformative-smoosh-and-comparison-of-two-reports))]
            [ (list #f #f)
              (next known-to-lathe-comforts-data-cases)])))
      
      )))

; TODO SMOOSH: Consider whether we want to give this better smooshing
; behavior using `prop:expressly-smooshable-dynamic-type` and/or
; implement `prop:equal+hash` for it.
(define-imitation-simple-struct (any-dynamic-type?) any-dynamic-type
  'any-dynamic-type (current-inspector) (auto-write)
  
  (#:prop prop:expressly-smooshable-dynamic-type
    (make-expressly-smooshable-dynamic-type-impl
      
      #:get-smoosh-of-zero-report
      (fn self
        (uninformative-smoosh-reports))
      
      #:get-smoosh-of-one-report
      (fn self a
        (w- a-dt (get-dynamic-type-with-default-bindings a)
        /dynamic-type-get-smoosh-of-one-report a-dt a))
      
      #:get-smoosh-and-comparison-of-two-report
      (fn self b-dt a b
        (w- a-dt (get-dynamic-type-with-default-bindings a)
        /w- dead-dt (dead-end-dynamic-type)
        /smoosh-and-comparison-of-two-reports-join /list
          (dynamic-type-get-smoosh-and-comparison-of-two-report
            a-dt b-dt a b)
          (smoosh-and-comparison-of-two-reports-flip
            (dynamic-type-get-smoosh-and-comparison-of-two-report
              b-dt dead-dt b a))))
      
      )))
(ascribe-own-contract any-dynamic-type? (-> any/c boolean?))


; TODO SMOOSH: Implement the following parts of the API outlined in
; the last part of notes/2024-03-20-squashable-object-system.txt:
;
; smooshable-dynamic-type-get-info-smooshable-dynamic-type
; make-empty-immutable-total-order-<=-based-dict
; make-empty-immutable-trie-dict
;
; We're not implementing the mutable dicts. (If we need them, we're
; just going to model them as mutable boxes containing immutable
; dicts, or tack on the mutable dict stuff as an afterthought.)
;
; For information-ordering-indexed data structures, since we've ended
; up having only one concrete `gloss?` type, we have
; `path-related-wrapper` and `info-wrapper` wrapper types for our key
; values to indicate when they should be compared according to
; path-relatedness or according to their information ordering.

; TODO SMOOSH: Here we summarize some of our "implement smooshing"
; tasks. Implement smooshing and better `prop:equal+hash` equality for
; these types:
;
;   - Various types that can result from the default Racket reader, as
;     well as their corresponding mutable types where these exist.
;     We're referring to these as `base-readable?` values, but so far
;     we only handle a few of the cases.
;
;      - (Done) Booleans. (There is some precedent for considering
;        booleans to be ordered, but we won't consider any order to be
;        specified by default, at least for now.)
;
;      - (Done) Numbers with no NaN parts, ordered in a way consistent
;        with `<=` and `=`, and treating checks as having unknown
;        results when they involve nontrivial complex numbers and not
;        all the arguments are `=`.
;
;      - (Done) Non-NaN extflonums, ordered in a way consistent with
;        `extfl<=` and `extfl=`.
;
;      - (Done) Characters.
;
;      - (Done) Symbols.
;
;      - (Done) Keywords.
;
;      - (Done) Regular expressions.
;
;      - (Done) Compiled code expressions (`compiled-expression?`).
;
;      - (Done) Mutable and immutable strings.
;
;      - (Done) Mutable and immutable byte strings.
;
;      - Flvectors. (NOTE: As of Racket 8.12 [cs], the implementation
;        of `equal-always?` for this type seems to be incorrect, so
;        we're not supporting this yet.)
;
;      - Fxvectors. (NOTE: As of Racket 8.12 [cs], the implementation
;        of `equal-always?` for this type seems to be incorrect, so
;        we're not supporting this yet.)
;
;      - (Done) Empty lists.
;
;      - (Done) Cons cells.
;
;      - (Done) Mutable and immutable boxes.
;
;      - (Done) Mutable and immutable vectors.
;
;      - (Done) Prefab structs, at least of the kinds supported by
;        Racket so far.
;
;      - Hash tables of various kinds.
;
;      - Potentially others in future versions of Racket. The above
;        list is up-to-date as of Racket 8.12.
;
;   - Types defined here in smoosh.rkt.
;
;     - (Done) `knowable?`
;
;     - `path-related-wrapper?`
;
;     - `info-wrapper?`
;
;     - `gloss?`
;
;     - `dynamic-type-var-for-any-dynamic-type?`
;
;     - `equalw-gloss-key-wrapper?` (which may not need smooshing if
;       we treat it kind of like an implementation detail, but does
;       definitely need better `prop:equal+hash` equality).
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
;       - `dead-end-dynamic-type?`
;
;       - `compare-by-predicates-dynamic-type?`
;
;       - `base-readable-discrete-atom-dynamic-type?`
;
;       - `non-nan-number-dynamic-type?`
;
;       - `non-nan-extflonum-dynamic-type?`
;
;       - `cons-dynamic-type?`
;
;       - `immutable-vector-dynamic-type?`
;
;       - `mutable-vector-dynamic-type?`
;
;       - `immutable-box-dynamic-type?`
;
;       - `mutable-box-dynamic-type?`
;
;       - `immutable-prefab-struct-dynamic-type?`
;
;       - `mutable-prefab-struct-dynamic-type?`
;
;       - `base-readable-dynamic-type?`
;
;       - `knowable-dynamic-type?`
;
;       - `known-to-lathe-comforts-data-dynamic-type?`
;
;       - `any-dynamic-type?`
;
;   - Types defined by Lathe Comforts that this smooshing framework
;     uses.
;
;     - `maybe?`
;
;   - Types defined by Lathe Comforts even if this smooshing framework
;     doesn't use them.
;
;     - `obstinacy?`, for instance. Potentially others; we haven't
;       made a comprehensive list here yet.

; TODO SMOOSH: Implement usability as a `gloss?` key for all our
; smooshable values. Curiously, we have been implementing
; smooshability as though values return unknown results when smooshed
; with values they don't recognize, but `gloss?` keys rely on values
; knowing how to identify themselves by a dynamic type tag that can be
; distinguished from other tags. We may have some contradictory
; thoughts to iron out here.
