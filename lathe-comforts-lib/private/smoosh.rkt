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
(require /only-in lathe-comforts/struct
  auto-equal auto-write define-imitation-simple-generics
  define-imitation-simple-struct)
(require /only-in lathe-comforts/match match/c)
(require /only-in lathe-comforts/maybe
  just just? maybe? maybe/c nothing)


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
  expressly-has-dynamic-type-impl?
  prop:expressly-has-dynamic-type
  make-expressly-has-dynamic-type-impl
  uninformative-dynamic-type
  dynamic-type-var-for-any-dynamic-type?)
(provide
  dynamic-type-var-for-any-dynamic-type)
(provide /own-contract-out
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
  make-smoosh-and-comparison-of-two-report-impl
  smooshable-dynamic-type?
  smooshable-dynamic-type-impl?
  smooshable-dynamic-type-get-smoosh-of-zero-report
  smooshable-dynamic-type-get-smoosh-of-one-report
  smooshable-dynamic-type-get-smoosh-and-comparison-of-two-report
  prop:smooshable-dynamic-type
  make-smooshable-dynamic-type-impl
  uninformative-smoosh-report
  uninformative-smoosh-reports
  uninformative-smoosh-and-comparison-of-two-report
  uninformative-smoosh-and-comparison-of-two-reports
  smoosh-and-comparison-of-two-report-flip
  smoosh-and-comparison-of-two-reports-flip
  constant-smoosh-report
  constant-smoosh-reports
  constant-smoosh-and-comparison-of-two-report
  constant-smoosh-and-comparison-of-two-reports
  compare-by-predicates-dynamic-type
  equalw-gloss-key-wrapper
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

; TODO SMOOSH: Give this better smooshing behavior using
; `prop:smooshable-dynamic-type`.
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
  make-custom-gloss-key-report-from-various-unkeyworded
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
  (make-custom-gloss-key-report-from-various-unkeyworded
    get-==-glossesque-sys
    get-path-related-glossesque-sys))

; TODO SMOOSH: Give this better smooshing behavior using
; `prop:smooshable-dynamic-type`.
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
; `prop:smooshable-dynamic-type`.
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
  ; TODO SMOOSH: These uses of `get-dynamic-type`,
  ; `gloss-set-maybe-knowable`, `gloss-union-of-zero`,
  ; `dynamic-type-var-for-any-dynamic-type`, and `any-dynamic-type`
  ; are forward references. See if we can untangle them.
  (get-dynamic-type
    (known-value /gloss-set-maybe-knowable (gloss-union-of-zero)
      (dynamic-type-var-for-any-dynamic-type)
      (any-dynamic-type))
    v))

; TODO SMOOSH: Give this better smooshing behavior using
; `prop:smooshable-dynamic-type`.
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
  uninformative-dynamic-type-unguarded
  'uninformative-dynamic-type (current-inspector) (auto-write))

(define/own-contract (uninformative-dynamic-type)
  (-> any/c)
  (uninformative-dynamic-type-unguarded))

; TODO SMOOSH: Give this better smooshing behavior using
; `prop:smooshable-dynamic-type`.
(define-imitation-simple-struct
  (dynamic-type-var-for-any-dynamic-type?)
  dynamic-type-var-for-any-dynamic-type
  'dynamic-type-var-for-any-dynamic-type (current-inspector)
  (auto-write)
  (auto-equal)
  (#:prop prop:equalw-gloss-key /make-equalw-gloss-key-impl))
(ascribe-own-contract dynamic-type-var-for-any-dynamic-type?
  (-> any/c boolean?))

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

(define-imitation-simple-generics
  smooshable-dynamic-type? smooshable-dynamic-type-impl?
  (#:method smooshable-dynamic-type-get-smoosh-of-zero-report
    (#:this))
  (#:method smooshable-dynamic-type-get-smoosh-of-one-report
    (#:this)
    ())
  (#:method
    smooshable-dynamic-type-get-smoosh-and-comparison-of-two-report
    (#:this)
    ()
    ()
    ())
  prop:smooshable-dynamic-type
  make-smooshable-dynamic-type-from-various-unkeyworded
  'smooshable-dynamic-type 'smooshable-dynamic-type-impl (list))
(ascribe-own-contract smooshable-dynamic-type? (-> any/c boolean?))
(ascribe-own-contract smooshable-dynamic-type-impl?
  (-> any/c boolean?))
(ascribe-own-contract
  smooshable-dynamic-type-get-smoosh-of-zero-report
  (-> smooshable-dynamic-type?
    ; Each report in the infinite sequence gives the smoosh identity
    ; elements, first for the type's bespoke notion of ordering, then
    ; for the information ordering, then for the information ordering
    ; of the information ordering representatives, and so on.
    (sequence/c smoosh-report?)))
(ascribe-own-contract smooshable-dynamic-type-get-smoosh-of-one-report
  (-> smooshable-dynamic-type? any/c
    ; Each report in the infinite sequence gives the smoosh identity
    ; elements, first for the type's bespoke notion of ordering, then
    ; for the information ordering, then for the information ordering
    ; of the information ordering representatives, and so on.
    (sequence/c smoosh-report?)))
(ascribe-own-contract
  smooshable-dynamic-type-get-smoosh-and-comparison-of-two-report
  (->
    ; lhs type
    smooshable-dynamic-type?
    ; rhs type (usually dispatched to next, if this one can't fully
    ; determine the results)
    smooshable-dynamic-type?
    ; lhs
    any/c
    ; rhs
    any/c
    ; For each report in the infinite sequence, the next report says
    ; not only whether they smoosh along that one's == but also, only
    ; if they do, how their information ordering representatives
    ; smoosh along their information ordering.
    (sequence/c smoosh-and-comparison-of-two-report?)))
(ascribe-own-contract prop:smooshable-dynamic-type
  (struct-type-property/c smooshable-dynamic-type-impl?))

(define/own-contract
  (make-smooshable-dynamic-type-impl
    #:get-smoosh-of-zero-report get-smoosh-of-zero-report
    #:get-smoosh-of-one-report get-smoosh-of-one-report
    
    #:get-smoosh-and-comparison-of-two-report
    get-smoosh-and-comparison-of-two-report)
  (->
    #:get-smoosh-of-zero-report
    (-> smooshable-dynamic-type? (sequence/c smoosh-report?))
    
    #:get-smoosh-of-one-report
    (-> smooshable-dynamic-type? any/c (sequence/c smoosh-report?))
    
    #:get-smoosh-and-comparison-of-two-report
    (-> smooshable-dynamic-type? smooshable-dynamic-type? any/c any/c
      (sequence/c smoosh-and-comparison-of-two-report?))
    
    smooshable-dynamic-type-impl?)
  (make-smooshable-dynamic-type-from-various-unkeyworded
    get-smoosh-of-zero-report
    get-smoosh-of-one-report
    get-smoosh-and-comparison-of-two-report))


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
  (dead-end-dynamic-type?)
  dead-end-dynamic-type
  'dead-end-dynamic-type (current-inspector) (auto-write)
  (#:prop prop:smooshable-dynamic-type
    (make-smooshable-dynamic-type-impl
      
      #:get-smoosh-of-zero-report
      (fn self
        (uninformative-smoosh-reports))
      
      #:get-smoosh-of-one-report
      (fn self a
        (uninformative-smoosh-reports))
      
      #:get-smoosh-and-comparison-of-two-report
      (fn self b-dt a b
        (uninformative-smoosh-and-comparison-of-two-reports))
      
      )))

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
        (delay
          (knowable-map
            (force result-knowable-promise-maybe-knowable-promise)
          /fn result-knowable-promise-maybe
            (just? result-knowable-promise-maybe))))
      
      #:>=?-knowable-promise
      (dissectfn
        (constant-smoosh-and-comparison-of-two-report-unguarded
          result-knowable-promise-maybe-knowable-promise)
        (delay
          (knowable-map
            (force result-knowable-promise-maybe-knowable-promise)
          /fn result-knowable-promise-maybe
            (just? result-knowable-promise-maybe))))
      
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
  (compare-by-predicates-dynamic-type?
    compare-by-predicates-dynamic-type-getter-for-variant
    compare-by-predicates-dynamic-type-getter-for-predicate
    compare-by-predicates-dynamic-type-getter-for-compare)
  compare-by-predicates-dynamic-type-unguarded
  'compare-by-predicates-dynamic-type (current-inspector) (auto-write)
  (#:prop prop:smooshable-dynamic-type
    (make-smooshable-dynamic-type-impl
      
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

(define/own-contract (equalw-gloss-key-wrapper v)
  (-> any/c any/c)
  (equalw-gloss-key-wrapper-unguarded v))

(define-imitation-simple-struct (any-dynamic-type?) any-dynamic-type
  'any-dynamic-type (current-inspector) (auto-write) (auto-equal)
  
  ; TODO SMOOSH: Remove this pseudocode once we've implemented
  ; something analogous for a real type (though I suppose we might
  ; need to really implement this for cons cells).
  ;
  ; TODO SMOOSH: Before we remove this, make another example similar
  ; to this but for immutable vectors or for immutable boxes instead
  ; of for cons cells. This way, we can determine how `chaperone-of?`
  ; fits into things. It might be a little disappointing; a smooshable
  ; compound type which wants to treat `chaperone-of?` as part of its
  ; information ordering won't necessarily be able to perform most
  ; information ordering comparisons, since Racket doesn't provide a
  ; `chaperone-of?/recur` function or an
  ; `equivalent-according-to-chaperone-of?/recur` function. It does
  ; provide `equal-always?/recur`, so at least we'll be able to check
  ; path-relatedness. When the type doesn't have an "any"-typed
  ; element, `chaperone-of?` without the `/recur` is fine, and we can
  ; imitate `equivalent-according-to-chaperone-of?` without the
  ; `/recur` by calling `chaperone-of?` in both directions.
  ;
  ; NOTE: If this were the type of cons cells, the dynamic type would
  ; be something like this:
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
      /makeshift-struct-instance
        (#:prop prop:smooshable-dynamic-type
          (make-smooshable-dynamic-type-impl
            
            #:get-smoosh-of-zero-report
            (fn self
              (smoosh-reports-map
                (dynamic-type-get-smoosh-of-zero-report any-dt)
                (fn result
                  (cons result result))))
            
            #:get-smoosh-of-one-report
            (fn self a
              (expect a (cons a-car a-cdr)
                (uninformative-smoosh-report)
              /smoosh-reports-zip-map
                (list
                  (dynamic-type-get-smoosh-of-one-report any-dt a-car)
                  (dynamic-type-get-smoosh-of-one-report any-dt a-cdr)
                  )
                #:on-result-knowable-promise-maybe
                (fn result-knowable-promise-maybe-list
                  (maybe-zip-map result-knowable-promise-maybe-list
                    (fn result-knowable-promise-list
                      (promise-zip-map result-knowable-promise-list
                        (fn result-knowable-list
                          (knowable-zip-map result-knowable-list
                            (dissectfn (list result-car result-cdr)
                              (cons result-car result-cdr))))))))))
            
            #:get-smoosh-and-comparison-of-two-report
            (fn self b-dt a b
              (expect a (cons a-car a-cdr)
                (uninformative-smoosh-report)
              /expect b (cons b-car b-cdr)
                (uninformative-smoosh-report)
              /smoosh-and-comparison-of-two-reports-zip-map
                (list
                  (dynamic-type-get-smoosh-and-comparison-of-two-report
                    any-dt a-car b-car)
                  (dynamic-type-get-smoosh-and-comparison-of-two-report
                    any-dt a-cdr b-cdr))
                #:on-check-result
                (dissectfn (list check-result-car check-result-cdr)
                  (and check-result-car check-result-cdr))
                #:on-smoosh-result-knowable-promise-maybe
                (fn result-knowable-promise-maybe-list
                  (maybe-zip-map result-knowable-promise-maybe-list
                    (fn result-knowable-promise-list
                      (promise-zip-map result-knowable-promise-list
                        (fn result-knowable-list
                          (knowable-zip-map result-knowable-list
                            (dissectfn (list result-car result-cdr)
                              (cons result-car result-cdr))))))))))
            
            )))))
  
  (#:prop prop:expressly-has-dynamic-type
    (make-expressly-has-dynamic-type-impl /fn bindings self
      (compare-by-predicates-dynamic-type
        #:get-variant (fn /equalw-gloss-key-wrapper any-dynamic-type?)
        #:get-predicate (fn any-dynamic-type?)
        #:get-compare (fn /fn a b #t))))
  (#:prop prop:smooshable-dynamic-type
    (make-smooshable-dynamic-type-impl
      
      #:get-smoosh-of-zero-report
      (fn self
        (uninformative-smoosh-reports))
      
      #:get-smoosh-of-one-report
      (fn self a
        (w- a-dt (get-dynamic-type-with-default-bindings a)
        ; TODO SMOOSH: Factor out these calls to
        ; `smooshable-dynamic-type?` and
        ; `smooshable-dynamic-type-get-smoosh-of-one-report` into a
        ; utility `dynamic-type-get-smoosh-of-one-report`.
        /if (smooshable-dynamic-type? a-dt)
          (smooshable-dynamic-type-get-smoosh-of-one-report a-dt a)
          (uninformative-smoosh-reports)))
      
      #:get-smoosh-and-comparison-of-two-report
      (fn self b-dt a b
        (w- a-dt (get-dynamic-type-with-default-bindings a)
        ; TODO SMOOSH: Factor out these calls to
        ; `smooshable-dynamic-type?` and
        ; `smooshable-dynamic-type-get-smoosh-and-comparison-of-two-report`
        ; into a utility
        ; `dynamic-type-get-smoosh-and-comparison-of-two-report`. I
        ; think we'll ultimately want to zip together the `a b` result
        ; and the flipped `b a` result, leaving the latter's promises
        ; undisturbed unless the former returns `unknown?` values.
        /if (smooshable-dynamic-type? a-dt)
          (smooshable-dynamic-type-get-smoosh-and-comparison-of-two-report
            a-dt b-dt a b)
          (smoosh-and-comparison-of-two-reports-flip
            (smooshable-dynamic-type-get-smoosh-and-comparison-of-two-report
              b-dt (dead-end-dynamic-type) b a))))
      
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
