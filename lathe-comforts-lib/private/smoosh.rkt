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


(require /for-syntax lathe-comforts/private/shim)
(begin-for-syntax /init-shim)

(require lathe-comforts/private/shim)
(init-shim)

(require lathe-comforts)
(require lathe-comforts/contract)
(require lathe-comforts/hash)
(require lathe-comforts/knowable)
(require lathe-comforts/list)
(require lathe-comforts/match)
(require lathe-comforts/math)
(require lathe-comforts/maybe)
(require lathe-comforts/promise)
(require lathe-comforts/sequence)
(require lathe-comforts/string)
(require lathe-comforts/struct)
(require lathe-comforts/trivial)
(require lathe-comforts/yknow)


(provide /own-contract-out
  glossesque-summary-sys?
  glossesque-summary-sys-trivial-value
  glossesque-summary-sys-summarize-zero
  glossesque-summary-sys-summarize-one-entry
  glossesque-summary-sys-summary-plus
  glossesque-summary-sys-summary-minus
  prop:glossesque-summary-sys
  make-glossesque-summary-sys-impl
  trivial-glossesque-summary-sys
  forwarding-glossesque-summary-sys
  counted?
  counted-count
  counted-original)
(provide
  counted)
(provide /own-contract-out
  counted-glossesque-summary-sys
  summarized?
  summarized-summary
  summarized-original)
(provide
  summarized)
(provide /own-contract-out
  glossesque-sys?
  glossesque-sys-impl?
  glossesque-sys-get-summary-sys
  glossesque-sys-glossesque-union-of-zero
  glossesque-sys-glossesque-skv-union-of-two-knowable
  glossesque-sys-glossesque-ref-entry-maybe-knowable
  glossesque-sys-rider-and-glossesque-update-maybe-knowable
  glossesque-sys-glossesque-summarize
  glossesque-sys-glossesque-iteration-sequence
  prop:glossesque-sys
  glossesque-sys-glossesque-set-maybe-knowable
  glossesque-sys-glossesque-empty?
  ; TODO: When we document this, make sure we document its keyword
  ; arguments.
  make-glossesque-sys-impl
  glossesque-summary-sys-map-summary
  glossesque-summary-sys-map-entry
  glossesque-summary-sys-map-key
  glossesque-sys-map-summary
  glossesque-sys-map-glossesque
  get-rep-gss-for-mapped-key-glossesque-sys
  glossesque-sys-map-key
  maybe-nonempty-glossesque-sys
  tagged-glossesque-sys?
  tagged-glossesque-sys-impl?
  tagged-glossesque-sys-inhabitant?-knowable
  tagged-glossesque-sys-get-get-glossesque-sys
  tagged-glossesque-sys-inhabitant-get-identifiable-object-guard-wrapper-maybe-knowable
  prop:tagged-glossesque-sys
  make-tagged-glossesque-sys-impl
  tagged-glossesque-sys-get-glossesque-sys)
(provide
  lift
  lift-struct
  make-tagged-glossesque-sys
  derive-tagged-glossesque-sys)
(provide /own-contract-out
  tagged-glossesque-sys=?
  tagged-glossesque-sys-get-glossesque-sys-for-ladder
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
  make-roughly-constant-custom-gloss-key-report
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
  known-identifiable-object-or-not?
  boolean-and-yknow-zip*
  gloss?
  list-map-foldl-knowable
  rider-and-hash-update-maybe-knowable
  hash-skv-map-maybe-knowable
  summarized-hash-skv-union-of-two-knowable
  entry-maybe-skv-union-of-two-knowable
  gloss-union-of-zero
  gloss-iteration-sequence
  summarized-gloss-skv-union-of-two-knowable
  gloss-ref-entry-maybe-knowable
  gloss-ref-maybe-knowable
  gloss-count
  gloss-empty?
  rider-and-gloss-update-maybe-knowable
  make-summarized-gloss-glossesque-sys
  gloss-set-maybe-knowable
  uninformative-dynamic-type
  expressly-has-dynamic-type-impl?
  prop:expressly-has-dynamic-type
  make-expressly-has-dynamic-type-impl
  default-get-dynamic-type
  current-get-dynamic-type
  get-dynamic-type
  smoosh-report?
  smoosh-report-impl?
  smoosh-report-join-yknow-maybe-yknow
  smoosh-report-meet-yknow-maybe-yknow
  smoosh-report-==-yknow-maybe-yknow
  smoosh-report-path-related-yknow-maybe-yknow
  prop:smoosh-report
  make-smoosh-report-impl
  uninformative-smoosh-report
  uninformative-smoosh-reports
  smoosh-and-comparison-of-two-report?
  smoosh-and-comparison-of-two-report-impl?
  smoosh-and-comparison-of-two-report-<=?-yknow
  smoosh-and-comparison-of-two-report->=?-yknow
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
  dynamic-type-get-smoosh-and-comparison-of-two-reports-via-first
  dynamic-type-get-smoosh-and-comparison-of-two-reports-via-second
  dynamic-type-get-smoosh-and-comparison-of-two-reports
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
  expressly-potentially-an-s-expression-landmark-dynamic-type-impl?
  prop:expressly-potentially-an-s-expression-landmark-dynamic-type
  make-expressly-potentially-an-s-expression-landmark-dynamic-type-impl
  dynamic-type-value-s-expression-landmark?-knowable
  known-s-expression-landmark-or-not?
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
  constant-smoosh-and-comparison-of-two-report
  constant-smoosh-and-comparison-of-two-reports
  constant-smoosh-equal-hash-code-support-report
  constant-smoosh-equal-hash-code-support-reports
  eq-wrapper?
  eq-wrapper-value)
(provide
  eq-wrapper)
(provide /own-contract-out
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
  smoosh-and-comparison-of-two-report-joininfo
  smoosh-and-comparison-of-two-reports-joininfo
  smoosh-reports-with-hesitation-at-discrepancies
  smoosh-and-comparison-of-two-reports-with-hesitation-at-discrepancies
  list-rev-append
  list-rem-first-maybe-knowable
  summarized-assoc-list/c
  summarized-assoc-list-nil
  summarized-assoc-list-cons
  summarized-assoc-list-skv-union-of-two-knowable
  summarized-assoc-list-ref-entry-maybe-knowable
  rider-and-summarized-assoc-list-update-maybe-knowable
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
  identifiable-object-tagged-glossesque-sys
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
  dynamic-type-case-by-cases
  gloss-ref-entry
  gloss-ref
  gloss-set
  make-gloss
  gloss-keys
  get-dynamic-type-with-any-dynamic-type
  default-any-dynamic-type?)
(provide
  default-any-dynamic-type)
(provide /own-contract-out
  current-any-dynamic-type
  any-dynamic-type)


(define-imitation-simple-generics
  glossesque-summary-sys? glossesque-summary-sys-impl?
  (#:method glossesque-summary-sys-trivial-value (#:this) ())
  (#:method glossesque-summary-sys-summarize-zero (#:this))
  (#:method glossesque-summary-sys-summarize-one-entry (#:this) () ())
  (#:method glossesque-summary-sys-summary-plus (#:this) () ())
  (#:method glossesque-summary-sys-summary-minus (#:this) () ())
  prop:glossesque-summary-sys
  make-glossesque-summary-sys-impl-from-various-unkeyworded
  'glossesque-summary-sys 'glossesque-summary-sys-impl (list))
(ascribe-own-contract glossesque-summary-sys? (-> any/c boolean?))
(ascribe-own-contract glossesque-summary-sys-impl?
  (-> any/c boolean?))
(ascribe-own-contract glossesque-summary-sys-trivial-value
  (-> glossesque-summary-sys? any/c any/c))
(ascribe-own-contract glossesque-summary-sys-summarize-zero
  (-> glossesque-summary-sys? any/c))
(ascribe-own-contract glossesque-summary-sys-summarize-one-entry
  (-> glossesque-summary-sys? any/c any/c any/c))
(ascribe-own-contract glossesque-summary-sys-summary-plus
  (-> glossesque-summary-sys? any/c any/c any/c))
(ascribe-own-contract glossesque-summary-sys-summary-minus
  (-> glossesque-summary-sys? any/c any/c any/c))
(ascribe-own-contract prop:glossesque-summary-sys
  (struct-type-property/c glossesque-summary-sys-impl?))

(define/own-contract
  (make-glossesque-summary-sys-impl
    #:trivial-value trivial-value
    #:summarize-zero summarize-zero
    #:summarize-one-entry summarize-one-entry
    #:summary-plus summary-plus
    #:summary-minus summary-minus)
  (->
    #:trivial-value (-> glossesque-summary-sys? any/c any/c)
    #:summarize-zero (-> glossesque-summary-sys? any/c)
    
    #:summarize-one-entry
    (-> glossesque-summary-sys? any/c any/c any/c)
    
    #:summary-plus (-> glossesque-summary-sys? any/c any/c any/c)
    #:summary-minus (-> glossesque-summary-sys? any/c any/c any/c)
    glossesque-summary-sys-impl?)
  (make-glossesque-summary-sys-impl-from-various-unkeyworded
    trivial-value
    summarize-zero
    summarize-one-entry
    summary-plus
    summary-minus))

(define-imitation-simple-struct
  (trivial-glossesque-summary-sys?)
  trivial-glossesque-summary-sys-unguarded
  'trivial-glossesque-summary-sys (current-inspector) (auto-write)
  
  (#:prop prop:glossesque-summary-sys
    (make-glossesque-summary-sys-impl
      
      #:trivial-value (fn gss k /trivial)
      
      #:summarize-zero
      (fn gss
        (trivial))
      
      #:summarize-one-entry
      (fn gss k v
        (trivial))
      
      #:summary-plus
      (fn gss a b
        ; TODO: Remove these lines when we're sure nothing invokes
        ; this on a non-`trivial?` value by accident. Seems like we
        ; should use a contract to enforce this kind of thing.
        (dissect a (trivial)
        /dissect b (trivial)
        /trivial))
      
      #:summary-minus
      (fn gs a b
        ; TODO: Remove these lines when we're sure nothing invokes
        ; this on a non-`trivial?` value by accident. Seems like we
        ; should use a contract to enforce this kind of thing.
        (dissect a (trivial)
        /dissect b (trivial)
        /trivial))
      
      ))
  
  )

(define/own-contract (trivial-glossesque-summary-sys)
  (-> glossesque-summary-sys?)
  (trivial-glossesque-summary-sys-unguarded))

(define-imitation-simple-struct
  (forwarding-glossesque-summary-sys?
    forwarding-glossesque-summary-sys-summarize-one-entry
    forwarding-glossesque-summary-sys-original)
  forwarding-glossesque-summary-sys-unguarded
  'forwarding-glossesque-summary-sys (current-inspector) (auto-write)
  
  (#:prop prop:glossesque-summary-sys
    (make-glossesque-summary-sys-impl
      
      #:trivial-value
      (fn gss k
        (dissect gss
          (forwarding-glossesque-summary-sys-unguarded
            summarize-one-entry original)
        ; TODO: See if we need to do more than this.
        /glossesque-summary-sys-trivial-value original k))
      
      #:summarize-zero
      (dissectfn
        (forwarding-glossesque-summary-sys-unguarded
          summarize-one-entry original)
        (glossesque-summary-sys-summarize-zero original))
      
      #:summarize-one-entry
      (fn gss k v
        (dissect gss
          (forwarding-glossesque-summary-sys-unguarded
            summarize-one-entry original)
        /summarize-one-entry k v))
      
      #:summary-plus
      (fn gss a b
        (dissect gss
          (forwarding-glossesque-summary-sys-unguarded
            summarize-one-entry original)
        /glossesque-summary-sys-summary-plus original a b))
      
      #:summary-minus
      (fn gss a b
        (dissect gss
          (forwarding-glossesque-summary-sys-unguarded
            summarize-one-entry original)
        /glossesque-summary-sys-summary-minus original a b))
      
      ))
  
  )

(define/own-contract
  (forwarding-glossesque-summary-sys gss
    #:summarize-one-entry summarize-one-entry)
  (->
    glossesque-summary-sys?
    #:summarize-one-entry (-> any/c any/c any/c)
    glossesque-summary-sys?)
  (forwarding-glossesque-summary-sys-unguarded
    summarize-one-entry gss))

(define-imitation-simple-struct
  (counted? counted-count counted-original)
  counted 'counted (current-inspector) (auto-write) (auto-equal))
(ascribe-own-contract counted? (-> any/c boolean?))
(ascribe-own-contract counted-count (-> counted? any/c))
(ascribe-own-contract counted-original (-> counted? any/c))

(define-imitation-simple-struct
  (counted-glossesque-summary-sys?
    counted-glossesque-summary-sys-original)
  counted-glossesque-summary-sys-unguarded
  'counted-glossesque-summary-sys (current-inspector) (auto-write)
  
  (#:prop prop:glossesque-summary-sys
    (make-glossesque-summary-sys-impl
      
      #:trivial-value
      (fn gss k
        (dissect gss
          (counted-glossesque-summary-sys-unguarded original)
        /glossesque-summary-sys-trivial-value original k))
      
      #:summarize-zero
      (dissectfn (counted-glossesque-summary-sys-unguarded original)
        (counted 0 /glossesque-summary-sys-summarize-zero original))
      
      #:summarize-one-entry
      (fn gss k v
        (dissect gss
          (counted-glossesque-summary-sys-unguarded original)
        /counted 1
          (glossesque-summary-sys-summarize-one-entry original k v)))
      
      #:summary-plus
      (fn gss a b
        (dissect gss
          (counted-glossesque-summary-sys-unguarded gss-original)
        /dissect a (counted a-count a-original)
        /dissect b (counted b-count b-original)
        /counted (+ a-count b-count)
          (glossesque-summary-sys-summary-plus
            gss-original a-original b-original)))
      
      #:summary-minus
      (fn gss a b
        (dissect gss
          (counted-glossesque-summary-sys-unguarded gss-original)
        /dissect a (counted a-count a-original)
        /dissect b (counted b-count b-original)
        /counted (- a-count b-count)
          (glossesque-summary-sys-summary-minus
            gss-original a-original b-original)))
      
      )))

(define/own-contract (counted-glossesque-summary-sys gss)
  (-> glossesque-summary-sys? glossesque-summary-sys?)
  (counted-glossesque-summary-sys-unguarded gss))

(define-imitation-simple-struct
  (summarized? summarized-summary summarized-original)
  summarized
  'summarized (current-inspector) (auto-write) (auto-equal))
(ascribe-own-contract summarized? (-> any/c boolean?))
(ascribe-own-contract summarized-summary (-> summarized? any/c))
(ascribe-own-contract summarized-original (-> summarized? any/c))

(define-imitation-simple-generics
  glossesque-sys? glossesque-sys-impl?
  (#:method glossesque-sys-get-summary-sys (#:this))
  (#:method glossesque-sys-glossesque-union-of-zero (#:this))
  (#:method glossesque-sys-glossesque-skv-union-of-two-knowable
    (#:this)
    ()
    ()
    ()
    ()
    ()
    ()
    ())
  (#:method
    glossesque-sys-glossesque-ref-entry-maybe-knowable (#:this) () ())
  (#:method glossesque-sys-rider-and-glossesque-update-maybe-knowable
    (#:this)
    ()
    ()
    ())
  (#:method glossesque-sys-glossesque-summarize (#:this) ())
  (#:method glossesque-sys-glossesque-iteration-sequence (#:this) ())
  prop:glossesque-sys
  make-glossesque-sys-impl-from-various-unkeyworded
  'glossesque-sys 'glossesque-sys-impl (list))
(ascribe-own-contract glossesque-sys? (-> any/c boolean?))
(ascribe-own-contract glossesque-sys-impl? (-> any/c boolean?))
(ascribe-own-contract glossesque-sys-get-summary-sys
  (-> glossesque-sys? glossesque-summary-sys?))
(ascribe-own-contract glossesque-sys-glossesque-union-of-zero
  (-> glossesque-sys? any/c))
(ascribe-own-contract
  glossesque-sys-glossesque-skv-union-of-two-knowable
  (-> glossesque-sys? any/c any/c any/c boolean? boolean?
    (-> any/c any/c any/c)
    (-> any/c any/c any/c any/c (knowable/c (list/c any/c maybe?)))
    (knowable/c (list/c any/c any/c))))
(ascribe-own-contract glossesque-sys-glossesque-ref-entry-maybe-knowable
  (-> glossesque-sys? any/c any/c
    (knowable/c (maybe/c (list/c any/c any/c)))))
(ascribe-own-contract
  glossesque-sys-rider-and-glossesque-update-maybe-knowable
  (-> glossesque-sys? (list/c any/c any/c) any/c
    (-> (list/c any/c maybe?) (knowable/c (list/c any/c maybe?)))
    (knowable/c (list/c any/c any/c))))
(ascribe-own-contract glossesque-sys-glossesque-summarize
  (-> glossesque-sys? any/c any/c))
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
      (dissectfn (list (trivial) old-m) (known /list (trivial) m)))
  /dissectfn (list (trivial) g)
    g))

(define/own-contract (glossesque-sys-glossesque-empty? gs g)
  (-> glossesque-sys? any/c boolean?)
  (for/and
    ([(k v) (glossesque-sys-glossesque-iteration-sequence gs g)])
    #f))

(define/own-contract
  (make-glossesque-sys-impl
    #:get-summary-sys get-summary-sys
    #:glossesque-union-of-zero glossesque-union-of-zero
    
    #:glossesque-skv-union-of-two-knowable
    glossesque-skv-union-of-two-knowable
    
    #:glossesque-ref-entry-maybe-knowable
    glossesque-ref-entry-maybe-knowable
    
    #:rider-and-glossesque-update-maybe-knowable
    rider-and-glossesque-update-maybe-knowable
    
    #:glossesque-summarize glossesque-summarize
    #:glossesque-iteration-sequence glossesque-iteration-sequence)
  (->
    #:get-summary-sys (-> glossesque-sys? glossesque-summary-sys?)
    #:glossesque-union-of-zero (-> glossesque-sys? any/c)
    
    #:glossesque-skv-union-of-two-knowable
    (-> glossesque-sys? any/c any/c any/c boolean? boolean?
      (-> any/c any/c any/c)
      (-> any/c any/c any/c any/c (knowable/c (list/c any/c maybe?)))
      (knowable/c (list/c any/c any/c)))
    
    #:glossesque-ref-entry-maybe-knowable
    (-> glossesque-sys? any/c any/c
      (knowable/c (maybe/c (list/c any/c any/c))))
    
    #:rider-and-glossesque-update-maybe-knowable
    (-> glossesque-sys? (list/c any/c any/c) any/c
      (-> (list/c any/c maybe?) (knowable/c (list/c any/c maybe?)))
      (knowable/c (list/c any/c any/c)))
    
    #:glossesque-summarize (-> glossesque-sys? any/c any/c)
    
    #:glossesque-iteration-sequence
    (-> glossesque-sys? any/c (sequence/c any/c any/c))
    
    glossesque-sys-impl?)
  (make-glossesque-sys-impl-from-various-unkeyworded
    get-summary-sys
    glossesque-union-of-zero
    glossesque-skv-union-of-two-knowable
    glossesque-ref-entry-maybe-knowable
    rider-and-glossesque-update-maybe-knowable
    glossesque-summarize
    glossesque-iteration-sequence))

(define-imitation-simple-struct
  (mapped-summary-glossesque-summary-sys?
    mapped-summary-glossesque-summary-sys-granted-summary
    mapped-summary-glossesque-summary-sys-on-summary
    mapped-summary-glossesque-summary-sys-original)
  mapped-summary-glossesque-summary-sys
  'mapped-summary-glossesque-summary-sys (current-inspector)
  (auto-write)
  
  (#:prop prop:glossesque-summary-sys
    (make-glossesque-summary-sys-impl
      
      #:trivial-value
      (fn gss k
        (dissect gss
          (mapped-summary-glossesque-summary-sys >s< <s> original)
        /glossesque-summary-sys-trivial-value original k))
      
      #:summarize-zero
      (dissectfn
        (mapped-summary-glossesque-summary-sys >s< <s> original)
        (<s> /glossesque-summary-sys-summarize-zero original))
      
      #:summarize-one-entry
      (fn gss k v
        (dissect gss
          (mapped-summary-glossesque-summary-sys >s< <s> original)
        /<s> /glossesque-summary-sys-summarize-one-entry original k v))
      
      #:summary-plus
      (fn gss a b
        (dissect gss
          (mapped-summary-glossesque-summary-sys >s< <s> original)
        /<s> /glossesque-summary-sys-summary-plus original
          (>s< a)
          (>s< b)))
      
      #:summary-minus
      (fn gss a b
        (dissect gss
          (mapped-summary-glossesque-summary-sys >s< <s> original)
        /<s> /glossesque-summary-sys-summary-minus original
          (>s< a)
          (>s< b)))
      
      )))

(define/own-contract
  (glossesque-summary-sys-map-summary gs
    #:granted-summary [granted-summary (fn s s)]
    #:on-summary [on-summary (fn s s)])
  (->*
    (glossesque-summary-sys?)
    (
      #:granted-summary (-> any/c any/c)
      #:on-summary (-> any/c any/c))
    glossesque-summary-sys?)
  (mapped-summary-glossesque-summary-sys
    granted-summary on-summary gs))

(define-imitation-simple-struct
  (mapped-entry-glossesque-summary-sys?
    mapped-entry-glossesque-summary-sys-name
    mapped-entry-glossesque-summary-sys-granted-entry
    mapped-entry-glossesque-summary-sys-on-value
    mapped-entry-glossesque-summary-sys-original)
  mapped-entry-glossesque-summary-sys
  'mapped-entry-glossesque-summary-sys (current-inspector)
  (auto-write)
  
  (#:prop prop:glossesque-summary-sys
    (make-glossesque-summary-sys-impl
      
      #:trivial-value
      (fn gss k
        (dissect gss
          (mapped-entry-glossesque-summary-sys _ >e< <v> original)
        /<v> k /glossesque-summary-sys-trivial-value original k))
      
      #:summarize-zero
      (dissectfn
        (mapped-entry-glossesque-summary-sys _ >e< <v> original)
        (glossesque-summary-sys-summarize-zero original))
      
      #:summarize-one-entry
      (fn gss k v
        (dissect gss
          (mapped-entry-glossesque-summary-sys _ >e< <v> original)
        /dissect (>e< /list k v) (list k v)
        /glossesque-summary-sys-summarize-one-entry original k v))
      
      #:summary-plus
      (fn gss a b
        (dissect gss
          (mapped-entry-glossesque-summary-sys _ >e< <v> original)
        /glossesque-summary-sys-summary-plus original a b))
      
      #:summary-minus
      (fn gss a b
        (dissect gss
          (mapped-entry-glossesque-summary-sys _ >e< <v> original)
        /glossesque-summary-sys-summary-minus original a b))
      
      )))

(define/own-contract
  (glossesque-summary-sys-map-entry gs
    #:name [name (unquoted-printing-string "")]
    #:granted-entry [granted-entry (fn e e)]
    #:on-value [on-value (fn k v v)])
  (->*
    (glossesque-summary-sys?)
    (
      #:name any/c
      #:granted-entry (-> (list/c any/c any/c) (list/c any/c any/c))
      #:on-value (-> any/c any/c any/c))
    glossesque-summary-sys?)
  (mapped-entry-glossesque-summary-sys name granted-entry on-value gs))

(define/own-contract
  (glossesque-summary-sys-map-key gs
    #:name [name (unquoted-printing-string "")]
    #:granted-key [granted-key (fn k k)])
  (->*
    (glossesque-summary-sys?)
    (#:name any/c #:granted-key (-> any/c any/c))
    glossesque-summary-sys?)
  (glossesque-summary-sys-map-entry gs
    #:name `(key ,name)
    #:granted-entry (dissectfn (list k v) /list (granted-key k) v)))

(define-imitation-simple-struct
  (mapped-summary-glossesque-sys?
    mapped-summary-glossesque-sys-granted-summary
    mapped-summary-glossesque-sys-on-summary
    mapped-summary-glossesque-sys-original)
  mapped-summary-glossesque-sys
  'mapped-summary-glossesque-sys (current-inspector) (auto-write)
  
  (#:prop prop:glossesque-sys /make-glossesque-sys-impl
    
    #:get-summary-sys
    (dissectfn (mapped-summary-glossesque-sys >s< <s> original)
      (glossesque-summary-sys-map-summary
        #:granted-summary >s<
        #:on-summary <s>
        (glossesque-sys-get-summary-sys original)))
    
    #:glossesque-union-of-zero
    (dissectfn (mapped-summary-glossesque-sys >s< <s> original)
      (glossesque-sys-glossesque-union-of-zero original))
    
    #:glossesque-skv-union-of-two-knowable
    (fn gs state a b a-keeping? b-keeping? on-keep skv-union-knowable
      (dissect gs (mapped-summary-glossesque-sys >s< <s> original)
      /glossesque-sys-glossesque-skv-union-of-two-knowable
        original state a b a-keeping? b-keeping?
        (fn state summary /on-keep state /<s> summary)
        skv-union-knowable))
    
    #:glossesque-ref-entry-maybe-knowable
    (fn gs g k
      (dissect gs (mapped-summary-glossesque-sys >s< <s> original)
      /glossesque-sys-glossesque-ref-entry-maybe-knowable
        original g k))
    
    #:rider-and-glossesque-update-maybe-knowable
    (fn gs rider-and-g k on-rider-and-m-knowable
      (dissect gs (mapped-summary-glossesque-sys >s< <s> original)
      /glossesque-sys-rider-and-glossesque-update-maybe-knowable
        original rider-and-g k on-rider-and-m-knowable))
    
    #:glossesque-summarize
    (fn gs g
      (dissect gs (mapped-summary-glossesque-sys >s< <s> original)
      /<s> /glossesque-sys-glossesque-summarize original g))
    
    #:glossesque-iteration-sequence
    (fn gs g
      (dissect gs (mapped-summary-glossesque-sys >s< <s> original)
      /glossesque-sys-glossesque-iteration-sequence original g))
    
    )
  
  )

(define/own-contract
  (glossesque-sys-map-summary gs
    #:granted-summary [granted-summary (fn s s)]
    #:on-summary [on-summary (fn s s)])
  (->*
    (glossesque-sys?)
    (#:granted-summary (-> any/c any/c) #:on-summary (-> any/c any/c))
    glossesque-sys?)
  (mapped-summary-glossesque-sys granted-summary on-summary gs))

(define-imitation-simple-struct
  (mapped-glossesque-glossesque-sys?
    mapped-glossesque-glossesque-sys-granted-glossesque
    mapped-glossesque-glossesque-sys-on-glossesque
    mapped-glossesque-glossesque-sys-original)
  mapped-glossesque-glossesque-sys
  'mapped-glossesque-glossesque-sys (current-inspector) (auto-write)
  
  (#:prop prop:glossesque-sys /make-glossesque-sys-impl
    
    #:get-summary-sys
    (dissectfn (mapped-glossesque-glossesque-sys >g< <g> original)
      (glossesque-sys-get-summary-sys original))
    
    #:glossesque-union-of-zero
    (dissectfn (mapped-glossesque-glossesque-sys >g< <g> original)
      (<g> /glossesque-sys-glossesque-union-of-zero original))
    
    #:glossesque-skv-union-of-two-knowable
    (fn gs state a b a-keeping? b-keeping? on-keep skv-union-knowable
      (dissect gs (mapped-glossesque-glossesque-sys >g< <g> original)
      /knowable-map
        (glossesque-sys-glossesque-skv-union-of-two-knowable
          original state (>g< a) (>g< b) a-keeping? b-keeping? on-keep
          skv-union-knowable)
      /fn result
        (<g> result)))
    
    #:glossesque-ref-entry-maybe-knowable
    (fn gs g k
      (dissect gs (mapped-glossesque-glossesque-sys >g< <g> original)
      /w- g (>g< g)
      /glossesque-sys-glossesque-ref-entry-maybe-knowable
        original g k))
    
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
    
    #:glossesque-summarize
    (fn gs g
      (dissect gs (mapped-glossesque-glossesque-sys >g< <g> original)
      /glossesque-sys-glossesque-summarize original />g< g))
    
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

(define/own-contract (get-rep-gss-for-mapped-key-glossesque-sys gss)
  (-> glossesque-summary-sys? glossesque-summary-sys?)
  (glossesque-summary-sys-map-entry gss
    #:name 'unwrap-double-key
    #:granted-entry (dissectfn (list internal-k /list k v) /list k v)
    #:on-value (fn k v /list k v)))

(define-imitation-simple-struct
  (mapped-key-glossesque-sys?
    mapped-key-glossesque-sys-name
    mapped-key-glossesque-sys-granted-key
    mapped-key-glossesque-sys-original-glossesque-summary-sys
    mapped-key-glossesque-sys-rep-glossesque-sys)
  mapped-key-glossesque-sys
  'mapped-key-glossesque-sys (current-inspector) (auto-write)
  
  (#:prop prop:glossesque-sys /make-glossesque-sys-impl
    
    #:get-summary-sys
    (dissectfn
      (mapped-key-glossesque-sys name >k< original-gss rep-gs)
      (glossesque-summary-sys-map-key original-gss
        #:name `(granted-key ,name)
        #:granted-key (fn k />k< k)))
    
    #:glossesque-union-of-zero
    (dissectfn (mapped-key-glossesque-sys _ >k< original-gss rep-gs)
      (glossesque-sys-glossesque-union-of-zero rep-gs))
    
    #:glossesque-skv-union-of-two-knowable
    (fn gs state a b a-keeping? b-keeping? on-keep skv-union-knowable
      (dissect gs
        (mapped-key-glossesque-sys _ >k< original-gss rep-gs)
      /glossesque-sys-glossesque-skv-union-of-two-knowable
        rep-gs state a b a-keeping? b-keeping? on-keep
      /fn state internal-k a-entry b-entry
        (dissect a-entry (list k a-v)
        /dissect b-entry (list _ b-v)
        /knowable-map (skv-union-knowable state k a-v b-v)
        /dissectfn (list state m)
          (list state /maybe-map m /fn v /list k v))))
    
    #:glossesque-ref-entry-maybe-knowable
    (fn gs g k
      (dissect gs
        (mapped-key-glossesque-sys _ >k< original-gss rep-gs)
      /w- internal-k (>k< k)
      /knowable-bind
        (glossesque-sys-glossesque-ref-entry-maybe-knowable
          rep-gs g internal-k)
      /fn entry-entry-m
      /known
        (maybe-map entry-entry-m
          (dissectfn (list internal-k /list k v)
            (list k v)))))
    
    #:rider-and-glossesque-update-maybe-knowable
    (fn gs rider-and-g k on-rider-and-m-knowable
      (dissect gs
        (mapped-key-glossesque-sys _ >k< original-gss rep-gs)
      /w- internal-k (>k< k)
      /glossesque-sys-rider-and-glossesque-update-maybe-knowable
        rep-gs rider-and-g internal-k
      /dissectfn (list rider entry-m)
        (w- k
          (expect entry-m (just entry) k
          /dissect entry (list k v)
            k)
        /w- v-m (maybe-map entry-m /dissectfn (list k v) v)
        /knowable-map (on-rider-and-m-knowable /list rider v-m)
        /dissectfn (list rider v-m)
          (list rider /maybe-map v-m /fn v /list k v))))
    
    #:glossesque-summarize
    (fn gs g
      (dissect gs
        (mapped-key-glossesque-sys _ >k< original-gss rep-gs)
      /glossesque-sys-glossesque-summarize rep-gs g))
    
    #:glossesque-iteration-sequence
    (fn gs g
      (dissect gs
        (mapped-key-glossesque-sys _ >k< original-gss rep-gs)
      /sequence-map
        (fn internal-k entry
          (dissect entry (list k v)
          /values k v))
        (glossesque-sys-glossesque-iteration-sequence rep-gs g)))
    
    )
  
  )

(define/own-contract
  (glossesque-sys-map-key original-gss get-original-gs
    #:name [name (unquoted-printing-string "")]
    #:granted-key granted-key)
  (->*
    (
      glossesque-summary-sys?
      (-> glossesque-summary-sys? glossesque-sys?)
      #:granted-key (-> any/c any/c))
    (#:name any/c)
    glossesque-sys?)
  (w- rep-gss (get-rep-gss-for-mapped-key-glossesque-sys original-gss)
  /w- rep-gs (get-original-gs rep-gss)
  /mapped-key-glossesque-sys name granted-key original-gss rep-gs))

(define/own-contract (maybe-nonempty-glossesque-sys original)
  (-> glossesque-sys? glossesque-sys?)
  (glossesque-sys-map-glossesque original
    #:granted-glossesque
    (fn g-m
      (mat g-m (just g) g
      ; TODO: Remove this line when we're sure nothing invokes this on
      ; a non-`maybe?` value by accident. Seems like we should use a
      ; contract to enforce this kind of thing.
      /dissect g-m (nothing)
      /glossesque-sys-glossesque-union-of-zero original))
    #:on-glossesque
    (fn g
      (maybe-if (not /glossesque-sys-glossesque-empty? original g) /fn
        g))))


(define-imitation-simple-generics
  tagged-glossesque-sys? tagged-glossesque-sys-impl?
  (#:method tagged-glossesque-sys-inhabitant?-knowable (#:this) ())
  (#:method tagged-glossesque-sys-get-get-glossesque-sys (#:this))
  (#:method
    tagged-glossesque-sys-inhabitant-get-identifiable-object-guard-wrapper-maybe-knowable
    (#:this)
    ())
  prop:tagged-glossesque-sys
  make-tagged-glossesque-sys-impl-from-various-unkeyworded
  'tagged-glossesque-sys 'tagged-glossesque-sys-impl (list))
(ascribe-own-contract tagged-glossesque-sys? (-> any/c boolean?))
(ascribe-own-contract tagged-glossesque-sys-impl? (-> any/c boolean?))
(ascribe-own-contract tagged-glossesque-sys-inhabitant?-knowable
  (-> tagged-glossesque-sys? any/c (knowable/c boolean?)))
(ascribe-own-contract tagged-glossesque-sys-get-get-glossesque-sys
  (-> tagged-glossesque-sys?
    (-> glossesque-summary-sys? glossesque-sys?)))
(ascribe-own-contract
  tagged-glossesque-sys-inhabitant-get-identifiable-object-guard-wrapper-maybe-knowable
  (-> tagged-glossesque-sys? any/c
    (knowable/c (maybe/c (-> any/c any/c)))))
(ascribe-own-contract prop:tagged-glossesque-sys
  (struct-type-property/c tagged-glossesque-sys-impl?))

(define/own-contract
  (make-tagged-glossesque-sys-impl
    #:inhabitant?-knowable inhabitant?-knowable
    #:get-get-glossesque-sys get-get-glossesque-sys
    
    #:inhabitant-get-identifiable-object-guard-wrapper-maybe-knowable
    inhabitant-get-identifiable-object-guard-wrapper-maybe-knowable
    
    )
  (->
    
    #:inhabitant?-knowable
    (-> tagged-glossesque-sys? any/c (knowable/c boolean?))
    
    #:get-get-glossesque-sys
    (-> tagged-glossesque-sys?
      (-> glossesque-summary-sys? glossesque-sys?))
    
    #:inhabitant-get-identifiable-object-guard-wrapper-maybe-knowable
    (-> tagged-glossesque-sys? any/c
      (knowable/c (maybe/c (-> any/c any/c))))
    
    glossesque-summary-sys-impl?)
  (make-tagged-glossesque-sys-impl-from-various-unkeyworded
    inhabitant?-knowable
    get-get-glossesque-sys
    inhabitant-get-identifiable-object-guard-wrapper-maybe-knowable))

(define/own-contract
  (tagged-glossesque-sys-get-glossesque-sys gss tgs)
  (-> glossesque-summary-sys? tagged-glossesque-sys? glossesque-sys?)
  ((tagged-glossesque-sys-get-get-glossesque-sys tgs) gss))

(define-syntax (lift stx)
  (syntax-parse stx / (_ result:expr)
  /syntax-local-lift-expression #'result))

(define-syntax (lift-struct stx)
  (syntax-parse stx / (_ field:expr)
    #'(w- lifted-struct
        (lift /let ()
          (define-imitation-simple-struct
            (lifted-struct? lifted-struct-field)
            lifted-struct 'lifted-struct (current-inspector)
            (auto-write)
            (auto-equal))
          lifted-struct)
      /lifted-struct field)))

(define-syntax (make-tagged-glossesque-sys stx)
  (syntax-parse stx
    [ (_ inhabitant?-knowable:expr get-gs:expr)
      #'(make-tagged-glossesque-sys #:equal-always-free-vars ()
          inhabitant?-knowable
          get-gs)]
    [
      (_ #:equal-always-free-vars (v:id ...)
        inhabitant?-knowable:expr
        get-gs:expr)
      #'(make-tagged-glossesque-sys #:equal-always-free-vars (v ...)
          inhabitant?-knowable
          get-gs
          
          #:inhabitant-get-identifiable-object-guard-wrapper-maybe-knowable
          (fn inhabitant
            (known /nothing)))]
    [
      (_ #:equal-always-free-vars (v:id ...)
        inhabitant?-knowable:expr
        get-gs:expr
        
        #:inhabitant-get-identifiable-object-guard-wrapper-maybe-knowable
        inhabitant-get-identifiable-object-guard-wrapper-maybe-knowable:expr)
      
      #:with (made-tagged-glossesque-sys-v ...)
      (generate-temporaries #'(v ...))
      
      #:with (a-v ...) (generate-temporaries #'(v ...))
      #:with (b-v ...) (generate-temporaries #'(v ...))
      #:with (tgs-v ...) (generate-temporaries #'(v ...))
      
      #'(w- made-tagged-glossesque-sys
          (lift /let ()
            (define-imitation-simple-struct
              (made-tagged-glossesque-sys?
                made-tagged-glossesque-sys-v ...)
              made-tagged-glossesque-sys
              'made-tagged-glossesque-sys (current-inspector)
              (auto-write)
              
              (#:gen gen:equal-mode+hash
                
                (define (equal-mode-proc a b recur now?)
                  (dissect a (made-tagged-glossesque-sys a-v ...)
                  /dissect b (made-tagged-glossesque-sys b-v ...)
                  /and (equal-always? a-v b-v) ...))
                
                (define (hash-mode-proc tgs recur now?)
                  (dissect tgs (made-tagged-glossesque-sys tgs-v ...)
                  /hash-code-combine
                    (equal-always-hash-code made-tagged-glossesque-sys?)
                    (equal-always-hash-code tgs-v)
                    ...))
                
                )
              
              (#:prop prop:tagged-glossesque-sys
                (make-tagged-glossesque-sys-impl
                  
                  #:inhabitant?-knowable
                  (fn tgs candidate
                    (dissect tgs (made-tagged-glossesque-sys v ...)
                    /inhabitant?-knowable candidate))
                  
                  #:get-get-glossesque-sys
                  (dissectfn (made-tagged-glossesque-sys v ...)
                    get-gs)
                  
                  #:inhabitant-get-identifiable-object-guard-wrapper-maybe-knowable
                  (fn tgs inhabitant
                    (
                      (dissect tgs (made-tagged-glossesque-sys v ...)
                        inhabitant-get-identifiable-object-guard-wrapper-maybe-knowable)
                      inhabitant))
                  
                  ))
              
              )
            made-tagged-glossesque-sys)
        /made-tagged-glossesque-sys v ...)]))

(define-syntax (derive-tagged-glossesque-sys stx)
  (syntax-parse stx
    [
      (_ orig-inhabitant?-knowable:id get-gs:id
        new-inhabitant?-knowable:expr
        new-get-gs:expr)
      #'(derive-tagged-glossesque-sys orig-inhabitant?-knowable get-gs
          new-inhabitant?-knowable
          new-get-gs
          
          #:inhabitant-get-identifiable-object-guard-wrapper-maybe-knowable
          inhabitant-get-identifiable-object-guard-wrapper-maybe-knowable
          inhabitant-get-identifiable-object-guard-wrapper-maybe-knowable)]
    [
      (_ orig-inhabitant?-knowable:id get-gs:id
        new-inhabitant?-knowable:expr
        new-get-gs:expr
        
        #:inhabitant-get-identifiable-object-guard-wrapper-maybe-knowable
        orig-inhabitant-get-identifiable-object-guard-wrapper-maybe-knowable:id
        new-inhabitant-get-identifiable-object-guard-wrapper-maybe-knowable:expr)
      #'(fn tgs-k
          (knowable-map tgs-k
            (fn tgs
              (make-tagged-glossesque-sys #:equal-always-free-vars (tgs)
                (w- orig-inhabitant?-knowable
                  (fn v
                    (tagged-glossesque-sys-inhabitant?-knowable tgs v))
                  new-inhabitant?-knowable)
                (w- get-gs (tagged-glossesque-sys-get-get-glossesque-sys tgs)
                  new-get-gs)
                
                #:inhabitant-get-identifiable-object-guard-wrapper-maybe-knowable
                (w-
                  orig-inhabitant-get-identifiable-object-guard-wrapper-maybe-knowable
                  (fn inhabitant
                    (tagged-glossesque-sys-inhabitant-get-identifiable-object-guard-wrapper-maybe-knowable
                      tgs inhabitant))
                  new-inhabitant-get-identifiable-object-guard-wrapper-maybe-knowable)))))]))

(define/own-contract (tagged-glossesque-sys=? a b)
  (-> tagged-glossesque-sys? tagged-glossesque-sys? boolean?)
  (equal-always? a b))

(define/own-contract
  (tagged-glossesque-sys-get-glossesque-sys-for-ladder gss tgs)
  (-> glossesque-summary-sys? tagged-glossesque-sys? glossesque-sys?)
  (tagged-glossesque-sys-get-glossesque-sys
    (counted-glossesque-summary-sys gss)
    tgs))


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
(ascribe-own-contract custom-gloss-key-report-impl?
  (-> any/c boolean?))
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
  (-> (endless-sequence/c custom-gloss-key-report?))
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
    ((endless-sequence/c custom-gloss-key-report?))
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
    (endless-sequence/c custom-gloss-key-report?))
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
    ((listof (endless-sequence/c custom-gloss-key-report?)))
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
    (endless-sequence/c custom-gloss-key-report?))
  (endless-sequence-zip*-map reports-list /fn report-list
    (custom-gloss-key-report-zip*-map report-list
      
      #:on-==-tagged-glossesque-sys-knowable
      on-==-tagged-glossesque-sys-knowable
      
      #:on-path-related-tagged-glossesque-sys-knowable
      on-path-related-tagged-glossesque-sys-knowable)))

(define/own-contract
  (constant-custom-gloss-key-report
    #:tagged-glossesque-sys-knowable [tagged-glossesque-sys-knowable #f]
    
    #:==-tagged-glossesque-sys-knowable
    [==-tagged-glossesque-sys-knowable
      (or tagged-glossesque-sys-knowable
        (raise-arguments-error 'constant-custom-gloss-key-report
          "expected either #:tagged-glossesque-sys-knowable or #:==-tagged-glossesque-sys-knowable to be provided"))]
    
    #:path-related-tagged-glossesque-sys-knowable
    [ path-related-tagged-glossesque-sys-knowable
      (or tagged-glossesque-sys-knowable
        (raise-arguments-error 'constant-custom-gloss-key-report
          "expected either #:tagged-glossesque-sys-knowable or #:path-related-tagged-glossesque-sys-knowable to be provided"))]
    
    )
  (->* ()
    (
      #:tagged-glossesque-sys-knowable
      (or/c #f (knowable/c tagged-glossesque-sys?))
      
      #:==-tagged-glossesque-sys-knowable
      (knowable/c tagged-glossesque-sys?)
      
      #:path-related-tagged-glossesque-sys-knowable
      (knowable/c tagged-glossesque-sys?)
      
      )
    
    custom-gloss-key-report?)
  (custom-gloss-key-report-zip*-map (list)
    
    #:on-==-tagged-glossesque-sys-knowable
    (dissectfn (list)
      ==-tagged-glossesque-sys-knowable)
    
    #:on-path-related-tagged-glossesque-sys-knowable
    (dissectfn (list)
      path-related-tagged-glossesque-sys-knowable)
    
    ))

(define/own-contract
  (constant-custom-gloss-key-reports
    #:tagged-glossesque-sys-knowable tagged-glossesque-sys-knowable)
  (->
    #:tagged-glossesque-sys-knowable
    (knowable/c tagged-glossesque-sys?)
    
    (endless-sequence/c custom-gloss-key-report?))
  (in-cycle /list /constant-custom-gloss-key-report
    #:tagged-glossesque-sys-knowable tagged-glossesque-sys-knowable))

(define/own-contract
  (make-roughly-constant-custom-gloss-key-report body)
  (->
    (->
      (-> custom-gloss-key-report?
        (knowable/c tagged-glossesque-sys?))
      (knowable/c tagged-glossesque-sys?))
    custom-gloss-key-report?)
  (constant-custom-gloss-key-report
    
    #:==-tagged-glossesque-sys-knowable
    (body /fn report
      (custom-gloss-key-report-get-==-tagged-glossesque-sys-knowable
        report))
    
    #:path-related-tagged-glossesque-sys-knowable
    (body /fn report
      (custom-gloss-key-report-get-path-related-tagged-glossesque-sys-knowable
        report))
    
    ))

(define-imitation-simple-struct
  (path-related-wrapper? path-related-wrapper-value)
  path-related-wrapper
  'path-related-wrapper (current-inspector) (auto-write)
  (#:gen gen:equal-mode+hash
    
    (define (equal-mode-proc a b recur now?)
      (knowable->falsable /knowable-map
        (yknow-value-knowable
          ; TODO FORWARD: These uses of
          ; `smoosh-report-==-yknow-maybe-yknow`,
          ; `smoosh-and-comparison-of-two-report-get-smoosh-report`,
          ; `dynamic-type-get-smoosh-and-comparison-of-two-reports`,
          ; and `any-dynamic-type` are forward references. See if we
          ; can untangle them.
          (smoosh-report-==-yknow-maybe-yknow
            (smoosh-and-comparison-of-two-report-get-smoosh-report
              (sequence-first
                (dynamic-type-get-smoosh-and-comparison-of-two-reports
                  (any-dynamic-type)
                  a
                  b)))))
        (fn ym
          (just? ym))))
    
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
        (yknow-value-knowable
          ; TODO FORWARD: These uses of
          ; `smoosh-report-==-yknow-maybe-yknow`,
          ; `smoosh-and-comparison-of-two-report-get-smoosh-report`,
          ; `dynamic-type-get-smoosh-and-comparison-of-two-reports`,
          ; and `any-dynamic-type` are forward references. See if we
          ; can untangle them.
          (smoosh-report-==-yknow-maybe-yknow
            (smoosh-and-comparison-of-two-report-get-smoosh-report
              (sequence-first
                (dynamic-type-get-smoosh-and-comparison-of-two-reports
                  (any-dynamic-type)
                  a
                  b)))))
        (fn ym
          (just? ym))))
    
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
  (#:method
    expressly-custom-gloss-key-dynamic-type-get-custom-gloss-key-reports
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
    (-> any/c any/c (endless-sequence/c custom-gloss-key-report?))
    
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
  (-> any/c any/c (endless-sequence/c custom-gloss-key-report?))
  (if (expressly-custom-gloss-key-dynamic-type? dt)
    (expressly-custom-gloss-key-dynamic-type-get-custom-gloss-key-reports
      dt inhabitant)
  /uninformative-custom-gloss-key-reports))


(define/own-contract (known-identifiable-object-or-not? v)
  (-> any/c boolean?)
  (known?
    (knowable-bind
      (custom-gloss-key-report-get-==-tagged-glossesque-sys-knowable
        (sequence-first
          (dynamic-type-get-custom-gloss-key-reports
            ; TODO FORWARD: This use of `any-dynamic-type` is a
            ; forward reference. See if we can untangle it.
            (any-dynamic-type)
            v)))
    /fn tgs
    /tagged-glossesque-sys-inhabitant-get-identifiable-object-guard-wrapper-maybe-knowable
      tgs v)))


(define-imitation-simple-struct (ladder-empty?) ladder-empty
  'ladder-empty (current-inspector) (auto-write))
(define-imitation-simple-struct
  (ladder-populated?
    
    ; A combined summary of the `ladder-populated-then` glossesque and
    ; the `ladder-populated-else` ladder.
    ladder-populated-summary
    
    ; A `tagged-glossesque-sys?`.
    ladder-populated-tagged-glossesque-sys
    
    ; A glossesque of the `(counted-glossesque-summary-sys gss)`
    ; variant of that system (where `gss` is the
    ; `glossesque-summary-sys?` used when manipulating this ladder),
    ; containing each entry that has a known true result for its
    ; `inhabitant?-knowable` predicate.
    ladder-populated-then
    
    ; A ladder of entries that have a known false result.
    ladder-populated-else
    
    )
  ladder-populated
  'ladder-populated (current-inspector) (auto-write))

(define (ladder-summarize gss g)
  (expect g (ladder-populated summary _ _ _)
    (glossesque-summary-sys-summarize-zero gss)
    summary))

(define (build-ladder gss tgs then else)
  (w- gs (tagged-glossesque-sys-get-glossesque-sys-for-ladder gss tgs)
  /dissect (glossesque-sys-glossesque-summarize gs then)
    (counted then-count then-summary)
  /mat then-count 0
    (raise-arguments-error 'build-ladder
      "internal error: somehow then-count was 0"
      "then" then
      "counted-then-summary" (counted then-count then-summary))
  /w- else-summary (ladder-summarize gss else)
  /w- summary
    (glossesque-summary-sys-summary-plus
      gss then-summary else-summary)
  /ladder-populated summary tgs then else))

(define (ladder-key-get-tagged-glossesque-sys-knowable k)
  ; TODO FORWARD: This use of `any-dynamic-type` is a forward
  ; reference. See if we can untangle it.
  (custom-gloss-key-report-get-==-tagged-glossesque-sys-knowable
    (sequence-first
      (dynamic-type-get-custom-gloss-key-reports
        (any-dynamic-type)
        k))))

(define
  (rider-and-ladder-update-maybe-knowable
    gss rider-and-g k on-rider-and-m-knowable)
  (w- gs (ladder-glossesque-sys gss)
  /dissect rider-and-g (list rider g)
  /w-loop next g g
    (expect g (ladder-populated _ tgs then else)
      (knowable-bind (on-rider-and-m-knowable /list rider /nothing)
      /dissectfn (list rider m)
      /expect m (just v) (known /list rider /ladder-empty)
      /knowable-bind (ladder-key-get-tagged-glossesque-sys-knowable k)
      /fn tgs
      /w- gs
        (tagged-glossesque-sys-get-glossesque-sys-for-ladder gss tgs)
      /w- then (glossesque-sys-glossesque-union-of-zero gs)
      /knowable-bind
        (glossesque-sys-glossesque-set-maybe-knowable
          gs then k (just v))
      /fn then
      /known /list rider /build-ladder gss tgs then /ladder-empty)
    /w- gs
      (tagged-glossesque-sys-get-glossesque-sys-for-ladder gss tgs)
    /knowable-bind (tagged-glossesque-sys-inhabitant?-knowable tgs k)
    /fn k-inhabits?
    /if k-inhabits?
      (knowable-map
        (glossesque-sys-rider-and-glossesque-update-maybe-knowable
          gs (list rider then) k on-rider-and-m-knowable)
      /dissectfn (list rider then)
        (list rider
          (dissect (glossesque-sys-glossesque-summarize gs then)
            (counted then-count _)
          /mat then-count 0
            else
            (build-ladder gss tgs then else))))
      (knowable-map (next else) /dissectfn (list rider else)
        (list rider /build-ladder gss tgs then else)))))

(define
  (ladder-skv-union-of-two-knowable
    gss state a b a-keeping? b-keeping? on-keep skv-union-knowable)
  (dissect a (summarized a-summary /gloss a-rep)
  /dissect b (summarized b-summary /gloss b-rep)
  /expect a (ladder-populated a-summary a-tgs a-then a-else)
    (if b-keeping?
      (known /list (on-keep state (ladder-summarize b)) b)
      (known /list state /ladder-empty))
  /expect b (ladder-populated b-summary b-tgs b-then b-else)
    (if a-keeping?
      (known /list (on-keep state a-summary) a)
      (known /list state /ladder-empty))
  /if (tagged-glossesque-sys=? a-tgs b-tgs)
    (w- tgs a-tgs
    /w- gs
      (tagged-glossesque-sys-get-glossesque-sys-for-ladder gss tgs)
    /knowable-bind
      (glossesque-sys-glossesque-skv-union-of-two-knowable
        gs state a-then b-then a-keeping? b-keeping?
        (fn state summary
          (dissect summary (counted _ summary)
          /on-keep state summary))
        (fn state k a-v b-v
          (skv-union-knowable state k a-v b-v)))
    /dissectfn (list state then)
    /knowable-bind
      (ladder-skv-union-of-two-knowable
        gss state a-else b-else a-keeping? b-keeping? on-keep
        skv-union-knowable)
    /dissectfn (list state else)
    /known /list state /build-ladder gss tgs then else)
  /w- a-gs
    (tagged-glossesque-sys-get-glossesque-sys-for-ladder gss a-tgs)
  /w- b-gs
    (tagged-glossesque-sys-get-glossesque-sys-for-ladder gss b-tgs)
  /dissect (glossesque-sys-glossesque-summarize a-gs a-then)
    (counted a-then-count _)
  /dissect (glossesque-sys-glossesque-summarize b-gs b-then)
    (counted b-then-count _)
  
  ; If the `then` glossesques don't match, we skip whichever one has
  ; the fewest entries, and add its entries one at a time after we've
  ; merged the rest. At call sites which primarily merge ladders keyed
  ; by a single type with its own well optimized merging algorithm,
  ; this approach gets out of that algorithm's way.
  ;
  ; NOTE: A more sophisticated approach would detect when the `then`
  ; of one ladder matches a glossesque deeper within another one,
  ; instead of just checking the `then` glossesques at the base. It
  ; would try to optimize a global property of the algorithm: Any
  ; glossesque we skip over because we can't merge it recursively
  ; means we have to perform all its elements' insertions individually
  ; (in order to check whether they need to belong to the other
  ; ladder's `then`; we can't just add them deeper within because that
  ; would break an invariant). Furthermore, when the tails have
  ; nothing in common, we have to skip everything from one or the
  ; other, incurring the cost of all its individual entry insertions.
  ; We're trying to minimize the overall number of individual entry
  ; insertions performed. Planning out a path of actions of the form
  ; "skip the `then` on the left," "skip the `then` on the right,"
  ; "merge the `then`s," and
  ; "one side is empty now, so use the other side" becomes something
  ; like a dynamic programming problem.
  ;
  ; A ladder can't have more than one match for a glossesque of
  ; another ladder If it did, the entries in its second matching
  ; glossesque would have been inserted into the first one.
  ;
  ; So actually, once we take the first step of finding where all the
  ; matching glossesques are, each ladder is roughly like a sequence
  ; of indexes into the other, and we have something like the
  ; "longest increasing subsequence" problem.
  ;
  ; https://en.wikipedia.org/wiki/Longest_increasing_subsequence
  ;
  ; In our case, we might not be concerned with the absolute best
  ; algorithmic efficiency in terms of sequence lengths because we
  ; assume the number of smooshable user-defined types, and hence the
  ; number of glossesques they define, is a constant for any given
  ; program. Our only goal is to avoid having the merging cost be
  ; equivalent to inserting entries one at a time in a case where the
  ; ladder's entries are primarily instances of a type that has its
  ; own superior merging algorithm.
  ;
  ; Here's a way we can approach it for our situation:
  ;
  ; Choose just one of the two ladders that's been annotated with the
  ; locations of its glossesques' matches in the other ladder. For
  ; each of its glossesques that has a match, from tail to head, we'll
  ; further annotate it with one of three strategies: Skip everything
  ; else in the left-hand ladder, skip everything else in the
  ; right-hand ladder, or skip just enough on the left and right so as
  ; to get to some particular (and designated) other matching pair of
  ; glossesques to be merged next. This strategy has an accumulated
  ; cost associated with proceeding from it, which we record as part
  ; of this annotation. The way we choose which strategy to annotate
  ; each pair of matching glossesques with is simply by attempting all
  ; of the possibilities.
  ;
  ; Once we've annotated all the pairs of matching glossesques this
  ; way, we can put one more annotation at the root (just past the
  ; heads of the ladders in the headward direction), chosen the same
  ; way. This represents our overall strategy for merging the ladders,
  ; and we proceed according to that strategy.
  ;
  /dissect
    (if (< a-then-count b-then-count)
      (list
        a b a-keeping? b-keeping? a-gs b-gs a-then b-then
        a-else b-else skv-union-knowable)
      (list
        b a b-keeping? a-keeping? b-gs a-gs b-then a-then
        b-else a-else
        (fn state k b-v a-v
          (skv-union-knowable k a-v b-v))))
    (list
      min max min-keeping? max-keeping? min-gs max-gs
      min-then max-then min-else max-else skv-union-knowable)
  /knowable-bind
    (ladder-skv-union-of-two-knowable
      gss state max min-else max-keeping? min-keeping? on-keep
      skv-union-knowable)
  /dissectfn (list state result)
  /w- min-thens
    (in-values-sequence
      (glossesque-sys-glossesque-iteration-sequence min-gs min-then))
  /w-loop next state state min-thens min-thens result result
    (expect min-thens (sequence* min-then-entry min-thens)
      (known /list state result)
    /dissect min-then-entry (list k min-then-v)
    /knowable-bind
      (rider-and-ladder-update-maybe-knowable
        (list state result)
        k
        (dissectfn (list state result-v-m)
          (expect result-v-m (just result-v)
            (if min-keeping?
              (known /list
                (on-keep state
                  (glossesque-summary-sys-summarize-one-entry
                    gss k min-then-v))
                (just min-then-v))
              (known /list state (nothing)))
          /skv-union-knowable state k min-then-v result-v)))
    /dissectfn (list state result)
    /next state min-thens result)))

(define-imitation-simple-struct
  (ladder-glossesque-sys? ladder-glossesque-sys-get-summary-sys)
  ladder-glossesque-sys-unguarded
  'ladder-glossesque-sys (current-inspector) (auto-write)
  
  (#:prop prop:glossesque-sys /make-glossesque-sys-impl
    
    #:get-summary-sys
    (dissectfn (ladder-glossesque-sys-unguarded gss)
      gss)
    
    #:glossesque-union-of-zero
    (fn gs
      (ladder-empty))
    
    #:glossesque-skv-union-of-two-knowable
    (fn gs state a b a-keeping? b-keeping? on-keep skv-union-knowable
      (dissect gs (ladder-glossesque-sys-unguarded gss)
      /ladder-skv-union-of-two-knowable
        gss state a b a-keeping? b-keeping? on-keep
        skv-union-knowable))
    
    #:glossesque-ref-entry-maybe-knowable
    (fn gs g k
      (dissect gs (ladder-glossesque-sys-unguarded gss)
      /w-loop next g g
        (expect g (ladder-populated _ tgs then else) (known /nothing)
        /w- gs
          (tagged-glossesque-sys-get-glossesque-sys-for-ladder
            gss tgs)
        /knowable-bind
          (tagged-glossesque-sys-inhabitant?-knowable tgs k)
        /fn k-inhabits?
        /if k-inhabits?
          (glossesque-sys-glossesque-ref-entry-maybe-knowable
            gs then k)
          (next else))))
    
    #:rider-and-glossesque-update-maybe-knowable
    (fn gs rider-and-g k on-rider-and-m-knowable
      (dissect gs (ladder-glossesque-sys-unguarded gss)
      /rider-and-ladder-update-maybe-knowable
        gss rider-and-g k on-rider-and-m-knowable))
    
    #:glossesque-summarize
    (fn gs g
      (dissect gs (ladder-glossesque-sys-unguarded gss)
      /ladder-summarize gss g))
    
    #:glossesque-iteration-sequence
    (fn gs g
      (dissect gs (ladder-glossesque-sys-unguarded gss)
      /w-loop next g g
        (expect g (ladder-populated _ tgs then else) (list)
        /w- gs
          (tagged-glossesque-sys-get-glossesque-sys-for-ladder
            gss tgs)
        /in-sequences
          (glossesque-sys-glossesque-iteration-sequence gs then)
          (next else))))
    
    )
  
  )

(define/own-contract (ladder-glossesque-sys gss)
  (-> glossesque-summary-sys? glossesque-sys?)
  (ladder-glossesque-sys-unguarded gss))


(define/own-contract (boolean-and-yknow-zip* y-list)
  (-> (listof (yknow/c boolean?)) (yknow/c boolean?))
  (make-yknow-from-value-knowable-promise
    (boolean-and-knowable-promise-zip* /list-map y-list /fn y
      (delay /yknow-value-knowable y))))

(define/own-contract
  (boolean-and-yknow-zip*/from-maybe-min-yknow-trivial-zip*-map
    maybe-min-yknow-trivial-zip*-map y-list)
  (->
    (-> (listof (yknow/c maybe?)) (-> (listof trivial?) trivial?)
      (yknow/c maybe?))
    (listof (yknow/c boolean?))
    (yknow/c boolean?))
  (yknow-map
    (maybe-min-yknow-trivial-zip*-map
      (list-map y-list /fn y
        (yknow-map y /fn b /maybe-if b /fn /trivial))
      (fn value-list /trivial))
    (fn m /just? m)))

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
        ; TODO FORWARD: This use of `gloss-ref-maybe-knowable` is a
        ; forward reference. See if we can untangle it.
        (knowable-map (gloss-ref-maybe-knowable b k) /fn b-v-m
          (maybe-map b-v-m /fn b-v
            (delay /value-equal-always?-knowable a-v b-v)))))
  /fn v=?-kpm-list
  /expect (maybe-min-zip* v=?-kpm-list) (just v=?-kp-list) (known #f)
  /force /boolean-and-knowable-promise-zip* v=?-kp-list))

(define-imitation-simple-struct
  (gloss? gloss-ladder)
  gloss 'gloss (current-inspector)
  
  (#:gen gen:custom-write
    (define write-proc
      (make-constructor-style-printer
        (fn self 'make-gloss)
        (fn self
          (list /for/list
            ; TODO FORWARD: This use of `gloss-iteration-sequence` is
            ; a forward reference. See if we can untangle it.
            ([(k v) (in-sequences /gloss-iteration-sequence self)])
            (cons k v))))))
  
  (#:gen gen:equal-mode+hash
    
    (define (equal-mode-proc a b recur now?)
      (knowable->falsable /gloss-equal-always?-knowable a b /fn a b
        (falsable->uninformative-knowable /recur a b)))
    
    (define (hash-mode-proc v recur now?)
      ; TODO FORWARD: This use of `any-dynamic-type` is a forward
      ; reference. See if we can untangle it.
      (define any-dt (any-dynamic-type))
      (define (hash-code-smooshable v)
        (force
          (smoosh-equal-hash-code-support-report-==-hash-code-promise
            (sequence-first
              (dynamic-type-get-smoosh-equal-hash-code-support-reports
                any-dt v))
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
  (summarized-hash-skv-union-of-two-knowable
    gss state a b a-keeping? b-keeping? on-keep skv-union-knowable)
  (->
    glossesque-summary-sys?
    any/c
    (match/c summarized any/c (and/c hash? immutable?))
    (match/c summarized any/c (and/c hash? immutable?))
    boolean?
    boolean?
    (-> any/c any/c any/c)
    (-> any/c any/c any/c any/c (knowable/c (list/c any/c maybe?)))
    (knowable/c (list/c any/c (and/c hash? immutable?))))
  (dissect a (summarized _ a)
  /dissect b (summarized _ b)
  /knowable-map
    (hash-skv-map-maybe-knowable
      (summarized (glossesque-summary-sys-summarize-zero gss) state)
      (hash-union a b #:combine /fn a b a)
      (fn summary-and-state k v
        (dissect summary-and-state (summarized summary state)
        /w- a-v-m (hash-ref-maybe a k)
        /w- b-v-m (hash-ref-maybe b k)
        /w- add-entry
          (fn k v
            (w- entry-summary
              (glossesque-summary-sys-summarize-one-entry gss k v)
            /known /list
              (summarized
                (glossesque-summary-sys-summary-plus
                  gss summary entry-summary)
                (on-keep state entry-summary))
              (just v)))
        /expect a-v-m (just a-v)
          (expect (list b-keeping? b-v-m) (list #t (just b-v))
            (known /list summary-and-state (nothing))
            (add-entry k b-v))
        /expect b-v-m (just b-v)
          (expect a-keeping? #t
            (known /list summary-and-state (nothing))
            (add-entry k a-v))
        /knowable-map (skv-union-knowable state k a-v b-v)
        /dissectfn (list state m)
          (list (summarized summary state) m))))
  /dissectfn (list (summarized summary state) h)
    (list state /summarized summary h)))

; Given two `maybe?` values and a stateful way of taking their union,
; this is a stateful way of taking their union that skips actually
; doing the work if both of the `maybe?` values is a `mothing?`. It
; just returns a `nothing?` in that case.
;
(define/own-contract
  (entry-maybe-skv-union-of-two-knowable
    gss state a b a-keeping? b-keeping? on-keep skv-union-knowable)
  (->
    glossesque-summary-sys?
    any/c
    (maybe/c (list/c any/c any/c))
    (maybe/c (list/c any/c any/c))
    boolean?
    boolean?
    (-> any/c any/c any/c)
    (-> any/c any/c any/c any/c (knowable/c (list/c any/c maybe?)))
    (knowable/c (list/c any/c (maybe/c (list/c any/c any/c)))))
  (expect a (just a-kv)
    (expect (list b-keeping? b) (list #t (just b-kv))
      (known /list state /nothing)
    /dissect b-kv (list b-k b-v)
    /known /list
      (on-keep state
        (glossesque-summary-sys-summarize-one-entry gss b-k b-v))
      b)
  /dissect a-kv (list a-k a-v)
  /expect b (just b-kv)
    (expect a-keeping? #t
      (known /list state /nothing)
    /known /list
      (on-keep state
        (glossesque-summary-sys-summarize-one-entry gss a-k a-v))
      a)
  /dissect b-kv (list b-k b-v)
  /knowable-map (skv-union-knowable state a-k a-v b-v)
  /dissectfn (list state m)
    (list state /maybe-map m /fn v /list a-k v)))

(define/own-contract (gloss-union-of-zero)
  (-> gloss?)
  (gloss /ladder-empty))

(define (gloss-ladder-glossesque-sys)
  (glossesque-sys-map-glossesque
    #:granted-glossesque (dissectfn (gloss rep) rep)
    #:on-glossesque (fn rep /gloss rep)
    (ladder-glossesque-sys
      (counted-glossesque-summary-sys
        (trivial-glossesque-summary-sys)))))

(define/own-contract (gloss-iteration-sequence g)
  (-> gloss? (sequence/c any/c any/c))
  (w- gs (gloss-ladder-glossesque-sys)
  /glossesque-sys-glossesque-iteration-sequence gs g))

(define/own-contract (gloss-ref-entry-maybe-knowable g k)
  (-> gloss? any/c (knowable/c (maybe/c (list/c any/c any/c))))
  (w- gs (gloss-ladder-glossesque-sys)
  /glossesque-sys-glossesque-ref-entry-maybe-knowable gs g k))

(define/own-contract (gloss-ref-maybe-knowable g k)
  (-> gloss? any/c (knowable/c maybe?))
  (knowable-map (gloss-ref-entry-maybe-knowable g k) /fn entry-m
    (maybe-map entry-m /dissectfn (list k v)
      v)))

(define/own-contract (gloss-count g)
  (-> gloss? natural?)
  (w- gs (gloss-ladder-glossesque-sys)
  /dissect (glossesque-sys-glossesque-summarize gs g)
    (counted result /trivial)
    result))

(define/own-contract (gloss-empty? g)
  (-> gloss? boolean?)
  (dissect g (gloss rep)
  /mat rep (ladder-empty) #t
    #f))

(define/own-contract
  (rider-and-gloss-update-maybe-knowable
    rider-and-g k on-rider-and-m-knowable)
  (-> (list/c any/c gloss?) any/c
    (-> (list/c any/c maybe?) (knowable/c (list/c any/c maybe?)))
    (knowable/c (list/c any/c gloss?)))
  (w- gs (gloss-ladder-glossesque-sys)
  /glossesque-sys-rider-and-glossesque-update-maybe-knowable
    gs rider-and-g k on-rider-and-m-knowable))

(define/own-contract
  (summarized-gloss-skv-union-of-two-knowable
    gss state a b a-keeping? b-keeping? on-keep skv-union-knowable)
  (->
    glossesque-summary-sys?
    any/c
    (match/c summarized any/c gloss?)
    (match/c summarized any/c gloss?)
    boolean?
    boolean?
    (-> any/c any/c any/c)
    (-> any/c any/c any/c any/c (knowable/c (list/c any/c maybe?)))
    (knowable/c (list/c any/c gloss?)))
  (dissect a (summarized _ a)
  /dissect b (summarized _ b)
  /w- add-entry-knowable
    (fn state summary result on-keep k v then
      (knowable-bind
        (rider-and-gloss-update-maybe-knowable (list (trivial) result)
          k
          (dissectfn (list (trivial) /nothing)
            (known /list (trivial) /just v)))
      /dissectfn (list (trivial) result)
      /w- entry-summary
        (glossesque-summary-sys-summarize-one-entry gss k v)
      /w- state (on-keep state entry-summary)
      /w- summary
        (glossesque-summary-sys-summary-plus
          gss summary entry-summary)
      /then state summary result))
  /w-loop next
    state state
    summary (glossesque-summary-sys-summarize-zero gss)
    a (in-values-sequence /gloss-iteration-sequence a)
    b b
    result (gloss-union-of-zero)
    
    (expect a (sequence* entry a)
      (expect b-keeping? #t
        (known /list state /summarized summary result)
      /w-loop next
        state state
        summary summary
        b (in-values-sequence /gloss-iteration-sequence b)
        result result
        
        (expect b (sequence* entry b)
          (known /list state /summarized summary result)
        /dissect entry (list k b-v)
        /add-entry-knowable state summary result on-keep k b-v
        /fn state summary result
        /next state summary b result))
    /dissect entry (list k a-v)
    /knowable-bind
      (rider-and-gloss-update-maybe-knowable (list (trivial) b) k
        (dissectfn (list (trivial) b-v-m)
          (known /list b-v-m /nothing)))
    /dissectfn (list b-v-m b)
    /expect b-v-m (just b-v)
      (if a-keeping?
        (add-entry-knowable state summary result on-keep k a-v
        /fn state summary result
        /next state summary a b result)
        (next state summary a b result))
    /knowable-bind (skv-union-knowable state k a-v b-v)
    /dissectfn (list state m)
    /expect m (just v)
      (next state summary a b result)
    /w- on-keep-do-nothing (fn state summary state)
    /add-entry-knowable state summary result on-keep-do-nothing k v
    /fn state summary result
    /next state summary a b result)))

(define-imitation-simple-struct
  (summarized-gloss-glossesque-sys?
    summarized-gloss-glossesque-sys-get-summary-sys)
  summarized-gloss-glossesque-sys
  'summarized-gloss-glossesque-sys (current-inspector) (auto-write)
  
  (#:prop prop:glossesque-sys /make-glossesque-sys-impl
    
    #:get-summary-sys
    (dissectfn (summarized-gloss-glossesque-sys gss)
      gss)
    
    #:glossesque-union-of-zero
    (dissectfn (summarized-gloss-glossesque-sys gss)
      (summarized (glossesque-summary-sys-summarize-zero gss)
        (gloss-union-of-zero)))
    
    #:glossesque-skv-union-of-two-knowable
    (fn gs state a b a-keeping? b-keeping? on-keep skv-union-knowable
      (dissect gs (summarized-gloss-glossesque-sys gss)
      /summarized-gloss-skv-union-of-two-knowable
        gss state a b a-keeping? b-keeping? on-keep
        skv-union-knowable))
    
    #:glossesque-ref-entry-maybe-knowable
    (fn gs g k
      (dissect g (summarized summary gloss)
      /gloss-ref-entry-maybe-knowable gloss k))
    
    #:rider-and-glossesque-update-maybe-knowable
    (fn gs rider-and-g k on-rider-and-m-knowable
      (dissect gs (summarized-gloss-glossesque-sys gss)
      /dissect rider-and-g (list rider /summarized summary gloss)
      /knowable-map
        (rider-and-gloss-update-maybe-knowable
          (list (summarized summary rider) gloss)
          k
        /dissectfn (list (summarized summary rider) m)
          (w- summary
            (expect m (just v) summary
            /glossesque-summary-sys-summary-minus gss summary
              (glossesque-summary-sys-summarize-one-entry gss k v))
          /knowable-map (on-rider-and-m-knowable /list rider m)
          /dissectfn (list rider m)
            (w- summary
              (expect m (just v) summary
              /glossesque-summary-sys-summary-plus gss summary
                (glossesque-summary-sys-summarize-one-entry gss k v))
            /list (summarized summary rider) m)))
      /dissectfn (list (summarized summary rider) gloss)
        (list rider /summarized summary gloss)))
    
    #:glossesque-summarize
    (fn gs g
      (dissect g (summarized summary gloss)
        summary))
    
    #:glossesque-iteration-sequence
    (fn gs g
      (dissect g (summarized summary gloss)
      /gloss-iteration-sequence gloss))
    
    )
  
  )

(define/own-contract (make-summarized-gloss-glossesque-sys gss)
  (-> glossesque-summary-sys? glossesque-sys?)
  (summarized-gloss-glossesque-sys gss))

(define/own-contract (gloss-set-maybe-knowable g k m)
  (-> gloss? any/c maybe? (knowable/c gloss?))
  (w- gs
    (make-summarized-gloss-glossesque-sys
      (trivial-glossesque-summary-sys))
  /knowable-map
    (glossesque-sys-glossesque-set-maybe-knowable
      gs (summarized (trivial) g) k m)
  /dissectfn (summarized (trivial) g)
    g))


(define-imitation-simple-struct (uninformative-dynamic-type?)
  uninformative-dynamic-type-unguarded
  'uninformative-dynamic-type (current-inspector) (auto-write))

(define/own-contract (uninformative-dynamic-type)
  (-> any/c)
  (uninformative-dynamic-type-unguarded))

(define-imitation-simple-generics
  expressly-has-dynamic-type? expressly-has-dynamic-type-impl?
  (#:method expressly-has-dynamic-type-get-dynamic-type
    ()
    ()
    (#:this))
  prop:expressly-has-dynamic-type make-expressly-has-dynamic-type-impl
  'expressly-has-dynamic-type 'expressly-has-dynamic-type-impl (list))
(ascribe-own-contract expressly-has-dynamic-type-impl?
  (-> any/c boolean?))
(ascribe-own-contract prop:expressly-has-dynamic-type
  (struct-type-property/c expressly-has-dynamic-type-impl?))
(ascribe-own-contract make-expressly-has-dynamic-type-impl
  (-> (-> gloss? any/c any/c any/c) expressly-has-dynamic-type-impl?))

; Gets as specific a dynamic type for Racket values as Lathe Comforts
; knows about.
(define/own-contract (default-get-dynamic-type bindings any-dt v)
  (-> gloss? any/c any/c any/c)
  (if (expressly-has-dynamic-type? v)
    (expressly-has-dynamic-type-get-dynamic-type bindings any-dt v)
  ; TODO FORWARD: These uses of `known-to-lathe-comforts-data?` and
  ; `known-to-lathe-comforts-data-dynamic-type` are forward
  ; references. See if we can untangle them.
  /if (known-to-lathe-comforts-data? v)
    (known-to-lathe-comforts-data-dynamic-type any-dt)
    (uninformative-dynamic-type)))

; This parameter's value is a function of contract
; `(-> gloss? any/c any/c any/c)` (defaulting to
; `default-get-dynamic-type`) that takes a `gloss?` of miscellaneous
; context, an "any" dynamic type to use recursively, and a value and
; returns its dynamic type.
;
(define/own-contract current-get-dynamic-type
  (parameter/c (-> gloss? any/c any/c any/c))
  (make-parameter /fn bindings any-dt v
    (default-get-dynamic-type bindings any-dt v)))

(define/own-contract (get-dynamic-type bindings any-dt v)
  (-> gloss? any/c any/c any/c)
  ((current-get-dynamic-type) bindings any-dt v))


(define-imitation-simple-generics
  smoosh-report? smoosh-report-impl?
  
  ; This says the operands' join, i.e. what result they smoosh into
  ; such that each operand satisfies `operand <= result`.
  ;
  (#:method smoosh-report-join-yknow-maybe-yknow (#:this))
  
  ; This says the operands' join, i.e. what result they smoosh into
  ; such that each operand satisfies `operand >= result`.
  ;
  (#:method smoosh-report-meet-yknow-maybe-yknow (#:this))
  
  ; This says how the operands smoosh along ==, in the sense of a
  ; result such that each operand satisfies both `operand <= result`
  ; and `operand >= result`.
  ;
  (#:method smoosh-report-==-yknow-maybe-yknow (#:this))
  
  ; This says how they smoosh along path-relatedness, in the sense of
  ; a result such that each operand is path-related to the result. Two
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
  (#:method smoosh-report-path-related-yknow-maybe-yknow (#:this))
  
  prop:smoosh-report make-smoosh-report-impl-from-various-unkeyworded
  'smoosh-report 'smoosh-report-impl (list))
(define smoosh-report-component/c
  (-> smoosh-report? (yknow/c (maybe/c (yknow/c any/c)))))
(ascribe-own-contract smoosh-report? (-> any/c boolean?))
(ascribe-own-contract smoosh-report-impl? (-> any/c boolean?))
(ascribe-own-contract smoosh-report-join-yknow-maybe-yknow
  smoosh-report-component/c)
(ascribe-own-contract smoosh-report-meet-yknow-maybe-yknow
  smoosh-report-component/c)
(ascribe-own-contract smoosh-report-==-yknow-maybe-yknow
  smoosh-report-component/c)
(ascribe-own-contract smoosh-report-path-related-yknow-maybe-yknow
  smoosh-report-component/c)
(ascribe-own-contract prop:smoosh-report
  (struct-type-property/c smoosh-report-impl?))

(define/own-contract
  (make-smoosh-report-impl
    #:join-yknow-maybe-yknow join-yknow-maybe-yknow
    #:meet-yknow-maybe-yknow meet-yknow-maybe-yknow
    #:==-yknow-maybe-yknow ==-yknow-maybe-yknow
    #:path-related-yknow-maybe-yknow path-related-yknow-maybe-yknow)
  (->
    #:join-yknow-maybe-yknow smoosh-report-component/c
    #:meet-yknow-maybe-yknow smoosh-report-component/c
    #:==-yknow-maybe-yknow smoosh-report-component/c
    #:path-related-yknow-maybe-yknow smoosh-report-component/c
    smoosh-report-impl?)
  (make-smoosh-report-impl-from-various-unkeyworded
    join-yknow-maybe-yknow
    meet-yknow-maybe-yknow
    ==-yknow-maybe-yknow
    path-related-yknow-maybe-yknow))

(define-imitation-simple-struct
  (uninformative-smoosh-report?)
  uninformative-smoosh-report-unguarded
  'uninformative-smoosh-report (current-inspector) (auto-write)
  (#:prop prop:smoosh-report /make-smoosh-report-impl
    
    #:join-yknow-maybe-yknow
    (fn self
      (uninformative-yknow))
    
    #:meet-yknow-maybe-yknow
    (fn self
      (uninformative-yknow))
    
    #:==-yknow-maybe-yknow
    (fn self
      (uninformative-yknow))
    
    #:path-related-yknow-maybe-yknow
    (fn self
      (uninformative-yknow))
    
    ))

(define/own-contract (uninformative-smoosh-report)
  (-> smoosh-report?)
  (uninformative-smoosh-report-unguarded))

(define/own-contract (uninformative-smoosh-reports)
  (-> (endless-sequence/c smoosh-report?))
  (in-cycle /list /uninformative-smoosh-report))

(define-imitation-simple-generics
  smoosh-and-comparison-of-two-report?
  smoosh-and-comparison-of-two-report-impl?
  ; This says whether they're related as (lhs <= rhs).
  (#:method smoosh-and-comparison-of-two-report-<=?-yknow (#:this))
  ; This says whether they're related as (lhs >= rhs).
  (#:method smoosh-and-comparison-of-two-report->=?-yknow (#:this))
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
(ascribe-own-contract smoosh-and-comparison-of-two-report-<=?-yknow
  (-> smoosh-and-comparison-of-two-report? (yknow/c boolean?)))
(ascribe-own-contract smoosh-and-comparison-of-two-report->=?-yknow
  (-> smoosh-and-comparison-of-two-report? (yknow/c boolean?)))
(ascribe-own-contract
  smoosh-and-comparison-of-two-report-get-smoosh-report
  (-> smoosh-and-comparison-of-two-report? smoosh-report?))
(ascribe-own-contract prop:smoosh-and-comparison-of-two-report
  (struct-type-property/c smoosh-and-comparison-of-two-report-impl?))

(define/own-contract
  (make-smoosh-and-comparison-of-two-report-impl
    #:<=?-yknow <=?-yknow
    #:>=?-yknow >=?-yknow
    #:get-smoosh-report get-smoosh-report)
  (->
    #:<=?-yknow
    (-> smoosh-and-comparison-of-two-report? (yknow/c boolean?))
    
    #:>=?-yknow
    (-> smoosh-and-comparison-of-two-report? (yknow/c boolean?))
    
    #:get-smoosh-report
    (-> smoosh-and-comparison-of-two-report? smoosh-report?)
    
    smoosh-report-impl?)
  (make-smoosh-and-comparison-of-two-report-impl-from-various-unkeyworded
    <=?-yknow >=?-yknow get-smoosh-report))

(define-imitation-simple-struct
  (uninformative-smoosh-and-comparison-of-two-report?)
  uninformative-smoosh-and-comparison-of-two-report-unguarded
  'uninformative-smoosh-and-comparison-of-two-report
  (current-inspector)
  (auto-write)
  (#:prop prop:smoosh-and-comparison-of-two-report
    (make-smoosh-and-comparison-of-two-report-impl
      
      #:<=?-yknow
      (fn self
        (uninformative-yknow))
      
      #:>=?-yknow
      (fn self
        (uninformative-yknow))
      
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
  (-> (endless-sequence/c smoosh-and-comparison-of-two-report?))
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
    expressly-smooshable-dynamic-type-get-smoosh-and-comparison-of-two-reports-via-first
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
    [get-smoosh-and-comparison-of-two-reports #f]
    
    #:get-smoosh-and-comparison-of-two-reports-via-first
    [ get-smoosh-and-comparison-of-two-reports-via-first
      (or get-smoosh-and-comparison-of-two-reports
        (raise-arguments-error 'make-expressly-smooshable-dynamic-type-impl
          "expected either #:get-smoosh-and-comparison-of-two-reports or #:get-smoosh-and-comparison-of-two-reports-via-first to be provided"))]
    
    #:get-smoosh-and-comparison-of-two-reports-via-second
    [ get-smoosh-and-comparison-of-two-reports-via-second
      (or get-smoosh-and-comparison-of-two-reports
        (raise-arguments-error 'make-expressly-smooshable-dynamic-type-impl
          "expected either #:get-smoosh-and-comparison-of-two-reports or #:get-smoosh-and-comparison-of-two-reports-via-second to be provided"))]
    
    )
  (->*
    (
      #:get-smoosh-of-zero-reports
      (-> any/c (endless-sequence/c smoosh-report?))
      
      #:get-smoosh-of-one-reports
      (-> any/c any/c (endless-sequence/c smoosh-report?))
      
      #:get-smoosh-and-comparison-of-two-reports
      (or/c #f
        (-> any/c any/c any/c
          (endless-sequence/c smoosh-and-comparison-of-two-report?)))
      
      )
    (
      #:get-smoosh-and-comparison-of-two-reports-via-first
      (-> any/c any/c any/c
        (endless-sequence/c smoosh-and-comparison-of-two-report?))
      
      #:get-smoosh-and-comparison-of-two-reports-via-second
      (-> any/c any/c any/c
        (endless-sequence/c smoosh-and-comparison-of-two-report?))
      
      )
    expressly-smooshable-dynamic-type-impl?)
  (make-expressly-smooshable-dynamic-type-impl-from-various-unkeyworded
    get-smoosh-of-zero-reports
    get-smoosh-of-one-reports
    get-smoosh-and-comparison-of-two-reports-via-first
    get-smoosh-and-comparison-of-two-reports-via-second))

(define/own-contract (dynamic-type-get-smoosh-of-zero-reports dt)
  (-> any/c
    ; Each report in the infinite sequence gives the smoosh identity
    ; elements, first for the type's bespoke notion of ordering, then
    ; for the information ordering, then for the information ordering
    ; of the information ordering representatives, and so on.
    (endless-sequence/c smoosh-report?))
  (if (expressly-smooshable-dynamic-type? dt)
    (expressly-smooshable-dynamic-type-get-smoosh-of-zero-reports dt)
    (uninformative-smoosh-reports)))

(define/own-contract (dynamic-type-get-smoosh-of-one-reports a-dt a)
  (-> any/c any/c (endless-sequence/c smoosh-report?))
  (if (expressly-smooshable-dynamic-type? a-dt)
    (expressly-smooshable-dynamic-type-get-smoosh-of-one-reports
      a-dt a)
    (uninformative-smoosh-reports)))

(define/own-contract
  (dynamic-type-get-smoosh-and-comparison-of-two-reports-via-first
    dt a b)
  (-> any/c any/c any/c
    ; For each report in the infinite sequence, the next report says
    ; not only whether they smoosh along that one's == but also, only
    ; if they do, how their information ordering representatives
    ; smoosh along their information ordering.
    (endless-sequence/c smoosh-and-comparison-of-two-report?))
  (if (expressly-smooshable-dynamic-type? dt)
    (expressly-smooshable-dynamic-type-get-smoosh-and-comparison-of-two-reports-via-first
      dt a b)
    (uninformative-smoosh-and-comparison-of-two-reports)))

(define/own-contract
  (dynamic-type-get-smoosh-and-comparison-of-two-reports-via-second
    dt a b)
  (-> any/c any/c any/c
    ; For each report in the infinite sequence, the next report says
    ; not only whether they smoosh along that one's == but also, only
    ; if they do, how their information ordering representatives
    ; smoosh along their information ordering.
    (endless-sequence/c smoosh-and-comparison-of-two-report?))
  (if (expressly-smooshable-dynamic-type? dt)
    (expressly-smooshable-dynamic-type-get-smoosh-and-comparison-of-two-reports-via-second
      dt a b)
    (uninformative-smoosh-and-comparison-of-two-reports)))

(define/own-contract
  (dynamic-type-get-smoosh-and-comparison-of-two-reports dt a b)
  (-> any/c any/c any/c
    ; For each report in the infinite sequence, the next report says
    ; not only whether they smoosh along that one's == but also, only
    ; if they do, how their information ordering representatives
    ; smoosh along their information ordering.
    (endless-sequence/c smoosh-and-comparison-of-two-report?))
  (smoosh-and-comparison-of-two-reports-joininfo /list
    (dynamic-type-get-smoosh-and-comparison-of-two-reports-via-first
      dt a b)
    (dynamic-type-get-smoosh-and-comparison-of-two-reports-via-second
      dt a b)))


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
  'uninformative-smoosh-equal-hash-code-support-report
  (current-inspector)
  (auto-write)
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
  (-> (endless-sequence/c smoosh-equal-hash-code-support-report?))
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
    (-> any/c any/c (endless-sequence/c smoosh-report?))
    
    expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type-impl?)
  (make-expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type-impl-from-various-unkeyworded
    get-smoosh-equal-hash-code-support-reports))

(define/own-contract
  (dynamic-type-get-smoosh-equal-hash-code-support-reports a-dt a)
  (-> any/c any/c (endless-sequence/c smoosh-report?))
  (if
    (expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type?
      a-dt)
    (expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type-get-smoosh-equal-hash-code-support-reports
      a-dt a)
    (uninformative-smoosh-equal-hash-code-support-reports)))


(define-imitation-simple-generics
  expressly-potentially-an-s-expression-landmark-dynamic-type?
  expressly-potentially-an-s-expression-landmark-dynamic-type-impl?
  (#:method
    expressly-potentially-an-s-expression-landmark-dynamic-type-value-s-expression-landmark?-knowable
    (#:this)
    ())
  prop:expressly-potentially-an-s-expression-landmark-dynamic-type
  make-expressly-potentially-an-s-expression-landmark-dynamic-type-impl-from-various-unkeyworded
  'expressly-potentially-an-s-expression-landmark-dynamic-type
  'expressly-potentially-an-s-expression-landmark-dynamic-type-impl
  (list))
(ascribe-own-contract
  expressly-potentially-an-s-expression-landmark-dynamic-type-impl?
  (-> any/c boolean?))
(ascribe-own-contract
  prop:expressly-potentially-an-s-expression-landmark-dynamic-type
  (struct-type-property/c
    expressly-potentially-an-s-expression-landmark-dynamic-type-impl?))

(define/own-contract
  (make-expressly-potentially-an-s-expression-landmark-dynamic-type-impl
    #:value-s-expression-landmark?-knowable
    value-s-expression-landmark?-knowable)
  (->
    #:value-s-expression-landmark?-knowable
    (-> any/c any/c (knowable/c boolean?))
    
    expressly-potentially-an-s-expression-landmark-dynamic-type-impl?)
  (make-expressly-potentially-an-s-expression-landmark-dynamic-type-impl-from-various-unkeyworded
    value-s-expression-landmark?-knowable))

(define/own-contract
  (dynamic-type-value-s-expression-landmark?-knowable a-dt a)
  (-> any/c any/c (knowable/c boolean?))
  (if
    (expressly-potentially-an-s-expression-landmark-dynamic-type?
      a-dt)
    (expressly-potentially-an-s-expression-landmark-dynamic-type-value-s-expression-landmark?-knowable
      a-dt a)
    (known #f)))

(define/own-contract (known-s-expression-landmark-or-not? v)
  (-> any/c boolean?)
  (known? /dynamic-type-value-s-expression-landmark?-knowable
    ; TODO FORWARD: This use of `any-dynamic-type` is a forward
    ; reference. See if we can untangle it.
    (any-dynamic-type)
    v))


(define-imitation-simple-struct
  (mapped-smoosh-report?
    mapped-smoosh-report-on-join-yknow-maybe-yknow
    mapped-smoosh-report-on-meet-yknow-maybe-yknow
    mapped-smoosh-report-on-==-yknow-maybe-yknow
    mapped-smoosh-report-on-path-related-yknow-maybe-yknow
    mapped-smoosh-report-original)
  mapped-smoosh-report 'mapped-smoosh-report (current-inspector)
  (auto-write)
  (#:prop prop:smoosh-report /make-smoosh-report-impl
    
    #:join-yknow-maybe-yknow
    (dissectfn
      (mapped-smoosh-report
        on-join-yknow-maybe-yknow
        on-meet-yknow-maybe-yknow
        on-==-yknow-maybe-yknow
        on-path-related-yknow-maybe-yknow
        original)
      (on-join-yknow-maybe-yknow
        (smoosh-report-join-yknow-maybe-yknow original)))
    
    #:meet-yknow-maybe-yknow
    (dissectfn
      (mapped-smoosh-report
        on-join-yknow-maybe-yknow
        on-meet-yknow-maybe-yknow
        on-==-yknow-maybe-yknow
        on-path-related-yknow-maybe-yknow
        original)
      (on-meet-yknow-maybe-yknow
        (smoosh-report-meet-yknow-maybe-yknow
          original)))
    
    #:==-yknow-maybe-yknow
    (dissectfn
      (mapped-smoosh-report
        on-join-yknow-maybe-yknow
        on-meet-yknow-maybe-yknow
        on-==-yknow-maybe-yknow
        on-path-related-yknow-maybe-yknow
        original)
      (on-==-yknow-maybe-yknow
        (smoosh-report-==-yknow-maybe-yknow
          original)))
    
    #:path-related-yknow-maybe-yknow
    (dissectfn
      (mapped-smoosh-report
        on-join-yknow-maybe-yknow
        on-meet-yknow-maybe-yknow
        on-==-yknow-maybe-yknow
        on-path-related-yknow-maybe-yknow
        original)
      (on-path-related-yknow-maybe-yknow
        (smoosh-report-path-related-yknow-maybe-yknow
          original)))
    
    ))

(define/own-contract
  (smoosh-report-map report
    
    #:on-smoosh-result-knowable-promise-maybe-knowable-promise
    [ on-smoosh-result-knowable-promise-maybe-knowable-promise
      (fn kpmkp
        kpmkp)]
    
    #:on-join-yknow-maybe-yknow
    [ on-join-yknow-maybe-yknow
      on-smoosh-result-knowable-promise-maybe-knowable-promise]
    
    #:on-meet-yknow-maybe-yknow
    [ on-meet-yknow-maybe-yknow
      on-smoosh-result-knowable-promise-maybe-knowable-promise]
    
    #:on-==-yknow-maybe-yknow
    [ on-==-yknow-maybe-yknow
      on-smoosh-result-knowable-promise-maybe-knowable-promise]
    
    #:on-path-related-yknow-maybe-yknow
    [ on-path-related-yknow-maybe-yknow
      on-smoosh-result-knowable-promise-maybe-knowable-promise]
    
    )
  (->*
    (smoosh-report?)
    (
      #:on-smoosh-result-knowable-promise-maybe-knowable-promise
      (-> (yknow/c (maybe/c yknow?)) (yknow/c (maybe/c yknow?)))
      
      #:on-join-yknow-maybe-yknow
      (-> (yknow/c (maybe/c yknow?)) (yknow/c (maybe/c yknow?)))
      
      #:on-meet-yknow-maybe-yknow
      (-> (yknow/c (maybe/c yknow?)) (yknow/c (maybe/c yknow?)))
      
      #:on-==-yknow-maybe-yknow
      (-> (yknow/c (maybe/c yknow?)) (yknow/c (maybe/c yknow?)))
      
      #:on-path-related-yknow-maybe-yknow
      (-> (yknow/c (maybe/c yknow?)) (yknow/c (maybe/c yknow?)))
      
      )
    smoosh-report?)
  (mapped-smoosh-report
    on-join-yknow-maybe-yknow
    on-meet-yknow-maybe-yknow
    on-==-yknow-maybe-yknow
    on-path-related-yknow-maybe-yknow
    report))

(define/own-contract
  (smoosh-reports-map reports
    
    #:on-smoosh-result-knowable-promise-maybe-knowable-promise
    [ on-smoosh-result-knowable-promise-maybe-knowable-promise
      (fn kpmkp
        kpmkp)]
    
    #:on-join-yknow-maybe-yknow
    [ on-join-yknow-maybe-yknow
      on-smoosh-result-knowable-promise-maybe-knowable-promise]
    
    #:on-meet-yknow-maybe-yknow
    [ on-meet-yknow-maybe-yknow
      on-smoosh-result-knowable-promise-maybe-knowable-promise]
    
    #:on-==-yknow-maybe-yknow
    [ on-==-yknow-maybe-yknow
      on-smoosh-result-knowable-promise-maybe-knowable-promise]
    
    #:on-path-related-yknow-maybe-yknow
    [ on-path-related-yknow-maybe-yknow
      on-smoosh-result-knowable-promise-maybe-knowable-promise]
    
    )
  (->*
    ((endless-sequence/c smoosh-report?))
    (
      #:on-smoosh-result-knowable-promise-maybe-knowable-promise
      (-> (yknow/c (maybe/c yknow?)) (yknow/c (maybe/c yknow?)))
      
      #:on-join-yknow-maybe-yknow
      (-> (yknow/c (maybe/c yknow?)) (yknow/c (maybe/c yknow?)))
      
      #:on-meet-yknow-maybe-yknow
      (-> (yknow/c (maybe/c yknow?)) (yknow/c (maybe/c yknow?)))
      
      #:on-==-yknow-maybe-yknow
      (-> (yknow/c (maybe/c yknow?)) (yknow/c (maybe/c yknow?)))
      
      #:on-path-related-yknow-maybe-yknow
      (-> (yknow/c (maybe/c yknow?)) (yknow/c (maybe/c yknow?)))
      
      )
    (endless-sequence/c smoosh-report?))
  (sequence-map
    (fn report
      (smoosh-report-map report
        #:on-join-yknow-maybe-yknow on-join-yknow-maybe-yknow
        #:on-meet-yknow-maybe-yknow on-meet-yknow-maybe-yknow
        #:on-==-yknow-maybe-yknow on-==-yknow-maybe-yknow
        
        #:on-path-related-yknow-maybe-yknow
        on-path-related-yknow-maybe-yknow
        
        ))
    reports))

(define-imitation-simple-struct
  (zip*-mapped-smoosh-report?
    zip*-mapped-smoosh-report-on-join-yknow-maybe-yknow
    zip*-mapped-smoosh-report-on-meet-yknow-maybe-yknow
    zip*-mapped-smoosh-report-on-==-yknow-maybe-yknow
    zip*-mapped-smoosh-report-on-path-related-yknow-maybe-yknow
    zip*-mapped-smoosh-report-original-list)
  zip*-mapped-smoosh-report
  'zip*-mapped-smoosh-report (current-inspector)
  (auto-write)
  (#:prop prop:smoosh-report /make-smoosh-report-impl
    
    #:join-yknow-maybe-yknow
    (dissectfn
      (zip*-mapped-smoosh-report
        on-join-yknow-maybe-yknow
        on-meet-yknow-maybe-yknow
        on-==-yknow-maybe-yknow
        on-path-related-yknow-maybe-yknow
        original-list)
      (on-join-yknow-maybe-yknow
        (list-map original-list /fn original
          (smoosh-report-join-yknow-maybe-yknow original))))
    
    #:meet-yknow-maybe-yknow
    (dissectfn
      (zip*-mapped-smoosh-report
        on-join-yknow-maybe-yknow
        on-meet-yknow-maybe-yknow
        on-==-yknow-maybe-yknow
        on-path-related-yknow-maybe-yknow
        original-list)
      (on-meet-yknow-maybe-yknow
        (list-map original-list /fn original
          (smoosh-report-meet-yknow-maybe-yknow original))))
    
    #:==-yknow-maybe-yknow
    (dissectfn
      (zip*-mapped-smoosh-report
        on-join-yknow-maybe-yknow
        on-meet-yknow-maybe-yknow
        on-==-yknow-maybe-yknow
        on-path-related-yknow-maybe-yknow
        original-list)
      (on-==-yknow-maybe-yknow
        (list-map original-list /fn original
          (smoosh-report-==-yknow-maybe-yknow original))))
    
    #:path-related-yknow-maybe-yknow
    (dissectfn
      (zip*-mapped-smoosh-report
        on-join-yknow-maybe-yknow
        on-meet-yknow-maybe-yknow
        on-==-yknow-maybe-yknow
        on-path-related-yknow-maybe-yknow
        original-list)
      (on-path-related-yknow-maybe-yknow
        (list-map original-list /fn original
          (smoosh-report-path-related-yknow-maybe-yknow original))))
    
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
    
    #:on-join-yknow-maybe-yknow
    [ on-join-yknow-maybe-yknow
      on-smoosh-result-knowable-promise-maybe-knowable-promise]
    
    #:on-meet-yknow-maybe-yknow
    [ on-meet-yknow-maybe-yknow
      on-smoosh-result-knowable-promise-maybe-knowable-promise]
    
    #:on-==-yknow-maybe-yknow
    [ on-==-yknow-maybe-yknow
      on-smoosh-result-knowable-promise-maybe-knowable-promise]
    
    #:on-path-related-yknow-maybe-yknow
    [ on-path-related-yknow-maybe-yknow
      on-smoosh-result-knowable-promise-maybe-knowable-promise]
    
    )
  (->*
    ((listof smoosh-report?))
    (
      #:on-smoosh-result-knowable-promise-maybe-knowable-promise
      (-> (listof (yknow/c (maybe/c yknow?)))
        (yknow/c (maybe/c yknow?)))
      
      #:on-join-yknow-maybe-yknow
      (-> (listof (yknow/c (maybe/c yknow?)))
        (yknow/c (maybe/c yknow?)))
      
      #:on-meet-yknow-maybe-yknow
      (-> (listof (yknow/c (maybe/c yknow?)))
        (yknow/c (maybe/c yknow?)))
      
      #:on-==-yknow-maybe-yknow
      (-> (listof (yknow/c (maybe/c yknow?)))
        (yknow/c (maybe/c yknow?)))
      
      #:on-path-related-yknow-maybe-yknow
      (-> (listof (yknow/c (maybe/c yknow?)))
        (yknow/c (maybe/c yknow?)))
      
      )
    smoosh-report?)
  (zip*-mapped-smoosh-report
    on-join-yknow-maybe-yknow
    on-meet-yknow-maybe-yknow
    on-==-yknow-maybe-yknow
    on-path-related-yknow-maybe-yknow
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
    
    #:on-join-yknow-maybe-yknow
    [ on-join-yknow-maybe-yknow
      on-smoosh-result-knowable-promise-maybe-knowable-promise]
    
    #:on-meet-yknow-maybe-yknow
    [ on-meet-yknow-maybe-yknow
      on-smoosh-result-knowable-promise-maybe-knowable-promise]
    
    #:on-==-yknow-maybe-yknow
    [ on-==-yknow-maybe-yknow
      on-smoosh-result-knowable-promise-maybe-knowable-promise]
    
    #:on-path-related-yknow-maybe-yknow
    [ on-path-related-yknow-maybe-yknow
      on-smoosh-result-knowable-promise-maybe-knowable-promise]
    
    )
  (->*
    ((listof (endless-sequence/c smoosh-report?)))
    (
      #:on-smoosh-result-knowable-promise-maybe-knowable-promise
      (-> (listof (yknow/c (maybe/c yknow?)))
        (yknow/c (maybe/c yknow?)))
      
      #:on-join-yknow-maybe-yknow
      (-> (listof (yknow/c (maybe/c yknow?)))
        (yknow/c (maybe/c yknow?)))
      
      #:on-meet-yknow-maybe-yknow
      (-> (listof (yknow/c (maybe/c yknow?)))
        (yknow/c (maybe/c yknow?)))
      
      #:on-==-yknow-maybe-yknow
      (-> (listof (yknow/c (maybe/c yknow?)))
        (yknow/c (maybe/c yknow?)))
      
      #:on-path-related-yknow-maybe-yknow
      (-> (listof (yknow/c (maybe/c yknow?)))
        (yknow/c (maybe/c yknow?)))
      
      )
    (endless-sequence/c smoosh-report?))
  (endless-sequence-zip*-map reports-list /fn report-list
    (smoosh-report-zip*-map report-list
      #:on-join-yknow-maybe-yknow on-join-yknow-maybe-yknow
      #:on-meet-yknow-maybe-yknow on-meet-yknow-maybe-yknow
      #:on-==-yknow-maybe-yknow on-==-yknow-maybe-yknow
      
      #:on-path-related-yknow-maybe-yknow
      on-path-related-yknow-maybe-yknow)))

(define-imitation-simple-struct
  (mapped-smoosh-and-comparison-of-two-report?
    mapped-smoosh-and-comparison-of-two-report-on-<=?-yknow
    mapped-smoosh-and-comparison-of-two-report-on->=?-yknow
    mapped-smoosh-and-comparison-of-two-report-on-join-yknow-maybe-yknow
    mapped-smoosh-and-comparison-of-two-report-on-meet-yknow-maybe-yknow
    mapped-smoosh-and-comparison-of-two-report-on-==-yknow-maybe-yknow
    mapped-smoosh-and-comparison-of-two-report-on-path-related-yknow-maybe-yknow
    mapped-smoosh-and-comparison-of-two-report-original)
  mapped-smoosh-and-comparison-of-two-report
  'mapped-smoosh-and-comparison-of-two-report (current-inspector)
  (auto-write)
  (#:prop prop:smoosh-and-comparison-of-two-report
    (make-smoosh-and-comparison-of-two-report-impl
      
      #:<=?-yknow
      (dissectfn
        (mapped-smoosh-and-comparison-of-two-report
          on-<=?-yknow
          on->=?-yknow
          on-join-yknow-maybe-yknow
          on-meet-yknow-maybe-yknow
          on-==-yknow-maybe-yknow
          on-path-related-yknow-maybe-yknow
          original)
        (on-<=?-yknow
          (smoosh-and-comparison-of-two-report-<=?-yknow original)))
      
      #:>=?-yknow
      (dissectfn
        (mapped-smoosh-and-comparison-of-two-report
          on-<=?-yknow
          on->=?-yknow
          on-join-yknow-maybe-yknow
          on-meet-yknow-maybe-yknow
          on-==-yknow-maybe-yknow
          on-path-related-yknow-maybe-yknow
          original)
        (on->=?-yknow
          (smoosh-and-comparison-of-two-report->=?-yknow original)))
      
      #:get-smoosh-report
      (dissectfn
        (mapped-smoosh-and-comparison-of-two-report
          on-<=?-yknow
          on->=?-yknow
          on-join-yknow-maybe-yknow
          on-meet-yknow-maybe-yknow
          on-==-yknow-maybe-yknow
          on-path-related-yknow-maybe-yknow
          original)
        (smoosh-report-map
          (smoosh-and-comparison-of-two-report-get-smoosh-report
            original)
          #:on-join-yknow-maybe-yknow on-join-yknow-maybe-yknow
          #:on-meet-yknow-maybe-yknow on-meet-yknow-maybe-yknow
          #:on-==-yknow-maybe-yknow on-==-yknow-maybe-yknow
          
          #:on-path-related-yknow-maybe-yknow
          on-path-related-yknow-maybe-yknow
          
          ))
      
      )))

(define/own-contract
  (smoosh-and-comparison-of-two-report-map report
    
    #:on-check-result-yknow
    [ on-check-result-yknow
      (fn y
        y)]
    
    #:on-<=?-yknow [on-<=?-yknow on-check-result-yknow]
    #:on->=?-yknow [on->=?-yknow on-check-result-yknow]
    
    #:on-smoosh-result-yknow-maybe-yknow
    [ on-smoosh-result-yknow-maybe-yknow
      (fn ymy
        ymy)]
    
    #:on-join-yknow-maybe-yknow
    [on-join-yknow-maybe-yknow on-smoosh-result-yknow-maybe-yknow]
    
    #:on-meet-yknow-maybe-yknow
    [on-meet-yknow-maybe-yknow on-smoosh-result-yknow-maybe-yknow]
    
    #:on-==-yknow-maybe-yknow
    [on-==-yknow-maybe-yknow on-smoosh-result-yknow-maybe-yknow]
    
    #:on-path-related-yknow-maybe-yknow
    [ on-path-related-yknow-maybe-yknow
      on-smoosh-result-yknow-maybe-yknow]
    
    )
  (->*
    (smoosh-and-comparison-of-two-report?)
    (
      #:on-check-result-yknow
      (-> (yknow/c boolean?) (yknow/c boolean?))
      
      #:on-<=?-yknow (-> (yknow/c boolean?) (yknow/c boolean?))
      #:on->=?-yknow (-> (yknow/c boolean?) (yknow/c boolean?))
      
      #:on-smoosh-result-yknow-maybe-yknow
      (-> (yknow/c (maybe/c yknow?)) (yknow/c (maybe/c yknow?)))
      
      #:on-join-yknow-maybe-yknow
      (-> (yknow/c (maybe/c yknow?)) (yknow/c (maybe/c yknow?)))
      
      #:on-meet-yknow-maybe-yknow
      (-> (yknow/c (maybe/c yknow?)) (yknow/c (maybe/c yknow?)))
      
      #:on-==-yknow-maybe-yknow
      (-> (yknow/c (maybe/c yknow?)) (yknow/c (maybe/c yknow?)))
      
      #:on-path-related-yknow-maybe-yknow
      (-> (yknow/c (maybe/c yknow?)) (yknow/c (maybe/c yknow?)))
      
      )
    smoosh-and-comparison-of-two-report?)
  (mapped-smoosh-and-comparison-of-two-report
    on-<=?-yknow
    on->=?-yknow
    on-join-yknow-maybe-yknow
    on-meet-yknow-maybe-yknow
    on-==-yknow-maybe-yknow
    on-path-related-yknow-maybe-yknow
    report))

(define/own-contract
  (smoosh-and-comparison-of-two-reports-map reports
    
    #:on-check-result-yknow
    [ on-check-result-yknow
      (fn y
        y)]
    
    #:on-<=?-yknow [on-<=?-yknow on-check-result-yknow]
    #:on->=?-yknow [on->=?-yknow on-check-result-yknow]
    
    #:on-smoosh-result-yknow-maybe-yknow
    [ on-smoosh-result-yknow-maybe-yknow
      (fn ymy
        ymy)]
    
    #:on-join-yknow-maybe-yknow
    [on-join-yknow-maybe-yknow on-smoosh-result-yknow-maybe-yknow]
    
    #:on-meet-yknow-maybe-yknow
    [on-meet-yknow-maybe-yknow on-smoosh-result-yknow-maybe-yknow]
    
    #:on-==-yknow-maybe-yknow
    [on-==-yknow-maybe-yknow on-smoosh-result-yknow-maybe-yknow]
    
    #:on-path-related-yknow-maybe-yknow
    [ on-path-related-yknow-maybe-yknow
      on-smoosh-result-yknow-maybe-yknow]
    
    )
  (->*
    ((endless-sequence/c smoosh-and-comparison-of-two-report?))
    (
      #:on-check-result-yknow
      (-> (yknow/c boolean?) (yknow/c boolean?))
      
      #:on-<=?-yknow (-> (yknow/c boolean?) (yknow/c boolean?))
      #:on->=?-yknow (-> (yknow/c boolean?) (yknow/c boolean?))
      
      #:on-smoosh-result-yknow-maybe-yknow
      (-> (yknow/c (maybe/c yknow?)) (yknow/c (maybe/c yknow?)))
      
      #:on-join-yknow-maybe-yknow
      (-> (yknow/c (maybe/c yknow?)) (yknow/c (maybe/c yknow?)))
      
      #:on-meet-yknow-maybe-yknow
      (-> (yknow/c (maybe/c yknow?)) (yknow/c (maybe/c yknow?)))
      
      #:on-==-yknow-maybe-yknow
      (-> (yknow/c (maybe/c yknow?)) (yknow/c (maybe/c yknow?)))
      
      #:on-path-related-yknow-maybe-yknow
      (-> (yknow/c (maybe/c yknow?)) (yknow/c (maybe/c yknow?)))
      
      )
    (endless-sequence/c smoosh-and-comparison-of-two-report?))
  (sequence-map
    (fn report
      (smoosh-and-comparison-of-two-report-map report
        #:on-<=?-yknow on-<=?-yknow
        #:on->=?-yknow on->=?-yknow
        #:on-join-yknow-maybe-yknow on-join-yknow-maybe-yknow
        #:on-meet-yknow-maybe-yknow on-meet-yknow-maybe-yknow
        #:on-==-yknow-maybe-yknow on-==-yknow-maybe-yknow
        
        #:on-path-related-yknow-maybe-yknow
        on-path-related-yknow-maybe-yknow))
    reports))

(define-imitation-simple-struct
  (zip*-mapped-smoosh-and-comparison-of-two-report?
    zip*-mapped-smoosh-and-comparison-of-two-report-on-<=?-yknow
    zip*-mapped-smoosh-and-comparison-of-two-report-on->=?-yknow
    zip*-mapped-smoosh-and-comparison-of-two-report-on-join-yknow-maybe-yknow
    zip*-mapped-smoosh-and-comparison-of-two-report-on-meet-yknow-maybe-yknow
    zip*-mapped-smoosh-and-comparison-of-two-report-on-==-yknow-maybe-yknow
    zip*-mapped-smoosh-and-comparison-of-two-report-on-path-related-yknow-maybe-yknow
    zip*-mapped-smoosh-and-comparison-of-two-report-original-list)
  zip*-mapped-smoosh-and-comparison-of-two-report
  'zip*-mapped-smoosh-and-comparison-of-two-report (current-inspector)
  (auto-write)
  (#:prop prop:smoosh-and-comparison-of-two-report
    (make-smoosh-and-comparison-of-two-report-impl
      
      #:<=?-yknow
      (dissectfn
        (zip*-mapped-smoosh-and-comparison-of-two-report
          on-<=?-yknow
          on->=?-yknow
          on-join-yknow-maybe-yknow
          on-meet-yknow-maybe-yknow
          on-==-yknow-maybe-yknow
          on-path-related-yknow-maybe-yknow
          original-list)
        (on-<=?-yknow
          (list-map original-list /fn original
            (smoosh-and-comparison-of-two-report-<=?-yknow
              original))))
      
      #:>=?-yknow
      (dissectfn
        (zip*-mapped-smoosh-and-comparison-of-two-report
          on-<=?-yknow
          on->=?-yknow
          on-join-yknow-maybe-yknow
          on-meet-yknow-maybe-yknow
          on-==-yknow-maybe-yknow
          on-path-related-yknow-maybe-yknow
          original-list)
        (on->=?-yknow
          (list-map original-list /fn original
            (smoosh-and-comparison-of-two-report->=?-yknow
              original))))
      
      #:get-smoosh-report
      (dissectfn
        (zip*-mapped-smoosh-and-comparison-of-two-report
          on-<=?-yknow
          on->=?-yknow
          on-join-yknow-maybe-yknow
          on-meet-yknow-maybe-yknow
          on-==-yknow-maybe-yknow
          on-path-related-yknow-maybe-yknow
          original-list)
        (smoosh-report-zip*-map
          (list-map original-list /fn original
            (smoosh-and-comparison-of-two-report-get-smoosh-report
              original))
          #:on-join-yknow-maybe-yknow on-join-yknow-maybe-yknow
          #:on-meet-yknow-maybe-yknow on-meet-yknow-maybe-yknow
          #:on-==-yknow-maybe-yknow on-==-yknow-maybe-yknow
          
          #:on-path-related-yknow-maybe-yknow
          on-path-related-yknow-maybe-yknow))
      
      )))

(define/own-contract
  (smoosh-and-comparison-of-two-report-zip*-map report-list
    
    #:on-check-result-yknow
    [ on-check-result-yknow
      (fn y-list
        (raise-arguments-error 'smoosh-and-comparison-of-two-report-zip*-map
          "tried to retrieve a check result when its mapping behavior was undefined"
          "check-result-yknow-list" y-list))]
    
    #:on-<=?-yknow [on-<=?-yknow on-check-result-yknow]
    #:on->=?-yknow [on->=?-yknow on-check-result-yknow]
    
    #:on-smoosh-result-yknow-maybe-yknow
    [ on-smoosh-result-yknow-maybe-yknow
      (fn ymy-list
        (raise-arguments-error 'smoosh-and-comparison-of-two-report-zip*-map
          "tried to retrieve a smoosh result when its mapping behavior was undefined"
          "smoosh-result-yknow-maybe-yknow-list" ymy-list))]
    
    #:on-join-yknow-maybe-yknow
    [on-join-yknow-maybe-yknow on-smoosh-result-yknow-maybe-yknow]
    
    #:on-meet-yknow-maybe-yknow
    [on-meet-yknow-maybe-yknow on-smoosh-result-yknow-maybe-yknow]
    
    #:on-==-yknow-maybe-yknow
    [on-==-yknow-maybe-yknow on-smoosh-result-yknow-maybe-yknow]
    
    #:on-path-related-yknow-maybe-yknow
    [ on-path-related-yknow-maybe-yknow
      on-smoosh-result-yknow-maybe-yknow]
    
    )
  (->*
    ((listof smoosh-and-comparison-of-two-report?))
    (
      #:on-check-result-yknow
      (-> (listof (yknow/c boolean?)) (yknow/c boolean?))
      
      #:on-<=?-yknow
      (-> (listof (yknow/c boolean?)) (yknow/c boolean?))
      
      #:on->=?-yknow
      (-> (listof (yknow/c boolean?)) (yknow/c boolean?))
      
      #:on-smoosh-result-yknow-maybe-yknow
      (-> (listof (yknow/c (maybe/c yknow?)))
        (yknow/c (maybe/c yknow?)))
      
      #:on-join-yknow-maybe-yknow
      (-> (listof (yknow/c (maybe/c yknow?)))
        (yknow/c (maybe/c yknow?)))
      
      #:on-meet-yknow-maybe-yknow
      (-> (listof (yknow/c (maybe/c yknow?)))
        (yknow/c (maybe/c yknow?)))
      
      #:on-==-yknow-maybe-yknow
      (-> (listof (yknow/c (maybe/c yknow?)))
        (yknow/c (maybe/c yknow?)))
      
      #:on-path-related-yknow-maybe-yknow
      (-> (listof (yknow/c (maybe/c yknow?)))
        (yknow/c (maybe/c yknow?)))
      
      )
    smoosh-and-comparison-of-two-report?)
  (zip*-mapped-smoosh-and-comparison-of-two-report
    on-<=?-yknow
    on->=?-yknow
    on-join-yknow-maybe-yknow
    on-meet-yknow-maybe-yknow
    on-==-yknow-maybe-yknow
    on-path-related-yknow-maybe-yknow
    report-list))

(define/own-contract
  (smoosh-and-comparison-of-two-reports-zip*-map reports-list
    
    #:on-check-result-yknow
    [ on-check-result-yknow
      (fn y-list
        (raise-arguments-error 'smoosh-and-comparison-of-two-report-zip*-map
          "tried to retrieve a check result when its mapping behavior was undefined"
          "check-result-yknow-list" y-list))]
    
    #:on-<=?-yknow [on-<=?-yknow on-check-result-yknow]
    #:on->=?-yknow [on->=?-yknow on-check-result-yknow]
    
    #:on-smoosh-result-yknow-maybe-yknow
    [ on-smoosh-result-yknow-maybe-yknow
      (fn ymy-list
        (raise-arguments-error 'smoosh-and-comparison-of-two-report-zip*-map
          "tried to retrieve a smoosh result when its mapping behavior was undefined"
          "smoosh-result-yknow-maybe-yknow-list" ymy-list))]
    
    #:on-join-yknow-maybe-yknow
    [on-join-yknow-maybe-yknow on-smoosh-result-yknow-maybe-yknow]
    
    #:on-meet-yknow-maybe-yknow
    [on-meet-yknow-maybe-yknow on-smoosh-result-yknow-maybe-yknow]
    
    #:on-==-yknow-maybe-yknow
    [on-==-yknow-maybe-yknow on-smoosh-result-yknow-maybe-yknow]
    
    #:on-path-related-yknow-maybe-yknow
    [ on-path-related-yknow-maybe-yknow
      on-smoosh-result-yknow-maybe-yknow]
    
    )
  (->*
    (
      (listof
        (endless-sequence/c smoosh-and-comparison-of-two-report?)))
    (
      #:on-check-result-yknow
      (-> (listof (yknow/c boolean?)) (yknow/c boolean?))
      
      #:on-<=?-yknow
      (-> (listof (yknow/c boolean?)) (yknow/c boolean?))
      
      #:on->=?-yknow
      (-> (listof (yknow/c boolean?)) (yknow/c boolean?))
      
      #:on-smoosh-result-yknow-maybe-yknow
      (-> (listof (yknow/c (maybe/c yknow?)))
        (yknow/c (maybe/c yknow?)))
      
      #:on-join-yknow-maybe-yknow
      (-> (listof (yknow/c (maybe/c yknow?)))
        (yknow/c (maybe/c yknow?)))
      
      #:on-meet-yknow-maybe-yknow
      (-> (listof (yknow/c (maybe/c yknow?)))
        (yknow/c (maybe/c yknow?)))
      
      #:on-==-yknow-maybe-yknow
      (-> (listof (yknow/c (maybe/c yknow?)))
        (yknow/c (maybe/c yknow?)))
      
      #:on-path-related-yknow-maybe-yknow
      (-> (listof (yknow/c (maybe/c yknow?)))
        (yknow/c (maybe/c yknow?)))
      
      )
    (endless-sequence/c smoosh-and-comparison-of-two-report?))
  (endless-sequence-zip*-map reports-list /fn report-list
    (smoosh-and-comparison-of-two-report-zip*-map report-list
      #:on-<=?-yknow on-<=?-yknow
      #:on->=?-yknow on->=?-yknow
      #:on-join-yknow-maybe-yknow on-join-yknow-maybe-yknow
      #:on-meet-yknow-maybe-yknow on-meet-yknow-maybe-yknow
      #:on-==-yknow-maybe-yknow on-==-yknow-maybe-yknow
      
      #:on-path-related-yknow-maybe-yknow
      on-path-related-yknow-maybe-yknow)))


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
    ((endless-sequence/c smoosh-equal-hash-code-support-report?))
    (
      #:on-hash-code-promise
      (-> (promise/c fixnum?) (promise/c fixnum?))
      
      #:on-==-hash-code-promise
      (-> (promise/c fixnum?) (promise/c fixnum?))
      
      #:on-path-related-hash-code-promise
      (-> (promise/c fixnum?) (promise/c fixnum?))
      
      )
    (endless-sequence/c smoosh-equal-hash-code-support-report?))
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
  'zip*-mapped-smoosh-equal-hash-code-support-report
  (current-inspector)
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
    (
      (listof
        (endless-sequence/c smoosh-equal-hash-code-support-report?)))
    (
      #:on-hash-code-promise
      (-> (listof (promise/c fixnum?)) (promise/c fixnum?))
      
      #:on-==-hash-code-promise
      (-> (listof (promise/c fixnum?)) (promise/c fixnum?))
      
      #:on-path-related-hash-code-promise
      (-> (listof (promise/c fixnum?)) (promise/c fixnum?))
      
      )
    (endless-sequence/c smoosh-equal-hash-code-support-report?))
  (endless-sequence-zip*-map reports-list /fn report-list
    (smoosh-equal-hash-code-support-report-zip*-map report-list
      #:on-==-hash-code-promise on-==-hash-code-promise
      
      #:on-path-related-hash-code-promise
      on-path-related-hash-code-promise)))

(define/own-contract (false-smoosh-and-comparison-of-two-reports)
  (-> (endless-sequence/c smoosh-and-comparison-of-two-report?))
  (smoosh-and-comparison-of-two-reports-zip*-map (list)
    #:on-check-result-yknow
    (dissectfn (list)
      (make-yknow-from-value #f))
    #:on-smoosh-result-yknow-maybe-yknow
    (dissectfn (list)
      (make-yknow-from-value /nothing))))

(define-imitation-simple-struct
  (constant-smoosh-report?
    constant-smoosh-report-result-yknow-maybe-yknow)
  constant-smoosh-report-unguarded
  'constant-smoosh-report (current-inspector) (auto-write)
  (#:prop prop:smoosh-report /make-smoosh-report-impl
    
    #:join-yknow-maybe-yknow
    (dissectfn
      (constant-smoosh-report-unguarded result-yknow-maybe-yknow)
      result-yknow-maybe-yknow)
    
    #:meet-yknow-maybe-yknow
    (dissectfn
      (constant-smoosh-report-unguarded result-yknow-maybe-yknow)
      result-yknow-maybe-yknow)
    
    #:==-yknow-maybe-yknow
    (dissectfn
      (constant-smoosh-report-unguarded result-yknow-maybe-yknow)
      result-yknow-maybe-yknow)
    
    #:path-related-yknow-maybe-yknow
    (dissectfn
      (constant-smoosh-report-unguarded result-yknow-maybe-yknow)
      result-yknow-maybe-yknow)
    
    ))

(define/own-contract (constant-smoosh-report result-yknow-maybe-yknow)
  (-> (yknow/c (maybe/c yknow?)) smoosh-report?)
  (constant-smoosh-report-unguarded result-yknow-maybe-yknow))

(define/own-contract
  (constant-smoosh-reports result-yknow-maybe-yknow)
  (-> (yknow/c (maybe/c yknow?)) (endless-sequence/c smoosh-report?))
  (in-cycle /list /constant-smoosh-report result-yknow-maybe-yknow))

(define-imitation-simple-struct
  (constant-smoosh-and-comparison-of-two-report?
    constant-smoosh-and-comparison-of-two-report-result-yknow-maybe-yknow)
  constant-smoosh-and-comparison-of-two-report-unguarded
  'constant-smoosh-and-comparison-of-two-report (current-inspector)
  (auto-write)
  (#:prop prop:smoosh-and-comparison-of-two-report
    (make-smoosh-and-comparison-of-two-report-impl
      
      #:<=?-yknow
      (dissectfn
        (constant-smoosh-and-comparison-of-two-report-unguarded
          result-yknow-maybe-yknow)
        (yknow-map result-yknow-maybe-yknow /fn ym /just? ym))
      
      #:>=?-yknow
      (dissectfn
        (constant-smoosh-and-comparison-of-two-report-unguarded
          result-yknow-maybe-yknow)
        (yknow-map result-yknow-maybe-yknow /fn ym /just? ym))
      
      #:get-smoosh-report
      (dissectfn
        (constant-smoosh-and-comparison-of-two-report-unguarded
          result-yknow-maybe-yknow)
        (constant-smoosh-report result-yknow-maybe-yknow))
      
      )))

(define/own-contract
  (constant-smoosh-and-comparison-of-two-report
    result-yknow-maybe-yknow)
  (-> (yknow/c (maybe/c yknow?)) smoosh-and-comparison-of-two-report?)
  (constant-smoosh-and-comparison-of-two-report-unguarded
    result-yknow-maybe-yknow))

(define/own-contract
  (constant-smoosh-and-comparison-of-two-reports
    result-yknow-maybe-yknow)
  (-> (yknow/c (maybe/c yknow?))
    (endless-sequence/c smoosh-and-comparison-of-two-report?))
  (in-cycle /list /constant-smoosh-and-comparison-of-two-report
    result-yknow-maybe-yknow))

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
    (endless-sequence/c smoosh-equal-hash-code-support-report?))
  (in-cycle /list /constant-smoosh-equal-hash-code-support-report
    hash-code-promise))

(define-imitation-simple-struct (eq-wrapper? eq-wrapper-value)
  eq-wrapper 'eq-wrapper (current-inspector) (auto-write)
  ; We use a comparison that consistently compares the value using
  ; `eq?`.
  (#:gen gen:equal-mode+hash
    
    (define (equal-mode-proc a b recur now?)
      (dissect a (eq-wrapper a-value)
      /dissect b (eq-wrapper b-value)
      /eq? a-value b-value))
    
    (define (hash-mode-proc v recur now?)
      (dissect v (eq-wrapper v-value)
      /hash-code-combine
        (equal-always-hash-code eq-wrapper?)
        (eq-hash-code v-value)))
    
    ))
(ascribe-own-contract eq-wrapper? (-> any/c boolean?))
(ascribe-own-contract eq-wrapper-value (-> eq-wrapper? any/c))

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
  (smoosh-and-comparison-of-two-report-joininfo reports-list)
  (-> (listof smoosh-and-comparison-of-two-report?)
    smoosh-and-comparison-of-two-report?)
  (smoosh-and-comparison-of-two-report-zip*-map reports-list
    #:on-check-result-yknow
    (fn y-list
      (yknow-joininfo* y-list))
    #:on-smoosh-result-yknow-maybe-yknow
    (fn ymy-list
      (yknow-maybe-yknow-joininfo* ymy-list))))

(define/own-contract
  (smoosh-and-comparison-of-two-reports-joininfo reports-list)
  (->
    (listof (endless-sequence/c smoosh-and-comparison-of-two-report?))
    (endless-sequence/c smoosh-and-comparison-of-two-report?))
  (endless-sequence-zip*-map reports-list /fn report-list
    (smoosh-and-comparison-of-two-report-joininfo report-list)))

(define/own-contract
  (smoosh-reports-with-hesitation-at-discrepancies reports
    #:known-distinct? [known-distinct? #t]
    #:known-discrete? [known-discrete? #f])
  (->*
    ((endless-sequence/c smoosh-report?))
    (#:known-distinct? boolean? #:known-discrete? boolean?)
    (endless-sequence/c smoosh-report?))
  (if (and known-distinct? known-discrete?) reports
  /w- my->known-true
    (fn my
      (yknow-map/knowable my /fn m /knowable-if (just? m) /fn m))
  /if (not known-distinct?)
    (smoosh-reports-map reports
      #:on-smoosh-result-yknow-maybe-yknow my->known-true)
  /dissect reports (sequence* report-0 report-1+)
  /sequence*
    (smoosh-report-map report-0
      #:on-smoosh-result-yknow-maybe-yknow my->known-true
      
      #:on-==-yknow-maybe-yknow
      (fn ymy
        ymy))
    report-1+))

(define/own-contract
  (smoosh-and-comparison-of-two-reports-with-hesitation-at-discrepancies
    reports
    #:known-distinct? [known-distinct? #t]
    #:known-discrete? [known-discrete? #f])
  (->*
    ((endless-sequence/c smoosh-and-comparison-of-two-report?))
    (#:known-distinct? boolean? #:known-discrete? boolean?)
    (endless-sequence/c smoosh-and-comparison-of-two-report?))
  (if (and known-distinct? known-discrete?) reports
  /w- kp->known-true
    (fn kp
      (promise-map kp /fn k
        (knowable-bind k /fn result
        /knowable-if result /fn result)))
  /w- y->known-true
    (fn y
      (yknow-map/knowable y /fn r /knowable-if r /fn r))
  /w- my->known-true
    (fn my
      (yknow-map/knowable my /fn m /knowable-if (just? m) /fn m))
  /if (not known-distinct?)
    (smoosh-and-comparison-of-two-reports-map reports
      #:on-check-result-yknow y->known-true
      #:on-smoosh-result-yknow-maybe-yknow my->known-true)
  /dissect reports (sequence* report-0 report-1+)
  /sequence*
    (smoosh-and-comparison-of-two-report-map report-0
      #:on-check-result-yknow y->known-true
      #:on-smoosh-result-yknow-maybe-yknow my->known-true
      
      #:on-==-yknow-maybe-yknow
      (fn ymy
        ymy))
    report-1+))

(define/own-contract
  (make-glossesque-sys-impl-for-hash get-summary-sys make-empty-hash)
  (->
    (-> glossesque-sys? glossesque-summary-sys?)
    (-> (and/c hash? immutable?))
    glossesque-sys-impl?)
  (make-glossesque-sys-impl
    
    #:get-summary-sys (fn gs /get-summary-sys gs)
    
    #:glossesque-union-of-zero
    (fn gs
      (w- gss (get-summary-sys gs)
      /summarized (glossesque-summary-sys-summarize-zero gss)
        (make-empty-hash)))
    
    #:glossesque-skv-union-of-two-knowable
    (fn gs state a b a-keeping? b-keeping? on-keep skv-union-knowable
      (w- gss (get-summary-sys gs)
      /summarized-hash-skv-union-of-two-knowable
        gss state a b a-keeping? b-keeping? on-keep
        skv-union-knowable))
    
    #:glossesque-ref-entry-maybe-knowable
    (fn gs g k
      (dissect g (summarized summary h)
      /known /maybe-map (hash-ref-maybe h k) /fn v
        (list (hash-ref-key h k) v)))
    
    #:rider-and-glossesque-update-maybe-knowable
    (fn gs rider-and-g k on-rider-and-m-knowable
      (w- gss (get-summary-sys gs)
      /dissect rider-and-g (list rider /summarized summary h)
      /knowable-map
        (rider-and-hash-update-maybe-knowable
          (list (summarized summary rider) h)
          k
        /dissectfn (list (summarized summary rider) m)
          (w- summary
            (expect m (just v) summary
            /glossesque-summary-sys-summary-minus gss summary
              (glossesque-summary-sys-summarize-one-entry gss k v))
          /knowable-map (on-rider-and-m-knowable /list rider m)
          /dissectfn (list rider m)
            (w- summary
              (expect m (just v) summary
              /glossesque-summary-sys-summary-plus gss summary
                (glossesque-summary-sys-summarize-one-entry gss k v))
            /list (summarized summary rider) m)))
      /dissectfn (list (summarized summary rider) h)
        (list rider /summarized summary h)))
    
    #:glossesque-summarize
    (fn gs g
      (dissect g (summarized summary h)
        summary))
    
    #:glossesque-iteration-sequence
    (fn gs g
      (dissect g (summarized summary h)
      /in-hash h))
    
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

; TODO: Give the resulting contract a better name, check that it has
; good `contract-stronger?` behavior, etc.
; TODO: Let the result be a chaperone contract if the given contracts
; are chaperone contracts.
(define/own-contract (summarized-assoc-list/c summary/c k/c v/c)
  (-> contract? contract? contract? contract?)
  (w- summary/c (coerce-contract 'summarized-assoc-list/c summary/c)
  /w- k/c (coerce-contract 'summarized-assoc-list/c k/c)
  /w- v/c (coerce-contract 'summarized-assoc-list/c v/c)
  /rename-contract
    (fix/c (self/c) /match/c summarized summary/c /or/c (list/c)
      (cons/c (cons/c k/c v/c) /self/c))
    `(summarized-assoc-list/c
      ,(contract-name summary/c)
      ,(contract-name k/c)
      ,(contract-name v/c))))

(define/own-contract (summarized-assoc-list-nil gss)
  (-> glossesque-summary-sys?
    (summarized-assoc-list/c any/c any/c any/c))
  (summarized (glossesque-summary-sys-summarize-zero gss) (list)))

(define/own-contract (summarized-assoc-list-cons gss k v rest)
  (->
    glossesque-summary-sys?
    any/c
    any/c
    (summarized-assoc-list/c any/c any/c any/c)
    (summarized-assoc-list/c any/c any/c any/c))
  (dissect rest (summarized rest-summary rest-unsummarized)
  /summarized
    (glossesque-summary-sys-summary-plus gss
      (glossesque-summary-sys-summarize-one-entry gss k v)
      rest-summary)
    (cons (cons k v) rest)))

(define/own-contract
  (summarized-assoc-list-skv-union-of-two-knowable
    gss ==?-knowable state a b a-keeping? b-keeping? on-keep
    skv-union-knowable)
  (->
    glossesque-summary-sys?
    (-> any/c any/c (knowable/c boolean?))
    any/c
    (summarized-assoc-list/c any/c any/c any/c)
    (summarized-assoc-list/c any/c any/c any/c)
    boolean?
    boolean?
    (-> any/c any/c any/c)
    (-> any/c any/c any/c any/c (knowable/c (list/c any/c maybe?)))
    (knowable/c
      (list/c any/c (summarized-assoc-list/c any/c any/c any/c))))
  (w- a (summarized-assoc-list->assoc-list a)
  /w- b (summarized-assoc-list->assoc-list b)
  /w-loop next
    state state
    a a
    b (reverse b)
    result (summarized-assoc-list-nil gss)
    
    (expect b (cons b-entry b)
      (expect a-keeping? #t
        (known result)
      /w-loop next state state a (reverse a) result result
      /expect a (cons a-entry a)
        (known /list state result)
      /dissect a-entry (cons a-k a-v)
      /next
        a
        (on-keep state
          (glossesque-summary-sys-summarize-one-entry gss a-k a-v))
        (summarized-assoc-list-cons gss a-k a-v result))
    /dissect b-entry (cons b-k b-v)
    /knowable-bind
      (list-rem-first-maybe-knowable a /dissectfn (cons a-k a-v)
        (==?-knowable a-k b-k))
    /fn maybe-a-entry-and-a
    /expect maybe-a-entry-and-a (just a-entry-and-a)
      (if b-keeping?
        (w- state
          (on-keep state
            (glossesque-summary-sys-summarize-one-entry gss b-k b-v))
        /next state a b
          (summarized-assoc-list-cons gss b-k b-v result))
        (next state a b result))
    /dissect a-entry-and-a (list (cons a-k a-v) a)
    /knowable-bind (skv-union-knowable state b-k a-v b-v)
    /dissectfn (list state m)
    /expect m (just v)
      (next state a b result)
    /next state a b (summarized-assoc-list-cons gss b-k v result))))

(define/own-contract
  (summarized-assoc-list-ref-entry-maybe-knowable ==?-knowable a k)
  (->
    (-> any/c any/c (knowable/c boolean?))
    (summarized-assoc-list/c any/c any/c any/c)
    any/c
    (knowable/c (maybe/c (list/c any/c any/c))))
  (w-loop next a a
    (dissect a (summarized _ a)
    /expect a (cons entry a) (known /nothing)
    /dissect entry (cons a-k v)
    /knowable-bind (==?-knowable k a-k) /fn succeeded?
    /if succeeded? (known /just /list a-k v)
    /next a)))

(define/own-contract
  (rider-and-summarized-assoc-list-update-maybe-knowable
    gss ==?-knowable rider-and-a k on-rider-and-m-knowable)
  (->
    glossesque-summary-sys?
    (-> any/c any/c (knowable/c boolean?))
    (list/c any/c (summarized-assoc-list/c any/c any/c any/c))
    any/c
    (-> (list/c any/c maybe?) (knowable/c (list/c any/c maybe?)))
    (knowable/c
      (list/c any/c (summarized-assoc-list/c any/c any/c any/c))))
  (dissect rider-and-a (list rider a)
  /w-loop next a a
    (dissect a (summarized _ a)
    /expect a (cons entry a)
      (knowable-map (on-rider-and-m-knowable /list rider /nothing)
      /dissectfn (list rider m)
        (w- snil (summarized-assoc-list-nil gss)
        /list rider
          (expect m (just v) snil
          /summarized-assoc-list-cons gss k v snil)))
    /dissect entry (cons a-k v)
    /knowable-bind (==?-knowable k a-k) /fn succeeded?
    /if succeeded?
      (knowable-map (on-rider-and-m-knowable /list rider /just v)
      /dissectfn (list rider m)
        (list rider
          (expect m (just v) a
          /summarized-assoc-list-cons gss a-k v a)))
    /knowable-bind (next a) /dissectfn (list rider a)
    /known /list rider /summarized-assoc-list-cons gss a-k v a)))

(define/own-contract
  (summarized-assoc-list->assoc-list a)
  (-> (summarized-assoc-list/c any/c any/c any/c) (listof pair?))
  (dissect a (summarized _ a)
  /expect a (cons entry a) (list)
  /cons entry /summarized-assoc-list->assoc-list a))

(define/own-contract
  (make-glossesque-sys-impl-for-equality-check-knowable
    get-summary-sys
    get-==?-knowable)
  (->
    (-> glossesque-sys? glossesque-summary-sys?)
    (-> glossesque-sys? (-> any/c any/c (knowable/c boolean?)))
    glossesque-sys-impl?)
  (make-glossesque-sys-impl
    
    #:get-summary-sys (fn gs /get-summary-sys gs)
    
    #:glossesque-union-of-zero
    (fn gs
      (w- gss (get-summary-sys gs)
      /summarized-assoc-list-nil gss))
    
    #:glossesque-skv-union-of-two-knowable
    (fn gs state a b a-keeping? b-keeping? on-keep skv-union-knowable
      (w- gss (get-summary-sys gs)
      /w- ==?-knowable (get-==?-knowable gs)
      /summarized-assoc-list-skv-union-of-two-knowable
        gss ==?-knowable state a b a-keeping? b-keeping? on-keep
        skv-union-knowable))
    
    #:glossesque-ref-entry-maybe-knowable
    (fn gs g k
      (w- ==?-knowable (get-==?-knowable gs)
      /summarized-assoc-list-ref-entry-maybe-knowable
        ==?-knowable g k))
    
    #:rider-and-glossesque-update-maybe-knowable
    (fn gs rider-and-g k on-rider-and-m-knowable
      (w- gss (get-summary-sys gs)
      /w- ==?-knowable (get-==?-knowable gs)
      /rider-and-summarized-assoc-list-update-maybe-knowable
        gss ==?-knowable rider-and-g k on-rider-and-m-knowable))
    
    #:glossesque-summarize
    (fn gs g
      (dissect g (summarized summary g)
        summary))
    
    #:glossesque-iteration-sequence
    (fn gs g
      (w- g (summarized-assoc-list->assoc-list g)
      /sequence-map (dissectfn (cons k v) (values k v)) g))
    
    ))

(define-imitation-simple-struct
  (equality-check-knowable-atom-glossesque-sys?
    equality-check-knowable-atom-glossesque-sys-get-summary-sys
    equality-check-knowable-atom-glossesque-sys-get-==?-knowable)
  equality-check-knowable-atom-glossesque-sys-unguarded
  'equality-check-knowable-atom-glossesque-sys (current-inspector)
  (auto-write)
  (#:prop prop:glossesque-sys
    (make-glossesque-sys-impl-for-equality-check-knowable
      (dissectfn
        (equality-check-knowable-atom-glossesque-sys-unguarded
          gss ==?-knowable)
        gss)
      (dissectfn
        (equality-check-knowable-atom-glossesque-sys-unguarded
          gss ==?-knowable)
        ==?-knowable))))

(define/own-contract
  (equality-check-knowable-atom-glossesque-sys gss ==?-knowable)
  (-> glossesque-summary-sys? (-> any/c any/c (knowable/c boolean?))
    glossesque-sys?)
  (equality-check-knowable-atom-glossesque-sys-unguarded
    gss ==?-knowable))

(define/own-contract (equality-check-atom-glossesque-sys gss ==?)
  (-> glossesque-summary-sys? (-> any/c any/c boolean?)
    glossesque-sys?)
  (equality-check-knowable-atom-glossesque-sys gss /fn a b
    (known /==? a b)))

(define/own-contract
  (make-glossesque-sys-impl-for-chaperone=-atom
    get-summary-sys
    get-gs-for-equal-always
    get-gs-for-chaperone=-assuming-equal-always)
  (->
    (-> glossesque-sys? glossesque-summary-sys?)
    (-> glossesque-summary-sys? glossesque-sys?)
    (-> glossesque-summary-sys? glossesque-sys? glossesque-sys?)
    glossesque-sys-impl?)
  (w- get-bin-gs
    (fn gs
      (w- gss (get-summary-sys gs)
      /maybe-nonempty-glossesque-sys
        (get-gs-for-chaperone=-assuming-equal-always gss gs)))
  /w- get-base-gs
    (fn gs
      (w- gss (get-summary-sys gs)
      /w- bin-gs (get-bin-gs gs)
      /get-gs-for-equal-always
        (forwarding-glossesque-summary-sys gss
          #:summarize-one-entry
          (fn k v
            (glossesque-sys-glossesque-summarize bin-gs
              (just v))))))
  /make-glossesque-sys-impl
    
    #:get-summary-sys (fn gs /get-summary-sys gs)
    
    #:glossesque-union-of-zero
    (fn gs
      (w- base-gs (get-base-gs gs)
      /glossesque-sys-glossesque-union-of-zero base-gs))
    
    #:glossesque-skv-union-of-two-knowable
    (fn gs state a b a-keeping? b-keeping? on-keep skv-union-knowable
      (w- base-gs (get-base-gs gs)
      /glossesque-sys-glossesque-skv-union-of-two-knowable
        base-gs state a b a-keeping? b-keeping? on-keep
        (fn state k a-bin b-bin
          (w- bin-gs (get-bin-gs gs)
          /glossesque-sys-glossesque-skv-union-of-two-knowable
            bin-gs state (just a-bin) (just b-bin)
            a-keeping? b-keeping? on-keep
            (fn state k a-v b-v
              (skv-union-knowable state k a-v b-v))))))
    
    #:glossesque-ref-entry-maybe-knowable
    (fn gs g k
      (w- base-gs (get-base-gs gs)
      /knowable-bind
        (glossesque-sys-glossesque-ref-entry-maybe-knowable
          base-gs g k)
      /fn bin-entry-m
      /w- bin-m (maybe-map bin-entry-m /dissectfn (list k bin) bin)
      /w- bin-gs (get-bin-gs gs)
      /glossesque-sys-glossesque-ref-entry-maybe-knowable
        bin-gs bin-m k))
    
    #:rider-and-glossesque-update-maybe-knowable
    (fn gs rider-and-g k on-rider-and-m-knowable
      (w- base-gs (get-base-gs gs)
      /glossesque-sys-rider-and-glossesque-update-maybe-knowable
        base-gs rider-and-g k
        (dissectfn (list rider bin-m)
          (w- bin-gs (get-bin-gs gs)
          /glossesque-sys-rider-and-glossesque-update-maybe-knowable
            bin-gs (list rider bin-m) k
            (fn rider-and-m
              (on-rider-and-m-knowable rider-and-m))))))
    
    #:glossesque-summarize
    (fn gs g
      (w- base-gs (get-base-gs gs)
      /glossesque-sys-glossesque-summarize base-gs g))
    
    #:glossesque-iteration-sequence
    (fn gs g
      (w- base-gs (get-base-gs gs)
      /w- bin-gs (get-bin-gs gs)
      /for*/stream
        (
          [ (k bin)
            (in-sequences
              (glossesque-sys-glossesque-iteration-sequence
                base-gs g))]
          [ (k v)
            (in-sequences
              (glossesque-sys-glossesque-iteration-sequence bin-gs
                (just bin)))])
        (values k v)))
    
    ))

(define-imitation-simple-struct
  (equal-always-atom-glossesque-sys?
    equal-always-atom-glossesque-sys-get-summary-sys)
  equal-always-atom-glossesque-sys
  'equal-always-atom-glossesque-sys (current-inspector) (auto-write)
  (#:prop prop:glossesque-sys /make-glossesque-sys-impl-for-hash
    (dissectfn (equal-always-atom-glossesque-sys gss) gss)
    (fn /hashalw)))
(ascribe-own-contract equal-always-atom-glossesque-sys
  (-> glossesque-summary-sys? glossesque-sys?))

(define/own-contract
  (equality-check-indistinct-atom-glossesque-sys gss ==?)
  (-> glossesque-summary-sys? (-> any/c any/c boolean?)
    glossesque-sys?)
  (equality-check-knowable-atom-glossesque-sys gss /fn a b
    (falsable->uninformative-knowable /==? a b)))

(define/own-contract (indistinct-glossesque-sys original-gs)
  (-> glossesque-sys? glossesque-sys?)
  (w- gss (glossesque-sys-get-summary-sys original-gs)
  /equality-check-knowable-atom-glossesque-sys gss /fn a b
    (w- g (glossesque-sys-glossesque-union-of-zero original-gs)
    /knowable-bind
      (glossesque-sys-glossesque-set-maybe-knowable original-gs g a
        (just /glossesque-summary-sys-trivial-value gss a))
    /fn g
    /knowable-bind
      (glossesque-sys-glossesque-ref-entry-maybe-knowable
        original-gs g b)
    /fn entry-m
    /knowable-if (just? entry-m) /fn #t)))

(define/own-contract (equal-always-indistinct-atom-glossesque-sys gss)
  (-> glossesque-summary-sys? glossesque-sys?)
  (equality-check-indistinct-atom-glossesque-sys gss /fn a b
    (equal-always? a b)))

(define-imitation-simple-struct
  (chaperone=-atom-glossesque-sys?
    chaperone=-atom-glossesque-sys-get-summary-sys)
  chaperone=-atom-glossesque-sys
  'chaperone=-atom-glossesque-sys (current-inspector) (auto-write)
  (#:prop prop:glossesque-sys
    (make-glossesque-sys-impl-for-chaperone=-atom
      (dissectfn (chaperone=-atom-glossesque-sys gss)
        gss)
      (fn gss /equal-always-atom-glossesque-sys gss)
      (fn gss gs
        (equality-check-indistinct-atom-glossesque-sys gss /fn a b
          (atom-chaperone=? a b))))))
(ascribe-own-contract chaperone=-atom-glossesque-sys
  (-> glossesque-summary-sys? glossesque-sys?))

(define-imitation-simple-struct
  (chaperone=-indistinct-atom-glossesque-sys?
    chaperone=-indistinct-atom-glossesque-sys-get-summary-sys)
  chaperone=-indistinct-atom-glossesque-sys
  'chaperone=-indistinct-atom-glossesque-sys (current-inspector)
  (auto-write)
  (#:prop prop:glossesque-sys
    (make-glossesque-sys-impl-for-chaperone=-atom
      (dissectfn (chaperone=-indistinct-atom-glossesque-sys gss)
        gss)
      (fn gss /equal-always-indistinct-atom-glossesque-sys gss)
      (fn gss gs
        (equality-check-indistinct-atom-glossesque-sys gss /fn a b
          (atom-chaperone=? a b))))))
(ascribe-own-contract chaperone=-indistinct-atom-glossesque-sys
  (-> glossesque-summary-sys? glossesque-sys?))

(define-imitation-simple-struct
  (eq-atom-glossesque-sys? eq-atom-glossesque-sys-get-summary-sys)
  eq-atom-glossesque-sys
  'eq-atom-glossesque-sys (current-inspector) (auto-write)
  (#:prop prop:glossesque-sys /make-glossesque-sys-impl-for-hash
    (dissectfn (eq-atom-glossesque-sys gss) gss)
    (fn /hashalw)))
(ascribe-own-contract eq-atom-glossesque-sys
  (-> glossesque-summary-sys? glossesque-sys?))

(define/own-contract (eq-indistinct-atom-glossesque-sys gss)
  (-> glossesque-summary-sys? glossesque-sys?)
  (equality-check-indistinct-atom-glossesque-sys gss /fn a b
    (eq? a b)))

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

(define (list-injection-trie-summarize gss trie)
  ; TODO FORWARD: This use of `get-cons-tries-gs` is a forward
  ; reference. See if we can untangle it.
  (w- cons-tries-gs (get-cons-tries-gs gss)
  /dissect trie (list nil-m cons-tries)
  /w- cons-summary
    (glossesque-sys-glossesque-summarize cons-tries-gs cons-tries)
  /expect nil-m (just kv)
    cons-summary
  /dissect kv (list k v)
  /glossesque-summary-sys-summary-plus gss
    (glossesque-summary-sys-summarize-one-entry gss k v)
    cons-summary))

(define (get-cons-tries-gs gss)
  (ladder-glossesque-sys /forwarding-glossesque-summary-sys gss
    #:summarize-one-entry
    (fn elem trie
      (list-injection-trie-summarize gss trie))))

(define (list-injection-trie-iteration-sequence gss trie)
  (w- cons-tries-gs (get-cons-tries-gs gss)
  /dissect trie (list nil-m cons-tries)
  /in-sequences
    (expect nil-m (just kv) (list)
      (dissect kv (list k v)
      /in-parallel (in-value k) (in-value v)))
    (for*/stream
      (
        [ (elem trie)
          (in-sequences
            (glossesque-sys-glossesque-iteration-sequence
              cons-tries-gs cons-tries))]
        [ (k v)
          (in-sequences
            (list-injection-trie-iteration-sequence gss trie))])
      (values k v))))

(define (list-injection-trie-union-of-zero gss)
  (w- cons-tries-gs (get-cons-tries-gs gss)
  /list
    (nothing)
    (glossesque-sys-glossesque-union-of-zero cons-tries-gs)))

(define (list-injection-trie-empty? gss trie)
  (dissect trie (list nil-m cons-tries)
  /and (nothing? nil-m)
  /w- cons-tries-gs (get-cons-tries-gs gss)
  /glossesque-sys-glossesque-empty? cons-tries-gs cons-tries))

(define
  (rider-and-list-injection-trie-update-maybe-knowable
    gss rider-and-t current-k-as-list overall-k
    on-rider-and-m-knowable)
  (w- cons-tries-gs (get-cons-tries-gs gss)
  /dissect rider-and-t (list rider /list nil-m cons-tries)
  /expect current-k-as-list (cons elem current-k-as-list)
    (w- overall-k
      (expect nil-m (just old-kv) overall-k
      /dissect old-kv (list old-k old-v)
        old-k)
    /knowable-map
      (on-rider-and-m-knowable /list rider
        (maybe-map nil-m /dissectfn (list k v) v))
    /dissectfn (list rider m)
      (list rider
        (list (maybe-map m /fn v /list overall-k v) cons-tries)))
  /knowable-map
    (glossesque-sys-rider-and-glossesque-update-maybe-knowable
      cons-tries-gs
      (list rider cons-tries)
      elem
      (dissectfn (list rider trie-m)
        (w- trie
          (mat trie-m (just trie) trie
          /list-injection-trie-union-of-zero gss)
        /knowable-map
          (rider-and-list-injection-trie-update-maybe-knowable
            gss
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
    get-gss get-gs-for-shallow get-->->list-is-constant? get-->->list)
  (->
    (-> glossesque-sys? glossesque-summary-sys?)
    (-> glossesque-summary-sys? glossesque-sys?)
    (-> glossesque-sys? boolean?)
    (-> glossesque-sys? (-> any/c (-> any/c list?)))
    glossesque-sys-impl?)
  (w- get-base-gs
    (fn gss
      (get-gs-for-shallow
        (forwarding-glossesque-summary-sys gss
          #:summarize-one-entry
          (fn k ->list-and-trie
            (dissect ->list-and-trie (list ->list trie)
            /list-injection-trie-summarize gss trie)))))
  /make-glossesque-sys-impl
    
    #:get-summary-sys (fn gs /get-gss gs)
    
    #:glossesque-union-of-zero
    (fn gs
      (w- gss (get-gss gs)
      /w- base-gs (get-base-gs gss)
      /glossesque-sys-glossesque-union-of-zero base-gs))
    
    #:glossesque-skv-union-of-two-knowable
    (fn gs state a b a-keeping? b-keeping? on-keep skv-union-knowable
      (w- gss (get-gss gs)
      /w- base-gs (get-base-gs gss)
      /w- cons-tries-gs (get-cons-tries-gs gss)
      /glossesque-sys-glossesque-skv-union-of-two-knowable
        base-gs state a b a-keeping? b-keeping? on-keep
        (fn state k a-->list-and-trie b-->list-and-trie
          (dissect a-->list-and-trie (list a-->list a-trie)
          /dissect b-->list-and-trie (list b-->list b-trie)
          /w- ->->list-is-constant? (get-->->list-is-constant? gs)
          ; NOTE OPTIMIZATION: For things that have indeterminate
          ; encodings as ordered lists (namely, `hash?` and `gloss?`
          ; values), where `->->list-is-constant?` is `#f`, we have to
          ; use a more exhaustive method of inserting each value of
          ; one trie into the other, relistifying each key we insert
          ; in terms of the other trie's listifier. Technically, the
          ; asymptotic time complexity of both this and the usual
          ; merging are probably about O(n), but this "more exhaustive
          ; method" makes O(n) invocations to `->list`, while the
          ; usual method makes none.
          /knowable-bind
            (expect ->->list-is-constant? #t
              ; This is the brute force route we use for data
              ; structures of indeterminate ordering.
              (knowable-bind
                ; We start out by having the `result` be the `a-trie`.
                ; We loop through its entries to process them by
                ; removing them or merging them with entries from
                ; `b-trie` as appropriate. We'll process the remaining
                ; entries of `b-trie` later on.
                (w-loop process-a-trie-knowable
                  state-and-b (list state b-trie)
                  result a-trie
                  
                  (dissect result (list nil-m cons-tries)
                  ; We process the `nil-m`.
                  /knowable-bind
                    (expect nil-m (just kv)
                      (known /list state-and-b result)
                    /dissect kv (list k a-v)
                    /dissect state-and-b (list state b-trie)
                    ; Here, we look up and remove an entry from
                    ; `b-trie` that coincides with the entry we're
                    ; processing from `a-trie`.
                    /knowable-bind
                      (rider-and-list-injection-trie-update-maybe-knowable
                        gss
                        (list (trivial) b-trie)
                        (b-->list k)
                        k
                        (dissectfn (list (trivial) b-v-m)
                          (known /list b-v-m /nothing)))
                    /dissectfn (list b-v-m b-trie)
                    ; Here, we remove, keep, or merge the entry in
                    ; `result` we're processing that came from
                    ; `a-trie`.
                    /rider-and-list-injection-trie-update-maybe-knowable
                      gss
                      (list state result)
                      (list)
                      k
                      (dissectfn (list state a-v-m)
                        (expect b-v-m (just b-v)
                          (expect a-keeping? #t
                            (known /list state /nothing)
                          /w- state
                            (on-keep state
                              (glossesque-summary-sys-summarize-one-entry
                                gss k a-v))
                          /known /list state /just a-v)
                        /skv-union-knowable state k a-v b-v)))
                  /dissectfn (list state /list nil-m cons-tries)
                  ; We process the `cons-tries`, which involves a lot
                  ; of looping as we process multiple entries of the
                  ; `cons-tries` ladder and recursively processing the
                  ; trie we get from each entry.
                  /w-loop process-a-trie-entries-knowable
                    state-and-b (list state b-trie)
                    
                    trie-entries
                    (in-values-sequence
                      (glossesque-sys-glossesque-iteration-sequence
                        cons-tries-gs cons-tries))
                    
                    result result
                    
                    (expect trie-entries
                      (sequence* trie-entry trie-entries)
                      (known /list state-and-b result)
                    /dissect trie-entry (list elem trie)
                    /knowable-bind
                      (process-a-trie-knowable state-and-b trie)
                    /dissectfn (list state-and-b trie)
                    /process-a-trie-entries-knowable
                      state-and-b trie-entries trie)))
              /dissectfn (list (list state b-trie) result)
              /expect b-keeping? #t
                (known /list state result)
              /w- state
                (on-keep state
                  (list-injection-trie-summarize gss b-trie))
              /w-loop next
                state state
                result result
                
                b
                (in-values-sequence
                  (list-injection-trie-iteration-sequence gss b-trie))
                
                (expect b (sequence* b-kv b)
                  (known /list state result)
                /dissect b-kv (list k b-v)
                /knowable-bind
                  (rider-and-list-injection-trie-update-maybe-knowable
                    gss
                    (list state result)
                    (a-->list k)
                    k
                    (dissectfn (list state /nothing)
                      (known /list state /just b-v)))
                /dissectfn (list state result)
                /next state result b))
            ; This is the divide-and-conquer route we use for data
            ; structures of determinate ordering.
            /w-loop next state state a-trie a-trie b-trie b-trie
              (dissect a-trie (list a-nil-m a-cons-tries)
              /dissect b-trie (list b-nil-m b-cons-tries)
              ; We process the `a-nil-m` and `b-nil-m`.
              /knowable-bind
                (entry-maybe-skv-union-of-two-knowable
                  state a-nil-m b-nil-m a-keeping? b-keeping? on-keep
                  (fn state a-k a-v b-v
                    (skv-union-knowable state a-k a-v b-v)))
              /dissectfn (list state nil-m)
              ; We process the `a-cons-tries` and `b-cons-tries`.
              /knowable-bind
                (glossesque-sys-glossesque-skv-union-of-two-knowable
                  cons-tries-gs state a-cons-tries b-cons-tries
                  a-keeping? b-keeping? on-keep
                  (fn state elem a-trie b-trie
                    (knowable-map (next state a-trie b-trie)
                    /dissectfn (list state trie)
                      (list state
                        (maybe-if
                          (not /list-injection-trie-empty? gss trie)
                        /fn
                          trie)))))
              /dissectfn (list state cons-tries)
              /known /list state /just /list nil-m cons-tries))
          /dissectfn (list state trie)
          /known /list state /just /list a-->list trie))))
    
    #:glossesque-ref-entry-maybe-knowable
    (fn gs g k
      (w- gss (get-gss gs)
      /w- base-gs (get-base-gs gss)
      /w- cons-tries-gs (get-cons-tries-gs gss)
      /knowable-bind
        (glossesque-sys-glossesque-ref-entry-maybe-knowable base-gs g
          (shallow-wrapper k))
      /fn ->list-and-trie-entry-maybe
      /expect ->list-and-trie-entry-maybe (just ->list-and-trie-entry)
        (known /nothing)
      /dissect ->list-and-trie-entry (list _ /list ->list trie)
      /w-loop next trie trie k (->list k)
        (dissect trie (list nil-m cons-tries)
        /expect k (cons elem k)
          (known nil-m)
        /knowable-bind
          (glossesque-sys-glossesque-ref-entry-maybe-knowable
            cons-tries-gs cons-tries elem)
        /fn trie-entry-m
        /expect trie-entry-m (just trie-entry) (known /nothing)
        /dissect trie-entry (list _ trie)
        /next trie k)))
    
    #:rider-and-glossesque-update-maybe-knowable
    (fn gs rider-and-g k on-rider-and-m-knowable
      (w- gss (get-gss gs)
      /w- base-gs (get-base-gs gss)
      /dissect rider-and-g (list rider g)
      /glossesque-sys-rider-and-glossesque-update-maybe-knowable
        base-gs (list rider g) (shallow-wrapper k)
        (dissectfn (list rider ->list-and-trie-m)
          (w- ->->list (get-->->list gs)
          /dissect
            (mat ->list-and-trie-m (just ->list-and-trie)
              ->list-and-trie
              (list
                (->->list k)
                (list-injection-trie-union-of-zero gss)))
            (list ->list trie)
          /knowable-bind
            (rider-and-list-injection-trie-update-maybe-knowable
              gss
              (list rider trie)
              (->list k)
              k
              (fn rider-and-m
                (on-rider-and-m-knowable rider-and-m)))
          /dissectfn (list rider trie)
          /known /list rider
            (maybe-if (not /list-injection-trie-empty? gss trie) /fn
              (list ->list trie))))))
    
    #:glossesque-summarize
    (fn gs g
      (w- gss (get-gss gs)
      /w- base-gs (get-base-gs gss)
      /glossesque-sys-glossesque-summarize base-gs g))
    
    #:glossesque-iteration-sequence
    (fn gs g
      (w- gss (get-gss gs)
      /w- base-gs (get-base-gs gss)
      /for*/stream
        (
          [ (tag ->list-and-trie)
            (in-sequences
              (glossesque-sys-glossesque-iteration-sequence
                base-gs g))]
          [ (k v)
            (in-sequences
              (dissect ->list-and-trie (list ->list trie)
              /list-injection-trie-iteration-sequence gss trie))])
        (values k v)))
    
    ))

(define-imitation-simple-struct
  (equal-always-from-list-injection-glossesque-sys?
    equal-always-from-list-injection-glossesque-sys-get-summary-sys
    equal-always-from-list-injection-glossesque-sys-->->list-is-constant?
    equal-always-from-list-injection-glossesque-sys-->->list)
  equal-always-from-list-injection-glossesque-sys-unguarded
  'equal-always-from-list-injection-glossesque-sys (current-inspector)
  (auto-write)
  (#:prop prop:glossesque-sys
    (make-glossesque-sys-impl-from-list-injection
      (dissectfn
        (equal-always-from-list-injection-glossesque-sys-unguarded
          gss ->->list-is-constant? ->->list)
        gss)
      (fn gss /equal-always-atom-glossesque-sys gss)
      (dissectfn
        (equal-always-from-list-injection-glossesque-sys-unguarded
          gss ->->list-is-constant? ->->list)
        ->->list-is-constant?)
      (dissectfn
        (equal-always-from-list-injection-glossesque-sys-unguarded
          gss ->->list-is-constant? ->->list)
        ->->list))))

(define/own-contract
  (equal-always-from-list-injection-glossesque-sys
    #:glossesque-summary-sys gss
    #:->list [->list #f]
    
    #:->->list
    [ ->->list
      (if ->list
        (dissectfn _ ->list)
        (raise-arguments-error 'equal-always-from-list-injection-glossesque-sys
          "expected either #:->list or #:->->list to be provided"))]
    
    )
  (->*
    (#:glossesque-summary-sys glossesque-summary-sys?)
    (
      #:->list (or/c #f (-> any/c list?))
      #:->->list (-> any/c (-> any/c list?)))
    glossesque-sys?)
  (equal-always-from-list-injection-glossesque-sys-unguarded
    gss (not /not ->list) ->->list))

(define-imitation-simple-struct
  (equal-always-indistinct-from-list-injection-glossesque-sys?
    equal-always-indistinct-from-list-injection-glossesque-sys-get-summary-sys
    equal-always-indistinct-from-list-injection-glossesque-sys-->->list-is-constant?
    equal-always-indistinct-from-list-injection-glossesque-sys-->->list)
  equal-always-indistinct-from-list-injection-glossesque-sys-unguarded
  'equal-always-indistinct-from-list-injection-glossesque-sys
  (current-inspector)
  (auto-write)
  (#:prop prop:glossesque-sys
    (make-glossesque-sys-impl-from-list-injection
      (dissectfn
        (equal-always-indistinct-from-list-injection-glossesque-sys-unguarded
          gss ->->list-is-constant? ->->list)
        gss)
      (fn gss /equal-always-indistinct-atom-glossesque-sys gss)
      (dissectfn
        (equal-always-indistinct-from-list-injection-glossesque-sys-unguarded
          gss ->->list-is-constant? ->->list)
        ->->list-is-constant?)
      (dissectfn
        (equal-always-indistinct-from-list-injection-glossesque-sys-unguarded
          gss ->->list-is-constant? ->->list)
        ->->list))))

(define/own-contract
  (equal-always-indistinct-from-list-injection-glossesque-sys
    #:glossesque-summary-sys gss
    #:->list [->list #f]
    
    #:->->list
    [ ->->list
      (if ->list
        (dissectfn _ ->list)
        (raise-arguments-error 'equal-always-indistinct-from-list-injection-glossesque-sys
          "expected either #:->list or #:->->list to be provided"))]
    
    )
  (->*
    (#:glossesque-summary-sys glossesque-summary-sys?)
    (
      #:->list (or/c #f (-> any/c list?))
      #:->->list (-> any/c (-> any/c list?)))
    glossesque-sys?)
  (equal-always-indistinct-from-list-injection-glossesque-sys-unguarded
    gss (not /not ->list) ->->list))

; TODO: See if we should export this.
(define/own-contract (shallowly-unchaperoned? copy v)
  (-> (-> any/c any/c) any/c boolean?)
  (chaperone-of? (copy v) v))

(define/own-contract
  (make-glossesque-sys-impl-for-chaperone=-copiable
    get-summary-sys
    get-gs-for-equal-always
    get-gs-for-wrapped-chaperone=-assuming-equal-always
    get-copy)
  (->
    (-> glossesque-sys? glossesque-summary-sys?)
    (-> glossesque-summary-sys? glossesque-sys?)
    (-> glossesque-summary-sys? (-> any/c any/c boolean?)
      glossesque-sys?)
    (-> glossesque-sys? (-> any/c any/c))
    glossesque-sys-impl?)
  (make-glossesque-sys-impl-for-chaperone=-atom
    (fn gs /get-summary-sys gs)
    (fn gss /get-gs-for-equal-always gss)
    (fn gss gs
      (w- copy (get-copy gs)
      /glossesque-sys-map-key #:name 'referencing-only-chaperones
        #:granted-key
        (fn k
          (maybe-if (not /shallowly-unchaperoned? copy k) /fn k))
        
        gss
        (fn gss
          (get-gs-for-wrapped-chaperone=-assuming-equal-always gss
            (fn a b
              (match (list a b)
                [(list (list (nothing) _) (list (nothing) _)) #t]
                [ (list (list (just a-value) _) (list (just b-value) _))
                  (atom-chaperone=? a-value b-value)]
                [_ #f]))))))))

(define-imitation-simple-struct
  (chaperone=-copiable-glossesque-sys?
    chaperone=-copiable-glossesque-sys-get-summary-sys
    chaperone=-copiable-glossesque-sys-get-copy)
  chaperone=-copiable-glossesque-sys-unguarded
  'chaperone=-copiable-glossesque-sys (current-inspector) (auto-write)
  (#:prop prop:glossesque-sys
    (make-glossesque-sys-impl-for-chaperone=-copiable
      (dissectfn
        (chaperone=-copiable-glossesque-sys-unguarded gss copy)
        gss)
      (fn gss /equal-always-atom-glossesque-sys gss)
      (fn gss wrapped-key=?
        (equality-check-indistinct-atom-glossesque-sys gss /fn a b
          (wrapped-key=? a b)))
      (dissectfn
        (chaperone=-copiable-glossesque-sys-unguarded gss copy)
        copy))))

(define/own-contract
  (chaperone=-copiable-glossesque-sys
    #:glossesque-summary-sys gss
    #:copy copy)
  (->
    #:glossesque-summary-sys glossesque-summary-sys?
    #:copy (-> any/c (-> any/c list?))
    glossesque-sys?)
  (chaperone=-copiable-glossesque-sys-unguarded gss copy))

(define-imitation-simple-struct
  (chaperone=-indistinct-copiable-glossesque-sys?
    chaperone=-indistinct-copiable-glossesque-sys-get-summary-sys
    chaperone=-indistinct-copiable-glossesque-sys-get-copy)
  chaperone=-indistinct-copiable-glossesque-sys-unguarded
  'chaperone=-indistinct-copiable-glossesque-sys
  (current-inspector)
  (auto-write)
  (#:prop prop:glossesque-sys
    (make-glossesque-sys-impl-for-chaperone=-copiable
      (dissectfn
        (chaperone=-indistinct-copiable-glossesque-sys-unguarded
          gss copy)
        gss)
      (fn gss /equal-always-indistinct-atom-glossesque-sys gss)
      (fn gss wrapped-key=?
        (equality-check-indistinct-atom-glossesque-sys gss /fn a b
          (wrapped-key=? a b)))
      (dissectfn
        (chaperone=-indistinct-copiable-glossesque-sys-unguarded
          gss copy)
        copy))))

(define/own-contract
  (chaperone=-indistinct-copiable-glossesque-sys
    #:glossesque-summary-sys gss
    #:copy copy)
  (->
    #:glossesque-summary-sys glossesque-summary-sys?
    #:copy (-> any/c (-> any/c list?))
    glossesque-sys?)
  (chaperone=-indistinct-copiable-glossesque-sys-unguarded gss copy))

(define/own-contract (normalized-glossesque-sys gss granted-key)
  (-> glossesque-summary-sys? (-> any/c any/c) glossesque-sys?)
  (glossesque-sys-map-key #:name 'normalized
    gss (fn gss /equal-always-atom-glossesque-sys gss)
    #:granted-key (fn k /granted-key k)))

(define/own-contract (terminal-glossesque-sys gss)
  (-> glossesque-summary-sys? glossesque-sys?)
  (normalized-glossesque-sys gss /fn k #f))

(define/own-contract
  (identifiable-object-tagged-glossesque-sys
    cgkr-get-tgs-k guard-wrapper)
  (->
    (-> (endless-sequence/c custom-gloss-key-report?)
      (knowable/c tagged-glossesque-sys?))
    (-> any/c any/c)
    tagged-glossesque-sys?)
  (make-tagged-glossesque-sys
    #:equal-always-free-vars (cgkr-get-tgs-k guard-wrapper)
    (fn v
      (knowable-bind
        (cgkr-get-tgs-k
          (dynamic-type-get-custom-gloss-key-reports
            (any-dynamic-type)
            v))
      /fn tgs
      /knowable-map
        (tagged-glossesque-sys-inhabitant-get-identifiable-object-guard-wrapper-maybe-knowable
          tgs v)
      /fn guard-wrapper-m
        (just? guard-wrapper-m)))
    (fn gss
      (glossesque-sys-map-key #:name 'identifiable-object-guard
        gss (fn gss /chaperone=-atom-glossesque-sys gss)
        #:granted-key
        (fn k
          (dissect
            (knowable-bind
              (cgkr-get-tgs-k
                (dynamic-type-get-custom-gloss-key-reports
                  (any-dynamic-type)
                  k))
            /fn tgs
            /tagged-glossesque-sys-inhabitant-get-identifiable-object-guard-wrapper-maybe-knowable
              tgs k)
            (known /just guard-wrapper)
          /guard-wrapper k))))
    
    #:inhabitant-get-identifiable-object-guard-wrapper-maybe-knowable
    (fn inhabitant
      (known /just /fn inhabitant /guard-wrapper inhabitant))))

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
; which passes the given `inhabitant?-knowable` predicate) and return
; a function that takes an inhabitant of similar structure and returns
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
;     `inhabitant?-knowable` predicate, which may be an instance of
;     `prop:expressly-knowable-predicate?`, or if neither of them has
;     a known true result for it, then unknown.
;     
;     Otherwise, if at least one operand has a known true result for
;     `inhabitant?-knowable` and at least one has a known false result
;     for it, then a known nothing.
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
;     under the same smoosh include an unknown and/or a known nothing,
;     then a result determined by the
;     `maybe-min-yknow-zip*-map/custom` argument. By default, this
;     results in unknown.
;     
;     Otherwise, if those recursive results are `eq?` to the elements
;     of an operand, then the first such operand.
;     
;     Otherwise, a new inhabitant (created by the given
;     `example-and-list->` function using the first operand as the
;     example) whose elements are those recursive results.
;   <=, >=:
;     If the operands do not both have known results for the given
;     `inhabitant?-knowable` predicate, which may be an instance of
;     `prop:expressly-knowable-predicate?`, or if neither of them has
;     a known true result for it, then unknown.
;     
;     Otherwise, if at least one operand has a known true result for
;     `inhabitant?-knowable` and at least one has a known false result
;     for it, then a known `#f`.
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
;     under the same smoosh include an unknown and/or a known nothing,
;     then a result determined by the
;     `maybe-min-yknow-zip*-map/custom` argument. By default, this
;     results in unknown.
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
    #:inhabitant?-knowable inhabitant?-knowable
    #:->list [->list #f]
    
    #:->->list
    [ ->->list
      (if ->list
        (dissectfn _ ->list)
        (raise-arguments-error 'make-expressly-smooshable-dynamic-type-impl-from-equal-always-list-isomorphism
          "expected either #:->list or #:->->list to be provided"))]
    
    #:example-and-list-> example-and-list->
    
    #:maybe-min-yknow-zip*-map/custom
    [ maybe-min-yknow-zip*-map/custom
      (fn my-list on-value
        (maybe-min-yknow-zip*-map/indistinct my-list /fn value-list
          (on-value value-list)))]
    
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
      #:inhabitant?-knowable (-> any/c boolean?)
      #:example-and-list-> (-> any/c list? any/c))
    (
      #:known-distinct? boolean?
      #:known-discrete? boolean?
      #:->list (or/c #f (-> any/c list?))
      #:->->list (-> any/c (-> any/c list?))
      
      #:maybe-min-yknow-zip*-map/custom
      (-> (listof (yknow/c maybe?)) (-> list? any/c) (yknow/c maybe?))
      
      #:inhabitant-shallowly-equal-always?-knowable
      (-> any/c any/c (knowable/c boolean?))
      
      #:get-smoosh-of-zero-reports
      (-> any/c (endless-sequence/c smoosh-report?))
      
      )
    expressly-smooshable-dynamic-type-impl?)
  (make-expressly-smooshable-dynamic-type-impl
    #:get-smoosh-of-zero-reports get-smoosh-of-zero-reports
    
    #:get-smoosh-of-one-reports
    (fn self a
      (w- any-dt (self-get-any-dynamic-type self)
      /expect (inhabitant?-knowable a) (known #t)
        (uninformative-smoosh-reports)
      /w- ->list (->->list a)
      /w- a-list (->list a)
      /smoosh-reports-zip*-map
        (list-map a-list /fn a-elem
          (dynamic-type-get-smoosh-of-one-reports any-dt a-elem))
        #:on-result-yknow-maybe-yknow
        (fn ymy-list
          (maybe-min-yknow-zip*-map/custom ymy-list /fn y-list
            (yknow-zip*-map y-list /fn result-list
              (if (list-elements-eq? result-list a-list) a
              /example-and-list-> a result-list))))))
    
    #:get-smoosh-and-comparison-of-two-reports
    (fn self a b
      (w- any-dt (self-get-any-dynamic-type self)
      /expect (inhabitant?-knowable a) (known a-inhabits?)
        (uninformative-smoosh-and-comparison-of-two-reports)
      /expect (inhabitant?-knowable b) (known b-inhabits?)
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
        #:on-check-result-yknow
        (fn y-list
          (boolean-and-yknow-zip*/from-maybe-min-yknow-trivial-zip*-map
            maybe-min-yknow-zip*-map/custom y-list))
        #:on-smoosh-result-yknow-maybe-yknow
        (fn ymy-list
          (maybe-min-yknow-zip*-map/custom ymy-list /fn y-list
            (yknow-zip*-map y-list /fn result-list
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
; which passes the given `inhabitant?-knowable` predicate) and return
; a function that takes an inhabitant of similar structure and returns
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
;     `inhabitant?-knowable` predicate, which may be an instance of
;     `prop:expressly-knowable-predicate?`, or if neither of them has
;     a known true result for it, then unknown.
;     
;     Otherwise, if at least one operand has a known true result for
;     `inhabitant?-knowable` and at least one has a known false result
;     for it, then a known nothing.
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
;     under the same smoosh include an unknown and/or a known nothing,
;     then a result determined by the
;     `maybe-min-yknow-zip*-map/custom` argument. By default, this
;     results in unknown.
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
;     `inhabitant?-knowable` predicate, which may be an instance of
;     `prop:expressly-knowable-predicate?`, or if neither of them has
;     a known true result for it, then unknown.
;     
;     Otherwise, if at least one operand has a known true result for
;     `inhabitant?-knowable` and at least one has a known false result
;     for it, then a known `#f`.
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
;     under the same smoosh include an unknown and/or a known nothing,
;     then a result determined by the
;     `maybe-min-yknow-zip*-map/custom` argument. By default, this
;     results in unknown.
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
    #:inhabitant?-knowable inhabitant?-knowable
    #:->list [->list #f]
    
    #:->->list
    [ ->->list
      (if ->list
        (dissectfn _ ->list)
        (raise-arguments-error 'make-expressly-smooshable-dynamic-type-impl-from-chaperone-of-list-isomorphism
          "expected either #:->list or #:->->list to be provided"))]
    
    #:example-and-list-> example-and-list->
    
    #:maybe-min-yknow-zip*-map/custom
    [ maybe-min-yknow-zip*-map/custom
      (fn my-list on-value
        (maybe-min-yknow-zip*-map/indistinct my-list /fn value-list
          (on-value value-list)))]
    
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
      #:inhabitant?-knowable (-> any/c (knowable/c boolean?))
      #:example-and-list-> (-> any/c list? any/c))
    (
      #:known-distinct? boolean?
      #:known-discrete? boolean?
      #:->list (or/c #f (-> any/c list?))
      #:->->list (-> any/c (-> any/c list?))
      
      #:maybe-min-yknow-zip*-map/custom
      (-> (listof (yknow/c maybe?)) (-> list? any/c) (yknow/c maybe?))
      
      #:inhabitant-shallowly-equal-always?-knowable
      (-> any/c any/c (knowable/c boolean?))
      
      #:copy (-> any/c any/c)
      
      #:get-smoosh-of-zero-reports
      (-> any/c (endless-sequence/c smoosh-report?))
      
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
      /expect (inhabitant?-knowable a) (known #t)
        (uninformative-smoosh-reports)
      /w- ->list (->->list a)
      /w- a-list (->list a)
      /dissect
        (smoosh-reports-zip*-map
          (list-map a-list /fn a-elem
            (dynamic-type-get-smoosh-of-one-reports any-dt a-elem))
          #:on-result-yknow-maybe-yknow
          (fn ymy-list
            (maybe-min-yknow-zip*-map/custom ymy-list /fn y-list
              (yknow-zip*-map y-list /fn result-list
                result-list))))
        (sequence* report-0 report-1 report-2+)
      /w- a-shallowly-unchaperoned?-promise
        (delay /inhabitant-shallowly-unchaperoned? a)
      /w- on-smoosh-result-yknow-maybe-yknow
        (fn result-needs-to-be-chaperone-of?
          (fn result-list-ymy
            (yknow-map/knowable result-list-ymy /fn list-ym
              (expect list-ym (just list-y) (known /nothing)
              /knowable-bind (yknow-value-knowable list-y)
              /fn result-list
              /if (list-elements-eq? result-list a-list)
                (known /just /make-yknow-from-value a)
              /w- noncanonical-result
                (example-and-list-> a result-list)
              /if
                ; If we're doing a particularly strict check and the
                ; operand `a` is wrapped with impersonators or
                ; interposing chaperones, we have no `known?` result.
                (or
                  (not result-needs-to-be-chaperone-of?)
                  (force a-shallowly-unchaperoned?-promise))
                (known
                  (just /make-yknow-from-value noncanonical-result))
              /unknown))))
      /sequence*
        (smoosh-report-map report-0
          #:on-smoosh-result-yknow-maybe-yknow
          (on-smoosh-result-yknow-maybe-yknow #f))
        (smoosh-report-map report-1
          #:on-join-yknow-maybe-yknow
          (on-smoosh-result-yknow-maybe-yknow #t)
          #:on-meet-yknow-maybe-yknow
          (on-smoosh-result-yknow-maybe-yknow #t)
          #:on-==-yknow-maybe-yknow
          (on-smoosh-result-yknow-maybe-yknow #t)
          #:on-path-related-yknow-maybe-yknow
          (on-smoosh-result-yknow-maybe-yknow #f))
        (smoosh-reports-map report-2+
          #:on-smoosh-result-yknow-maybe-yknow
          (on-smoosh-result-yknow-maybe-yknow #t))))
    
    #:get-smoosh-and-comparison-of-two-reports
    (fn self a b
      (w- any-dt (self-get-any-dynamic-type self)
      /expect (inhabitant?-knowable a) (known a-inhabits?)
        (uninformative-smoosh-and-comparison-of-two-reports)
      /expect (inhabitant?-knowable b) (known b-inhabits?)
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
          #:on-check-result-yknow
          (fn y-list
            (boolean-and-yknow-zip*/from-maybe-min-yknow-trivial-zip*-map
              maybe-min-yknow-zip*-map/custom y-list))
          #:on-smoosh-result-yknow-maybe-yknow
          (fn ymy-list
            (maybe-min-yknow-zip*-map/custom ymy-list /fn y-list
              (yknow-zip*-map y-list /fn result-list
                result-list))))
        (sequence* report-0 report-1 report-2+)
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
      /w- on-check-result-yknow
        (fn should-a-be-small? should-b-be-small?
          (fn y
            (yknow-map/knowable y /fn result
              (boolean-and-knowable-thunk-zip* /list
                (fn /known result)
                (fn /boolean-or-knowable-thunk-zip* /list
                  (fn /known /not should-a-be-small?)
                  (fn /falsable->uninformative-knowable
                    (inhabitant-shallowly-chaperone-of? b a)))
                (fn /boolean-or-knowable-thunk-zip* /list
                  (fn /known /not should-b-be-small?)
                  (fn /falsable->uninformative-knowable
                    (inhabitant-shallowly-chaperone-of? a b)))))))
      /w- on-smoosh-result-yknow-maybe-yknow
        (fn acceptable-result?
          (fn result-list-ymy
            (yknow-map/knowable result-list-ymy /fn list-ym
              (expect list-ym (just list-y) (known /nothing)
              /knowable-bind (yknow-value-knowable list-y)
              /fn result-list
              /if
                (and
                  (list-elements-eq? result-list a-list)
                  (acceptable-result? a))
                (known /just /make-yknow-from-value a)
              /if
                (and
                  (list-elements-eq? result-list b-list)
                  (acceptable-result? b))
                (known /just /make-yknow-from-value b)
              /w- noncanonical-result
                (example-and-list-> a result-list)
              /if (acceptable-result? noncanonical-result)
                (known
                  (just /make-yknow-from-value noncanonical-result))
              /unknown))))
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
      /sequence*
        (smoosh-and-comparison-of-two-report-map report-0
          #:on-check-result-yknow (on-check-result-yknow #f #f)
          #:on-smoosh-result-yknow-maybe-yknow
          (on-smoosh-result-yknow-maybe-yknow
            path-related-acceptable-result?))
        (smoosh-and-comparison-of-two-report-map report-1
          #:on-<=?-yknow (on-check-result-yknow #t #f)
          #:on->=?-yknow (on-check-result-yknow #f #t)
          #:on-join-yknow-maybe-yknow
          (on-smoosh-result-yknow-maybe-yknow
            (fn v
              (and
                (inhabitant-shallowly-chaperone-of? v a)
                (inhabitant-shallowly-chaperone-of? v b))))
          #:on-meet-yknow-maybe-yknow
          (on-smoosh-result-yknow-maybe-yknow
            (fn v
              (and
                (or (eq? v a) (eq? v b))
                (inhabitant-shallowly-chaperone-of? a v)
                (inhabitant-shallowly-chaperone-of? b v))))
          #:on-==-yknow-maybe-yknow
          (on-smoosh-result-yknow-maybe-yknow ==-acceptable-result?)
          #:on-path-related-yknow-maybe-yknow
          (on-smoosh-result-yknow-maybe-yknow
            path-related-acceptable-result?))
        (smoosh-and-comparison-of-two-reports-map report-2+
          #:on-check-result-yknow (on-check-result-yknow #t #t)
          #:on-smoosh-result-yknow-maybe-yknow
          (on-smoosh-result-yknow-maybe-yknow
            ==-acceptable-result?))))
    
    ))

(define/own-contract
  (make-expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type-impl-from-list-injection
    #:self-get-any-dynamic-type self-get-any-dynamic-type
    #:inhabitant?-knowable inhabitant?-knowable
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
      #:inhabitant?-knowable (-> any/c (knowable/c boolean?)))
    (
      #:->list (or/c #f (-> any/c list?))
      #:->->list (-> any/c (-> any/c list?))
      #:combine-element-hash-codes (-> (listof fixnum?) fixnum?))
    expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type-impl?)
  (make-expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type-impl
    
    #:get-smoosh-equal-hash-code-support-reports
    (fn self a
      (constant-smoosh-equal-hash-code-support-reports /delay
        (expect (inhabitant?-knowable a) #t (uninformative-hash-code)
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
                (equal-always-hash-code inhabitant?-knowable)
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
    #:inhabitant?-knowable inhabitant?-knowable
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
      #:inhabitant?-knowable (-> any/c (knowable/c boolean?))
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
      (expect (inhabitant?-knowable a) (known #t)
        (uninformative-custom-gloss-key-reports)
      /w- falsable-map
        (fn f on-value
          (and f /on-value f))
      /w- function-map-result
        (fn f on-result
          (fn v
            (on-result (f v))))
      /w- wrap
        (fn info-level path-related-level v
          (w-loop next info-level info-level v v
            (expect (nat->maybe info-level) (just info-level)
              (mat path-related-level 1 (path-related-wrapper v)
                v)
            /next info-level (info-wrapper v))))
      /w- wrap-->list
        (fn info-level path-related-level ->list
          (function-map-result ->list /fn listified
            (list-map listified /fn elem
              (wrap info-level path-related-level elem))))
      /w- wrap-falsable-->list
        (fn info-level path-related-level falsable-->list
          (falsable-map ->list /fn ->list
            (wrap-->list info-level path-related-level ->list)))
      /w- wrap-->->list
        (fn info-level path-related-level ->->list
          (function-map-result ->->list /fn ->list
            (wrap-->list info-level path-related-level ->list)))
      /w- equal-always-indistinct-tgs-k
        (fn info-level path-related-level
          (w- ->list
            (wrap-falsable-->list info-level path-related-level
              ->list)
          /w- ->->list
            (wrap-->->list info-level path-related-level ->->list)
          /known /make-tagged-glossesque-sys
            
            #:equal-always-free-vars
            (inhabitant?-knowable ->list ->->list)
            
            inhabitant?-knowable
            (fn gss
              (equal-always-indistinct-from-list-injection-glossesque-sys
                #:glossesque-summary-sys gss
                #:->list ->list
                #:->->list ->->list))))
      /w- equal-always-distinct-tgs-k
        (fn info-level path-related-level
          (w- ->list
            (wrap-falsable-->list info-level path-related-level ->list)
          /w- ->->list
            (wrap-->->list info-level path-related-level ->->list)
          /known /make-tagged-glossesque-sys
            
            #:equal-always-free-vars
            (inhabitant?-knowable ->list ->->list)
            
            inhabitant?-knowable
            (fn gss
              (equal-always-from-list-injection-glossesque-sys
                #:glossesque-summary-sys gss
                #:->list ->list
                #:->->list ->->list))))
      /w- equal-always-tgs-k
        (if known-distinct?
          equal-always-distinct-tgs-k
          equal-always-indistinct-tgs-k)
      /w- chaperone=-indistinct-tgs-k
        (known /make-tagged-glossesque-sys
          #:equal-always-free-vars (inhabitant?-knowable copy)
          inhabitant?-knowable
          (fn gss
            (chaperone=-indistinct-copiable-glossesque-sys
              #:glossesque-summary-sys gss #:copy copy)))
      /w- chaperone=-distinct-tgs-k
        (known /make-tagged-glossesque-sys
          #:equal-always-free-vars (inhabitant?-knowable copy)
          inhabitant?-knowable
          (fn gss
            (chaperone=-copiable-glossesque-sys
              #:glossesque-summary-sys gss #:copy copy)))
      /w- chaperone=-tgs-k
        (if known-distinct?
          chaperone=-distinct-tgs-k
          chaperone=-indistinct-tgs-k)
      /w- possibly-chaperone=-tgs-k
        (if ignore-chaperones?
          equal-always-tgs-k
          (fn info-level path-related-level
            chaperone=-tgs-k))
      /sequence*
        (custom-gloss-key-report-zip*-map (list)
          #:on-path-related-tagged-glossesque-sys-knowable
          (dissectfn (list)
            (if (and known-distinct? known-discrete?)
              (equal-always-distinct-tgs-k 0 1)
              (equal-always-indistinct-tgs-k 0 1)))
          #:on-==-tagged-glossesque-sys-knowable
          (dissectfn (list)
            (equal-always-tgs-k 0 0)))
        (custom-gloss-key-report-zip*-map (list)
          #:on-path-related-tagged-glossesque-sys-knowable
          (dissectfn (list)
            (equal-always-tgs-k 1 1))
          #:on-==-tagged-glossesque-sys-knowable
          (dissectfn (list)
            (possibly-chaperone=-tgs-k 1 0)))
        (sequence-map
          (fn i
            (w- info-level (+ 2 i)
            /custom-gloss-key-report-zip*-map (list)
              #:on-path-related-tagged-glossesque-sys-knowable
              (dissectfn (list)
                (possibly-chaperone=-tgs-k info-level 1))
              #:on-==-tagged-glossesque-sys-knowable
              (dissectfn (list)
                (possibly-chaperone=-tgs-k info-level 0))))
          (in-naturals))))
    
    ))

(define/own-contract
  (make-expressly-smooshable-bundle-property-from-list-isomorphism
    #:ignore-chaperones? [ignore-chaperones? #f]
    #:known-distinct? [known-distinct? #t]
    #:known-discrete? [known-discrete? #f]
    #:self-get-any-dynamic-type self-get-any-dynamic-type
    #:inhabitant?-knowable inhabitant?-knowable
    #:->list [->list #f]
    
    #:->->list
    [ ->->list
      (if ->list
        (dissectfn _ ->list)
        (raise-arguments-error 'make-expressly-smooshable-bundle-property-from-list-isomorphism
          "expected either #:->list or #:->->list to be provided"))]
    
    #:example-and-list-> example-and-list->
    
    #:maybe-min-yknow-zip*-map/custom
    [ maybe-min-yknow-zip*-map/custom
      (fn my-list on-value
        (maybe-min-yknow-zip*-map/indistinct my-list /fn value-list
          (on-value value-list)))]
    
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
      #:inhabitant?-knowable (-> any/c (knowable/c boolean?))
      #:example-and-list-> (-> any/c list? any/c))
    (
      #:ignore-chaperones? boolean?
      #:known-distinct? boolean?
      #:known-discrete? boolean?
      #:->list (or/c #f (-> any/c list?))
      #:->->list (-> any/c (-> any/c list?))
      
      #:maybe-min-yknow-zip*-map/custom
      (-> (listof (yknow/c maybe?)) (-> list? any/c) (yknow/c maybe?))
      
      #:combine-element-hash-codes (-> (listof fixnum?) fixnum?)
      
      #:inhabitant-shallowly-equal-always?-knowable
      (-> any/c any/c (knowable/c boolean?))
      
      #:copy (-> any/c any/c)
      
      #:get-smoosh-of-zero-reports
      (-> any/c (endless-sequence/c smoosh-report?))
      
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
                #:inhabitant?-knowable inhabitant?-knowable
                #:->list ->list
                #:->->list ->->list
                #:example-and-list-> example-and-list->
                
                #:maybe-min-yknow-zip*-map/custom
                maybe-min-yknow-zip*-map/custom
                
                #:inhabitant-shallowly-equal-always?-knowable
                inhabitant-shallowly-equal-always?-knowable
                
                #:get-smoosh-of-zero-reports
                get-smoosh-of-zero-reports)
              (make-expressly-smooshable-dynamic-type-impl-from-chaperone-of-list-isomorphism
                #:known-distinct? known-distinct?
                #:known-discrete? known-discrete?
                #:self-get-any-dynamic-type self-get-any-dynamic-type
                #:inhabitant?-knowable inhabitant?-knowable
                #:->list ->list
                #:->->list ->->list
                #:example-and-list-> example-and-list->
                
                #:maybe-min-yknow-zip*-map/custom
                maybe-min-yknow-zip*-map/custom
                
                #:inhabitant-shallowly-equal-always?-knowable
                inhabitant-shallowly-equal-always?-knowable
                
                #:copy copy
                
                #:get-smoosh-of-zero-reports
                get-smoosh-of-zero-reports))))
        (cons
          prop:expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type
          (dissectfn (trivial)
            (make-expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type-impl-from-list-injection
              #:self-get-any-dynamic-type self-get-any-dynamic-type
              #:inhabitant?-knowable inhabitant?-knowable
              #:->list ->list
              #:->->list ->->list
              
              #:combine-element-hash-codes
              combine-element-hash-codes)))
        (cons
          prop:expressly-custom-gloss-key-dynamic-type
          (dissectfn (trivial)
            (make-expressly-custom-gloss-key-dynamic-type-impl-from-list-injection
              #:ignore-chaperones? ignore-chaperones?
              #:known-distinct? known-distinct?
              #:known-discrete? known-discrete?
              #:inhabitant?-knowable inhabitant?-knowable
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
;     `inhabitant?-knowable` predicate, which may be an instance of
;     `prop:expressly-knowable-predicate?`, or if neither of them has
;     a known true result for it, then unknown.
;     
;     Otherwise, if at least one operand has a known true result for
;     `inhabitant?-knowable` and at least one has a known false result
;     for it, then a known nothing (or, for a check, `#f`).
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
;     `inhabitant?-knowable` predicate, which may be an instance of
;     `prop:expressly-knowable-predicate?`, or if neither of them has
;     a known true result for it, then unknown.
;     
;     Otherwise, if at least one operand has a known true result for
;     `inhabitant?-knowable` and at least one has a known false result
;     for it, then a known nothing.
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
    #:known-reflexive? [known-reflexive? #t]
    #:known-distinct? [known-distinct? known-reflexive?]
    #:known-discrete? [known-discrete? #f]
    #:inhabitant?-knowable inhabitant?-knowable
    
    #:==?
    [ ==?
      (if (not known-reflexive?)
        (fn a b #f)
        (fn a b /equal-always? a b))]
    
    )
  (->*
    (#:inhabitant?-knowable (-> any/c (knowable/c boolean?)))
    (
      #:known-reflexive? boolean?
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
      (expect known-reflexive? #t
        (uninformative-smoosh-reports)
      /constant-smoosh-reports /delay
        (expect (inhabitant?-knowable a) (known #t) (unknown)
        /known /just /delay/strict /known a)))
    
    #:get-smoosh-and-comparison-of-two-reports
    (fn self a b
      (expect (inhabitant?-knowable a) (known a-inhabits?)
        (uninformative-smoosh-and-comparison-of-two-reports)
      /expect (inhabitant?-knowable b) (known b-inhabits?)
        (uninformative-smoosh-and-comparison-of-two-reports)
      /expect (or a-inhabits? b-inhabits?) #t
        (uninformative-smoosh-and-comparison-of-two-reports)
      /expect (and a-inhabits? b-inhabits?) #t
        (smoosh-and-comparison-of-two-reports-with-hesitation-at-discrepancies
          (false-smoosh-and-comparison-of-two-reports))
      /smoosh-and-comparison-of-two-reports-with-hesitation-at-discrepancies
        #:known-distinct? known-distinct?
        #:known-discrete? known-discrete?
        (constant-smoosh-and-comparison-of-two-reports
          (make-yknow-from-value-knowable-promise /delay /known
            (maybe-if (==? a b) /fn /make-yknow-from-value a)))))
    
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
;     `inhabitant?-knowable` predicate, which may be an instance of
;     `prop:expressly-knowable-predicate?`, or if neither of them has
;     a known true result for it, then unknown.
;     
;     Otherwise, if at least one operand has a known true result for
;     `inhabitant?-knowable` and at least one has a known false result
;     for it, then a known nothing.
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
;     `inhabitant?-knowable` predicate, which may be an instance of
;     `prop:expressly-knowable-predicate?`, or if neither of them has
;     a known true result for it, then unknown.
;     
;     Otherwise, if at least one operand has a known true result for
;     `inhabitant?-knowable` and at least one has a known false result
;     for it, then a known `#f`.
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
    #:inhabitant?-knowable inhabitant?-knowable)
  (->*
    (#:inhabitant?-knowable (-> any/c (knowable/c boolean?)))
    (#:known-distinct? boolean? #:known-discrete? boolean?)
    expressly-smooshable-dynamic-type-impl?)
  (make-expressly-smooshable-dynamic-type-impl
    
    #:get-smoosh-of-zero-reports
    (fn self
      (uninformative-smoosh-reports))
    
    #:get-smoosh-of-one-reports
    (fn self a
      (constant-smoosh-reports /delay
        (expect (inhabitant?-knowable a) (known #t) (unknown)
        /known /just /delay/strict /known a)))
    
    #:get-smoosh-and-comparison-of-two-reports
    (fn self a b
      (expect (inhabitant?-knowable a) (known a-inhabits?)
        (uninformative-smoosh-and-comparison-of-two-reports)
      /expect (inhabitant?-knowable b) (known b-inhabits?)
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
      /w- on-check-result-yknow
        (fn should-a-be-small? should-b-be-small?
          (dissectfn (list)
            (make-yknow-from-value-knowable-promise /delay
              (boolean-and-knowable-thunk-zip* /list
                (fn /boolean-or-knowable-thunk-zip* /list
                  (fn /known /not should-a-be-small?)
                  (fn /falsable->uninformative-knowable
                    (inhabitant-chaperone-of? b a)))
                (fn /boolean-or-knowable-thunk-zip* /list
                  (fn /known /not should-b-be-small?)
                  (fn /falsable->uninformative-knowable
                    (inhabitant-chaperone-of? a b)))))))
      /w- on-smoosh-result-yknow-maybe-yknow
        (fn acceptable-result?
          (dissectfn (list)
            (make-yknow-from-value-knowable-promise /delay/strict
              (if (acceptable-result? a)
                (known /just /make-yknow-from-value a)
              /if (acceptable-result? b)
                (known /just /make-yknow-from-value b)
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
      /sequence*
        (smoosh-and-comparison-of-two-report-zip*-map (list)
          #:on-check-result-yknow (on-check-result-yknow #f #f)
          #:on-smoosh-result-yknow-maybe-yknow
          (on-smoosh-result-yknow-maybe-yknow
            path-related-acceptable-result?))
        (smoosh-and-comparison-of-two-report-zip*-map (list)
          #:on-<=?-yknow (on-check-result-yknow #t #f)
          #:on->=?-yknow (on-check-result-yknow #f #t)
          #:on-join-yknow-maybe-yknow
          (on-smoosh-result-yknow-maybe-yknow
            (fn v
              (and
                (inhabitant-chaperone-of? v a)
                (inhabitant-chaperone-of? v b))))
          #:on-meet-yknow-maybe-yknow
          (on-smoosh-result-yknow-maybe-yknow
            (fn v
              (and
                (inhabitant-chaperone-of? a v)
                (inhabitant-chaperone-of? b v))))
          #:on-==-yknow-maybe-yknow
          (on-smoosh-result-yknow-maybe-yknow ==-acceptable-result?)
          #:on-path-related-yknow-maybe-yknow
          (on-smoosh-result-yknow-maybe-yknow
            path-related-acceptable-result?))
        (smoosh-and-comparison-of-two-reports-zip*-map (list)
          #:on-check-result-yknow (on-check-result-yknow #t #t)
          #:on-smoosh-result-yknow-maybe-yknow
          (on-smoosh-result-yknow-maybe-yknow
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
      (sequence*
        (constant-smoosh-equal-hash-code-support-report
          (delay /hash-code-0 a))
        (constant-smoosh-equal-hash-code-support-reports
          (delay /hash-code-1+ a))))
    
    ))

(define/own-contract
  (make-expressly-custom-gloss-key-dynamic-type-impl-for-atom
    #:known-reflexive? [known-reflexive? #t]
    #:eq-matters? [eq-matters? #f]
    
    #:ignore-chaperones?
    [ignore-chaperones? (or (not known-reflexive?) eq-matters?)]
    
    #:mutable? [mutable? eq-matters?]
    #:known-distinct? [known-distinct? known-reflexive?]
    #:known-discrete? [known-discrete? #f]
    #:inhabitant?-knowable inhabitant?-knowable)
  (->*
    (#:inhabitant?-knowable (-> any/c (knowable/c boolean?)))
    (
      #:known-reflexive? boolean?
      #:eq-matters? boolean?
      #:ignore-chaperones? boolean?
      #:mutable? boolean?
      #:known-distinct? boolean?
      #:known-discrete? boolean?)
    expressly-custom-gloss-key-dynamic-type-impl?)
  (make-expressly-custom-gloss-key-dynamic-type-impl
    
    #:get-custom-gloss-key-reports
    (fn self a
      (expect known-reflexive? #t
        (uninformative-custom-gloss-key-reports)
      /expect (inhabitant?-knowable a) (known #t)
        (uninformative-custom-gloss-key-reports)
      /w- make-distinct
        (fn info-level
          (if mutable?
            (w- guard-wrapper
              (if eq-matters?
                (fn inhabitant /lift-struct /eq-wrapper inhabitant)
                (fn inhabitant
                  (lift-struct /equal-always-wrapper inhabitant)))
            /make-roughly-constant-custom-gloss-key-report
              (fn cgkr-get-tgs-k
                (known /identifiable-object-tagged-glossesque-sys
                  (fn reports
                    (cgkr-get-tgs-k /sequence-ref reports info-level))
                  guard-wrapper)))
            (constant-custom-gloss-key-report
              #:tagged-glossesque-sys-knowable
              (if eq-matters?
                (known /make-tagged-glossesque-sys
                  #:equal-always-free-vars (inhabitant?-knowable)
                  inhabitant?-knowable
                  (fn gss /eq-atom-glossesque-sys gss))
                (known /make-tagged-glossesque-sys
                  #:equal-always-free-vars (inhabitant?-knowable)
                  inhabitant?-knowable
                  (fn gss /equal-always-atom-glossesque-sys gss))))))
      /w- distinct-0-cgkr (make-distinct 0)
      /w- distinct-1+-cgkrs
        (sequence-map
          (fn i
            (w- info-level (add1 i)
            /if (or eq-matters? ignore-chaperones?)
              (make-distinct info-level)
            /if mutable?
              (make-roughly-constant-custom-gloss-key-report
                (fn cgkr-get-tgs-k
                  (known /identifiable-object-tagged-glossesque-sys
                    (fn reports
                      (cgkr-get-tgs-k
                        (sequence-ref reports info-level)))
                    (fn inhabitant /lift-struct inhabitant))))
              (constant-custom-gloss-key-report
                #:tagged-glossesque-sys-knowable
                (known /make-tagged-glossesque-sys
                  #:equal-always-free-vars (inhabitant?-knowable)
                  inhabitant?-knowable
                  (fn gss
                    (chaperone=-indistinct-atom-glossesque-sys
                      gss))))))
          (in-naturals))
      /w- indistinct-inhabitant?-knowable
        (fn v
          (knowable-bind (inhabitant?-knowable v) /fn inhabits?
          /falsable->uninformative-knowable inhabits?))
      /w- indistinct-0-cgkr
        (constant-custom-gloss-key-report
          #:tagged-glossesque-sys-knowable
          (if eq-matters?
            (known /make-tagged-glossesque-sys
              
              #:equal-always-free-vars
              (indistinct-inhabitant?-knowable)
              
              indistinct-inhabitant?-knowable
              (fn gss /eq-indistinct-atom-glossesque-sys gss))
            (known /make-tagged-glossesque-sys
              
              #:equal-always-free-vars
              (indistinct-inhabitant?-knowable)
              
              indistinct-inhabitant?-knowable
              (fn gss
                (equal-always-indistinct-atom-glossesque-sys gss)))))
      /w- indistinct-1+-cgkrs
        (if (or eq-matters? ignore-chaperones?)
          (in-cycle /list indistinct-0-cgkr)
          (constant-custom-gloss-key-reports
            #:tagged-glossesque-sys-knowable
            (known /make-tagged-glossesque-sys
              
              #:equal-always-free-vars
              (indistinct-inhabitant?-knowable)
              
              indistinct-inhabitant?-knowable
              (fn gss
                (chaperone=-indistinct-atom-glossesque-sys gss)))))
      /if (and known-distinct? known-discrete?)
        (sequence* distinct-0-cgkr distinct-1+-cgkrs)
      /if (not known-distinct?)
        (sequence* indistinct-0-cgkr indistinct-1+-cgkrs)
      /sequence*
        (custom-gloss-key-report-map indistinct-0-cgkr
          #:on-==-tagged-glossesque-sys-knowable
          (fn tgs-k
            (custom-gloss-key-report-get-==-tagged-glossesque-sys-knowable
              distinct-0-cgkr)))
        distinct-1+-cgkrs))
    
    ))

(define/own-contract
  (make-expressly-smooshable-bundle-property-for-atom
    #:known-reflexive? [known-reflexive? #t]
    #:eq-matters? [eq-matters? #f]
    
    #:ignore-chaperones?
    [ignore-chaperones? (or (not known-reflexive?) eq-matters?)]
    
    #:mutable? [mutable? eq-matters?]
    #:known-distinct? [known-distinct? known-reflexive?]
    #:known-discrete? [known-discrete? #f]
    #:inhabitant?-knowable inhabitant?-knowable
    
    #:==?
    [ ==?
      (if (not known-reflexive?)
        (fn a b #f)
      /if eq-matters?
        (fn a b /eq? a b)
        (fn a b /equal-always? a b))]
    
    #:hash-code
    [ hash-code
      (if (not known-reflexive?)
        (fn a /uninformative-hash-code)
      /if eq-matters?
        (fn a /eq-hash-code a)
        (fn a /equal-always-hash-code a))]
    
    #:hash-code-0 [hash-code-0 hash-code]
    #:hash-code-1+ [hash-code-1+ hash-code])
  (->*
    (#:inhabitant?-knowable (-> any/c (knowable/c boolean?)))
    (
      #:known-reflexive? boolean?
      #:eq-matters? boolean?
      #:ignore-chaperones? boolean?
      #:mutable? boolean?
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
                #:inhabitant?-knowable inhabitant?-knowable
                #:==? ==?)
              (make-expressly-smooshable-dynamic-type-impl-for-chaperone-of-atom
                #:known-distinct? known-distinct?
                #:known-discrete? known-discrete?
                #:inhabitant?-knowable inhabitant?-knowable))))
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
              #:known-reflexive? known-reflexive?
              #:eq-matters? eq-matters?
              #:ignore-chaperones? ignore-chaperones?
              #:mutable? mutable?
              #:known-distinct? known-distinct?
              #:known-discrete? known-discrete?
              #:inhabitant?-knowable inhabitant?-knowable))))))
  prop:bundle)

; Level 0:
;   <=, >=, path-related, join, meet:
;     If the operands are not both `flvector?` values, then unknown.
;     
;     If the operands are `eq?`, then the first operand (or, for a
;     check, `#t`).
;     
;     Otherwise, unknown.
;   ==:
;     If neither operand is an `flvector?` value, then unknown.
;     
;     If either operand is a non-`flvector?` value that's
;     `known-identifiable-object-or-not?`, then a known nothing.
;     
;     If the operands are not both `flvector?` values, then unknown.
;     
;     If the operands are `eq?`, then the first operand.
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
      
      #:inhabitant?-knowable
      (raw-knowable-predicate-by-appraisal flvector?
        known-identifiable-object-or-not?)
      
      )
    (trivial)))

; Level 0:
;   <=, >=, path-related, join, meet:
;     If the operands are not both `fxvector?` values, then unknown.
;     
;     Otherwise, if the operands are `eq?`, then the first operand
;     (or, for a check, `#t`).
;     
;     Otherwise, unknown.
;   ==:
;     If neither operand is an `fxvector?` value, then unknown.
;     
;     If either operand is a non-`fxvector?` value that's
;     `known-identifiable-object-or-not?`, then a known nothing.
;     
;     If the operands are not both `fxvector?` values, then unknown.
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
      
      #:inhabitant?-knowable
      (raw-knowable-predicate-by-appraisal fxvector?
        known-identifiable-object-or-not?)
      
      )
    (trivial)))

(define/own-contract (base-syntactic-atom? v)
  (-> any/c boolean?)
  (or (symbol? v) (keyword? v) (null? v)))

; Level 0:
;   <=, >=, path-related, join, meet:
;     If the operands are not both `base-syntactic-atom?` values, then
;     unknown.
;     
;     If the operands are `equal-always?`, then the first operand (or,
;     for a check, `#t`).
;     
;     Otherwise, unknown.
;   ==:
;     If neither operand is a `base-syntactic-atom?` value, then
;     unknown.
;     
;     If either operand is a non-`base-syntactic-atom?` value that's
;     `known-s-expression-landmark-or-not?`, then a known nothing.
;     
;     If the operands are not both `base-syntactic-atom?` values, then
;     unknown.
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
      
      #:inhabitant?-knowable
      (raw-knowable-predicate-by-appraisal base-syntactic-atom?
        known-s-expression-landmark-or-not?)
      
      )
    (trivial))
  
  (#:prop
    prop:expressly-potentially-an-s-expression-landmark-dynamic-type
    (make-expressly-potentially-an-s-expression-landmark-dynamic-type-impl
      #:value-s-expression-landmark?-knowable
      (fn a-dt a
        (known #t))))
  
  )

(define/own-contract (base-unidentifiable-literal? v)
  (-> any/c boolean?)
  (or
    (nan-number? v)
    (nan-extflonum? v)
    (regexp? v)
    (compiled-expression? v)))

; Level 0:
;   <=, >=, path-related, join, meet:
;     Unknown.
;   ==:
;     If neither operand is a `base-unidentifiable-literal?` value,
;     then unknown.
;     
;     Otherwise, unknown.
; Level 1+:
;   path-related, join, meet, ==:
;     Same as the description of level 0 ==.
;   <=, >=:
;     Same as the description of level 0 == as a check.
;
(define-imitation-simple-struct
  (base-unidentifiable-literal-dynamic-type?)
  base-unidentifiable-literal-dynamic-type
  'base-unidentifiable-literal-dynamic-type (current-inspector)
  (auto-write)
  
  (#:prop
    (make-expressly-smooshable-bundle-property-for-atom
      #:known-reflexive? #f
      
      #:inhabitant?-knowable
      (raw-knowable-predicate base-unidentifiable-literal?)
      
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
;     If neither operand is a `boolean?` value, then unknown.
;     
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
      #:inhabitant?-knowable (raw-knowable-predicate boolean?))
    (trivial)))

; Level 0:
;   <=, >=, path-related, join, meet:
;     If the operands are not both characters, then unknown.
;     
;     If the operands are `equal-always?`, then the first operand (or,
;     for a check, `#t`).
;     
;     Otherwise, unknown.
;   ==:
;     If neither operand is a character, then unknown.
;     
;     If the operands are not both characters, then unknown.
;     
;     If the operands are `equal-always?`, then the first operand.
;     
;     Otherwise, unknown.
; Level 1+:
;   path-related, join, meet, ==:
;     Same as the description of level 0 ==.
;   <=, >=:
;     Same as the description of level 0 == as a check.
;
(define-imitation-simple-struct (char-dynamic-type?) char-dynamic-type
  'char-dynamic-type (current-inspector) (auto-write)
  
  (#:prop
    (make-expressly-smooshable-bundle-property-for-atom
      #:ignore-chaperones? #t
      #:known-distinct? #f
      #:inhabitant?-knowable (raw-knowable-predicate char?))
    (trivial)))

; Level 0:
;   <=, >=, path-related, join, meet:
;     If the operands are not both immutable strings, then unknown.
;     
;     If the operands are `equal-always?`, then the first operand (or,
;     for a check, `#t`).
;     
;     Otherwise, unknown.
;   ==:
;     If neither operand is an immutable string, then unknown.
;     
;     If the operands are not both immutable strings, then unknown.
;     
;     If the operands are `equal-always?`, then the first operand.
;     
;     Otherwise, unknown.
; Level 1+:
;   path-related, join, meet, ==:
;     Same as the description of level 0 ==.
;   <=, >=:
;     Same as the description of level 0 == as a check.
;
(define-imitation-simple-struct (immutable-string-dynamic-type?)
  immutable-string-dynamic-type
  'immutable-string-dynamic-type (current-inspector) (auto-write)
  
  (#:prop
    (make-expressly-smooshable-bundle-property-for-atom
      #:ignore-chaperones? #t
      #:known-distinct? #f
      
      #:inhabitant?-knowable
      (raw-knowable-predicate immutable-string?)
      
      )
    (trivial)))

; Level 0:
;   <=, >=, path-related, join, meet:
;     If the operands are not both immutable byte strings, then
;     unknown.
;     
;     If the operands are `equal-always?`, then the first operand (or,
;     for a check, `#t`).
;     
;     Otherwise, unknown.
;   ==:
;     If neither operand is an immutable byte string, then unknown.
;     
;     If the operands are not both immutable byte strings, then
;     unknown.
;     
;     If the operands are `equal-always?`, then the first operand.
;     
;     Otherwise, unknown.
; Level 1+:
;   path-related, join, meet, ==:
;     Same as the description of level 0 ==.
;   <=, >=:
;     Same as the description of level 0 == as a check.
;
(define-imitation-simple-struct (immutable-bytes-dynamic-type?)
  immutable-bytes-dynamic-type
  'immutable-bytes-dynamic-type (current-inspector) (auto-write)
  
  (#:prop
    (make-expressly-smooshable-bundle-property-for-atom
      #:ignore-chaperones? #t
      #:known-distinct? #f
      
      #:inhabitant?-knowable
      (raw-knowable-predicate /fn v /and (bytes? v) (immutable? v))
      
      )
    (trivial)))

(define/own-contract (non-nan-number-glossesque-sys gss)
  (-> glossesque-summary-sys? glossesque-sys?)
  (normalized-glossesque-sys gss /fn k /normalize-non-nan-number k))

(define non-nan-number-dynamic-type-inhabitant?-knowable
  (raw-knowable-predicate non-nan-number?))

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
;     If neither operand is a `number?` value without NaN parts, then
;     unknown.
;     
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
;     If neither operand is a `number?` value without NaN parts, then
;     unknown.
;     
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
        (w- inhabitant?-knowable
          non-nan-number-dynamic-type-inhabitant?-knowable
        /expect (inhabitant?-knowable a) (known a-inhabits?)
          (uninformative-smoosh-and-comparison-of-two-reports)
        /expect (inhabitant?-knowable b) (known b-inhabits?)
          (uninformative-smoosh-and-comparison-of-two-reports)
        /expect (or a-inhabits? b-inhabits?) #t
          (uninformative-smoosh-and-comparison-of-two-reports)
        /expect (and a-inhabits? b-inhabits?) #t
          (smoosh-and-comparison-of-two-reports-with-hesitation-at-discrepancies
            (false-smoosh-and-comparison-of-two-reports))
        /w- report-1+
          (constant-smoosh-and-comparison-of-two-reports
            (make-yknow-from-value-knowable-promise /delay /known
              (maybe-if (equal-always? a b) /fn
                (make-yknow-from-value a))))
        /if (= a b)
          (sequence*
            (constant-smoosh-and-comparison-of-two-report
              (make-yknow-from-value /just /make-yknow-from-value a))
            report-1+)
        /w- real?-promise
          (delay /and (zero? /imag-part a) (zero? /imag-part b))
        /w- <=?-yknow
          (make-yknow-from-value-promise-maybe-knowable-promise
            (promise-map real?-promise /fn real?
              (knowable-if real? /fn
                (just /delay /<= (real-part a) (real-part b)))))
        /w- >=?-yknow
          (make-yknow-from-value-promise-maybe-knowable-promise
            (promise-map real?-promise /fn real?
              (knowable-if real? /fn
                (just /delay />= (real-part a) (real-part b)))))
        /w- join-yknow-maybe-yknow
          (yknow-map <=?-yknow /fn result
            (just /make-yknow-from-value /if result b a))
        /w- meet-yknow-maybe-yknow
          (yknow-map <=?-yknow /fn result
            (just /make-yknow-from-value /if result a b))
        /w- path-related-yknow-maybe-yknow
          (make-yknow-from-value-knowable-promise
            (promise-map real?-promise /fn real?
              (knowable-if real? /fn
                (just /make-yknow-from-value a))))
        /sequence*
          (smoosh-and-comparison-of-two-report-zip*-map (list)
            #:on-<=?-yknow
            (dissectfn (list)
              <=?-yknow)
            #:on->=?-yknow
            (dissectfn (list)
              >=?-yknow)
            #:on-join-yknow-maybe-yknow
            (dissectfn (list)
              join-yknow-maybe-yknow)
            #:on-meet-yknow-maybe-yknow
            (dissectfn (list)
              meet-yknow-maybe-yknow)
            #:on-==-yknow-maybe-yknow
            (dissectfn (list)
              (make-yknow-from-value /nothing))
            #:on-path-related-yknow-maybe-yknow
            (dissectfn (list)
              path-related-yknow-maybe-yknow))
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
        (w- inhabitant?-knowable
          non-nan-number-dynamic-type-inhabitant?-knowable
        /expect (non-nan-number? a) #t
          (uninformative-custom-gloss-key-reports)
        /sequence*
          (custom-gloss-key-report-zip*-map (list)
            #:on-==-tagged-glossesque-sys-knowable
            (dissectfn (list)
              (known /make-tagged-glossesque-sys
                #:equal-always-free-vars (inhabitant?-knowable)
                inhabitant?-knowable
                (fn gss /non-nan-number-glossesque-sys gss)))
            #:on-path-related-tagged-glossesque-sys-knowable
            (dissectfn (list)
              (if (zero? /imag-part a)
                (known /make-tagged-glossesque-sys
                  #:equal-always-free-vars (inhabitant?-knowable)
                  (fn v
                    (knowable-map (inhabitant?-knowable v)
                    /fn v-inhabits?
                      (and v-inhabits? (zero? /imag-part v))))
                  (fn gss /terminal-glossesque-sys gss))
                (w- a (normalize-non-nan-number a)
                /known /make-tagged-glossesque-sys
                  #:equal-always-free-vars (inhabitant?-knowable a)
                  (fn v
                    (knowable-bind (inhabitant?-knowable v)
                    /fn v-inhabits?
                    /mat v-inhabits? #f (known #f)
                    /knowable-if (= a v) /fn #t))
                  (fn gss /terminal-glossesque-sys gss)))))
          (constant-custom-gloss-key-reports
            #:tagged-glossesque-sys-knowable
            (known /make-tagged-glossesque-sys
              #:equal-always-free-vars (inhabitant?-knowable)
              inhabitant?-knowable
              (fn gss /equal-always-atom-glossesque-sys gss)))))
      
      ))
  
  )

(define non-nan-extflonum-dynamic-type-inhabitant?-knowable
  (raw-knowable-predicate non-nan-extflonum?))

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
;     If neither operand is a non-NaN `extflonum?` value, then
;     unknown.
;     
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
;     If neither operand is a non-NaN `extflonum?` value, then
;     unknown.
;     
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
        (w- inhabitant?-knowable
          non-nan-extflonum-dynamic-type-inhabitant?-knowable
        /expect (inhabitant?-knowable a) (known a-inhabits?)
          (uninformative-smoosh-and-comparison-of-two-reports)
        /expect (inhabitant?-knowable b) (known b-inhabits?)
          (uninformative-smoosh-and-comparison-of-two-reports)
        /expect (or a-inhabits? b-inhabits?) #t
          (uninformative-smoosh-and-comparison-of-two-reports)
        /expect (and a-inhabits? b-inhabits?) #t
          (smoosh-and-comparison-of-two-reports-with-hesitation-at-discrepancies
            (false-smoosh-and-comparison-of-two-reports))
        /w- report-1+
          (constant-smoosh-and-comparison-of-two-reports
            (make-yknow-from-value-knowable-promise /delay /known
              (maybe-if (equal-always? a b) /fn
                (make-yknow-from-value a))))
        /if (extfl= a b)
          (sequence*
            (constant-smoosh-and-comparison-of-two-report
              (make-yknow-from-value /just /make-yknow-from-value a))
            report-1+)
        /w- <=?-yknow
          (make-yknow-from-value-promise-maybe-knowable-promise
            (delay/strict /known /just /delay /extfl<= a b))
        /w- >=?-yknow
          (make-yknow-from-value-promise-maybe-knowable-promise
            (delay/strict /known /just /delay /extfl>= a b))
        /w- join-yknow-maybe-yknow
          (yknow-map <=?-yknow /fn result
            (just /make-yknow-from-value /if result b a))
        /w- meet-yknow-maybe-yknow
          (yknow-map <=?-yknow /fn result
            (just /make-yknow-from-value /if result a b))
        /sequence*
          (smoosh-and-comparison-of-two-report-zip*-map (list)
            #:on-<=?-yknow
            (dissectfn (list)
              <=?-yknow)
            #:on->=?-yknow
            (dissectfn (list)
              >=?-yknow)
            #:on-join-yknow-maybe-yknow
            (dissectfn (list)
              join-yknow-maybe-yknow)
            #:on-meet-yknow-maybe-yknow
            (dissectfn (list)
              meet-yknow-maybe-yknow)
            #:on-==-yknow-maybe-yknow
            (dissectfn (list)
              (make-yknow-from-value /nothing))
            #:on-path-related-yknow-maybe-yknow
            (dissectfn (list)
              (make-yknow-from-value /just /make-yknow-from-value a)))
          report-1+))
      
      ))
  
  (#:prop prop:expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type
    (make-expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type-impl-for-atom
      
      #:hash-code-0
      (fn a
        (expect (non-nan-extflonum? a) #t (uninformative-hash-code)
        /equal-always-hash-code /normalize-non-nan-extflonum a))
      
      #:hash-code-1+ (fn a /equal-always-hash-code a)))
  
  (#:prop prop:expressly-custom-gloss-key-dynamic-type
    (make-expressly-custom-gloss-key-dynamic-type-impl
      
      #:get-custom-gloss-key-reports
      (fn self a
        (w- inhabitant?-knowable
          non-nan-extflonum-dynamic-type-inhabitant?-knowable
        /expect (non-nan-extflonum? a) #t
          (uninformative-custom-gloss-key-reports)
        /constant-custom-gloss-key-reports
          #:tagged-glossesque-sys-knowable
          (known /make-tagged-glossesque-sys
            #:equal-always-free-vars (inhabitant?-knowable)
            inhabitant?-knowable
            (fn gss /equal-always-atom-glossesque-sys gss))))
      
      ))
  
  )

; This is an appropriate dynamic type of cons cells,
; information-ordered in a way that's consistent with `chaperone-of?`
; as long as the keys' and values' information orderings are. This is
; an instance of
; `make-expressly-smooshable-dynamic-type-impl-from-equal-always-list-isomorphism`
; and uses `maybe-min-yknow-zip*-map/indistinct`.
;
; NOTE: This would be used like so:
;
#;
(#:prop prop:expressly-has-dynamic-type
  (make-expressly-has-dynamic-type-impl /fn bindings any-dt self
    (cons-dynamic-type any-dt)))
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
      
      #:inhabitant?-knowable
      (raw-knowable-predicate-by-appraisal pair?
        known-s-expression-landmark-or-not?)
      
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
          #:on-smoosh-result-yknow-maybe-yknow
          (fn ymy
            (yknow-map ymy /fn ym
              (maybe-map ym /fn y
                (yknow-map y /fn result
                  (cons result result)))))))
      
      )
    (trivial))
  
  (#:prop
    prop:expressly-potentially-an-s-expression-landmark-dynamic-type
    (make-expressly-potentially-an-s-expression-landmark-dynamic-type-impl
      #:value-s-expression-landmark?-knowable
      (fn a-dt a
        (known #t))))
  
  )

; This is an appropriate dynamic type of immutable vectors and their
; chaperones, information-ordered in a way that's consistent with
; `chaperone-of?` as long as the elements' information orderings are.
; This is an instance of
; `make-expressly-smooshable-dynamic-type-impl-from-chaperone-of-list-isomorphism`
; and uses `maybe-min-yknow-zip*-map/indistinct`.
;
; NOTE: This would be used like so:
;
#;
(#:prop prop:expressly-has-dynamic-type
  (make-expressly-has-dynamic-type-impl /fn bindings any-dt self
    (immutable-vector-dynamic-type any-dt)))
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
      
      #:inhabitant?-knowable
      (raw-knowable-predicate /fn v /and (vector? v) (immutable? v))
      
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
; all the operands pass their `#:inhabitant?-knowable` predicate, this
; one considers `base-mutable-readable?` values to be known
; inhabitants and other `known-identifiable-object-or-not?` values to
; be known non-inhabitants, and it reports a known inhabitant and a
; known non-inhabitant as being known to be distinct.
;
(define-imitation-simple-struct (base-mutable-readable-dynamic-type?)
  base-mutable-readable-dynamic-type
  'base-mutable-readable-dynamic-type (current-inspector) (auto-write)
  
  (#:prop
    (make-expressly-smooshable-bundle-property-for-atom
      #:mutable? #t
      
      #:inhabitant?-knowable
      (raw-knowable-predicate-by-appraisal base-mutable-readable?
        known-identifiable-object-or-not?)
      
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
      
      #:inhabitant?-knowable
      (raw-knowable-predicate /fn v /and (box? v) (immutable? v))
      
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
          #:on-smoosh-result-yknow-maybe-yknow
          (fn ymy
            (yknow-map ymy /fn ym
              (maybe-map ym /fn y
                (yknow-map y /fn result
                  (box-immutable result)))))))
      
      )
    (trivial))
  
  )

; This is an appropriate dynamic type of immutable prefab structs and
; their chaperones, information-ordered in a way that's consistent
; with `chaperone-of?` as long as the elements' information orderings
; are. This is an instance of
; `make-expressly-smooshable-dynamic-type-impl-from-chaperone-of-list-isomorphism`
; and uses `maybe-min-yknow-zip*-map/indistinct`.
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
      
      #:inhabitant?-knowable
      (raw-knowable-predicate immutable-prefab-struct?)
      
      #:->list (fn s /cdr /vector->list /struct->vector s)
      
      #:example-and-list->
      (fn example lst
        (apply make-prefab-struct (prefab-struct-key example) lst))
      
      )
    (trivial))
  
  )

; This is an appropriate dynamic type of immutable hash tables and
; their chaperones, information-ordered in a way that's consistent
; with `chaperone-of?` as long as the keys' and values' information
; orderings are. This is an instance of
; `make-expressly-smooshable-dynamic-type-impl-from-chaperone-of-list-isomorphism`
; and uses `maybe-min-yknow-zip*-map/indistinct`.
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
      
      #:inhabitant?-knowable
      (raw-knowable-predicate /fn v /and (hash? v) (immutable? v))
      
      #:->->list
      (fn a
        (w- keys (hash-keys a)
        /fn b
          (append* /for/list ([k (in-list keys)])
            (list (hash-ref-key b k) (hash-ref b k)))))
      
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
          (for/list
            ([entry (in-slice 2 (in-list element-hash-codes))])
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
  (define
    (get-smoosh-and-comparison-of-two-reports dt a b get-via-operand)
    (dissect dt (case-dynamic-type any-dt)
    /w-loop next cases cases
      (expect cases (cons case cases)
        (uninformative-smoosh-and-comparison-of-two-reports)
      /dissect case (list check? make-specific-dt)
      /match (list (check? a) (check? b))
        [ (list #t #t)
          (w- specific-dt (make-specific-dt any-dt)
          /get-via-operand specific-dt a b)]
        [ (list #t #f)
          (if
            (list-any cases /dissectfn (list check? make-specific-dt)
              (check? b))
            distinct-cases-smoosh-and-comparison-of-two-reports
            (uninformative-smoosh-and-comparison-of-two-reports))]
        [ (list #f #t)
          (if
            (list-any cases /dissectfn (list check? make-specific-dt)
              (check? a))
            distinct-cases-smoosh-and-comparison-of-two-reports
            (uninformative-smoosh-and-comparison-of-two-reports))]
        [(list #f #f) (next cases)])))
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
        
        #:get-smoosh-and-comparison-of-two-reports-via-first
        (fn self a b
          (get-smoosh-and-comparison-of-two-reports self a b
          /fn specific-dt a b
            (dynamic-type-get-smoosh-and-comparison-of-two-reports-via-first
              specific-dt a b)))
        
        #:get-smoosh-and-comparison-of-two-reports-via-second
        (fn self a b
          (get-smoosh-and-comparison-of-two-reports self a b
          /fn specific-dt a b
            (dynamic-type-get-smoosh-and-comparison-of-two-reports-via-second
              specific-dt a b)))
        
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
        base-unidentifiable-literal?
        (fn any-dt /base-unidentifiable-literal-dynamic-type))
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

(match-define (list base-non-literal? base-non-literal-dynamic-type)
  (dynamic-type-case-by-cases 'base-non-literal-dynamic-type /list
    (list
      base-mutable-readable?
      (fn any-dt /base-mutable-readable-dynamic-type))
    (list flvector? (fn any-dt /flvector-dynamic-type))
    (list fxvector? (fn any-dt /fxvector-dynamic-type))
    (list
      base-syntactic-atom?
      (fn any-dt /base-syntactic-atom-dynamic-type))
    (list pair? (fn any-dt /cons-dynamic-type any-dt))))

(match-define (list base-readable? base-readable-dynamic-type)
  (dynamic-type-case-by-cases 'base-readable-dynamic-type /list
    (list
      base-non-literal?
      (fn any-dt /base-non-literal-dynamic-type any-dt))
    base-literal-dynamic-type-case))

; Level 0:
;   <=, >=, path-related, join, meet:
;     If the operands are not both `nothing?` values, then unknown.
;     
;     Otherwise, the first operand (or, for a check, `#t`).
;   ==:
;     If neither operand is a `nothing?` value, then unknown.
;     
;     If either operand is a `just?` value, then a known nothing.
;     
;     If the operands are not both `nothing?` values, then unknown.
;     
;     Otherwise, then the first operand.
; Level 1+:
;   path-related, join, meet, ==:
;     Same as the description of level 0 ==.
;   <=, >=:
;     Same as the description of level 0 == as a check.
;
(define-imitation-simple-struct (nothing-dynamic-type?)
  nothing-dynamic-type
  'nothing-dynamic-type (current-inspector) (auto-write)
  
  (#:prop
    (make-expressly-smooshable-bundle-property-for-atom
      #:ignore-chaperones? #t
      
      #:inhabitant?-knowable
      (raw-knowable-predicate-by-appraisal nothing? maybe?)
      
      )
    (trivial)))

; This is an appropriate dynamic type of `just?` values. This is an
; instance of
; `make-expressly-smooshable-dynamic-type-impl-from-equal-always-list-isomorphism`.
;
; Note that while most such instances return known results only when
; all the operands pass their `#:inhabitant?-knowable` predicate, this
; one considers `just?` values to be known inhabitants and `nothing?`
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
      
      #:inhabitant?-knowable
      (raw-knowable-predicate-by-appraisal just? maybe?)
      
      #:->list (dissectfn (just e) /list e)
      
      #:example-and-list->
      (fn example lst
        (dissect lst (list e)
        /just e))
      
      #:get-smoosh-of-zero-reports
      (dissectfn (just-dynamic-type any-dt)
        (smoosh-reports-map
          (dynamic-type-get-smoosh-of-zero-reports any-dt)
          #:on-smoosh-result-yknow-maybe-yknow
          (fn ymy
            (yknow-map ymy /fn ym
              (maybe-map ym /fn y
                (yknow-map y /fn result-value
                  (just result-value)))))))
      
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
      #:inhabitant?-knowable (raw-knowable-predicate trivial?))
    (trivial)))

(define/own-contract
  (on-known-smoosh-result-yknow-maybe-yknow operands ymy)
  (-> (listof known?) (yknow/c (maybe/c (yknow/c known?)))
    (yknow/c (maybe/c (yknow/c known?))))
  (yknow-map ymy /fn ym
    (maybe-map ym /fn y
      (yknow-map y /fn result-value
        (w-loop next operands operands
          (expect operands (cons operand operands)
            (known result-value)
          /dissect operand (known operand-value)
          /if (eq? operand-value result-value)
            operand
          /next operands))))))

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
;   meet, join, ==:
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
;     Otherwise, if the operands are both `unknown?` values, then the
;     first operand (a known unknown).
;     
;     Otherwise, for:
;       meet:
;         If the operands are both `known?` values and performing the
;         same smoosh on their values succeeds, then that result
;         wrapped in a `known?`.
;         
;         Otherwise, if the operands are both `known?` values (and
;         performing the same smoosh on their values fails), unknown.
;         
;         Otherwise, the `unknown?` value.
;       join:
;         If the operands are both `known?` values, then the result of
;         performing the same smoosh on their values, then wrapping
;         the result in a `known?` if it's successful.
;         
;         Otherwise, the `known?` operand.
;       ==:
;         If the operands are both `known?` values, then the result of
;         performing the same smoosh on their values, then wrapping
;         the result in a `known?` if it's successful.
;         
;         Otherwise, a known nothing.
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
            #:on-smoosh-result-yknow-maybe-yknow
            (fn ymy
              (on-known-smoosh-result-yknow-maybe-yknow
                (list a)
                ymy)))
        /sequence* (uninformative-smoosh-report)
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
          (if (or (example-unknown? a) (example-unknown? b))
            (constant-smoosh-and-comparison-of-two-reports
              (make-yknow-from-value /just /make-yknow-from-value a))
          /uninformative-smoosh-and-comparison-of-two-reports)
        /mat (list a b) (list (known a-value) (known b-value))
          (smoosh-and-comparison-of-two-reports-map
            #:on-smoosh-result-yknow-maybe-yknow
            (fn ymy
              (on-known-smoosh-result-yknow-maybe-yknow
                (list a b)
                ymy))
            (smoosh-and-comparison-of-two-reports-map
              (dynamic-type-get-smoosh-and-comparison-of-two-reports
                any-dt a-value b-value)
              
              #:on-path-related-yknow-maybe-yknow
              (fn ymy
                (make-yknow-from-value
                  (just /make-yknow-from-value a-value)))
              
              #:on-meet-yknow-maybe-yknow
              (fn ymy
                (yknow-map/knowable ymy /fn ym
                  (knowable-if (just? ym) /fn ym)))
              
              #:on-smoosh-result-yknow-maybe-yknow
              (fn ymy
                ymy)
              
              ))
        /sequence* (uninformative-smoosh-and-comparison-of-two-report)
          (smoosh-and-comparison-of-two-report-zip*-map (list)
            
            #:on-<=?-yknow
            (w- result (unknown? a)
              (dissectfn (list)
                (make-yknow-from-value result)))
            
            #:on->=?-yknow
            (w- result (unknown? b)
              (dissectfn (list)
                (make-yknow-from-value result)))
            
            #:on-join-yknow-maybe-yknow
            (w- result (if (known? a) a b)
              (dissectfn (list)
                (make-yknow-from-value
                  (just /make-yknow-from-value result))))
            
            #:on-meet-yknow-maybe-yknow
            (w- result (if (known? a) b a)
              (dissectfn (list)
                (make-yknow-from-value
                  (just /make-yknow-from-value result))))
            
            #:on-==-yknow-maybe-yknow
            (dissectfn (list)
              (make-yknow-from-value /nothing))
            
            #:on-path-related-yknow-maybe-yknow
            (dissectfn (list)
              (make-yknow-from-value
                (just /make-yknow-from-value a)))
            
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
        /sequence*
          (uninformative-smoosh-equal-hash-code-support-report)
          (constant-smoosh-equal-hash-code-support-reports
            (delay
              (hash-code-combine
                (equal-always-hash-code example-unknown?))))))
      
      ))
  
  (#:prop prop:expressly-custom-gloss-key-dynamic-type
    (make-expressly-custom-gloss-key-dynamic-type-impl
      
      #:get-custom-gloss-key-reports
      (fn self a
        (dissect self (knowable-dynamic-type any-dt)
        ; NOTE: This report is shared between the `example-unknown?`
        ; case and the `known?` case. Whichever one of these
        ; glossesques is used in a gloss first will take care of the
        ; other one's duties. Inlining the
        ; `knowable-tagged-glossesque-sys` function would change the
        ; behavior ever-so-slightly by making the two reports not
        ; share an equal tag, and hence making glossesques based on
        ; them not merge with each other quite the same way when
        ; calling `ladder-skv-union-of-two-knowable`. But actually,
        ; since these glossesques have an unwavering size of one (1)
        ; entry anyway, merging them in one big step for having the
        ; same tag and merging them one entry at a time are probably
        ; comparable in efficiency.
        /w- knowable-tagged-glossesque-sys
          (fn
            (make-tagged-glossesque-sys
              (raw-knowable-predicate knowable?)
              (fn gss /terminal-glossesque-sys gss)))
        /w- unknown-tagged-glossesque-sys
          (fn
            (make-tagged-glossesque-sys
              (raw-knowable-predicate-by-appraisal unknown? knowable?)
              (fn gss /terminal-glossesque-sys gss)))
        /if (example-unknown? a)
          (sequence*
            (uninformative-custom-gloss-key-report)
            (custom-gloss-key-report-zip*-map (list)
              
              #:on-path-related-tagged-glossesque-sys-knowable
              (dissectfn (list)
                (known /knowable-tagged-glossesque-sys))
              
              #:on-==-tagged-glossesque-sys-knowable
              (dissectfn (list)
                (known /unknown-tagged-glossesque-sys))
              
              )
            (constant-custom-gloss-key-reports
              #:tagged-glossesque-sys-knowable
              (known /unknown-tagged-glossesque-sys)))
        /expect a (known a-value)
          (uninformative-custom-gloss-key-reports)
        /dissect
          (custom-gloss-key-reports-map
            (dynamic-type-get-custom-gloss-key-reports any-dt a-value)
            #:on-tagged-glossesque-sys-knowable
            (derive-tagged-glossesque-sys inhabitant?-knowable get-gs
              (fn v
                (expect v (known v) (unknown)
                /inhabitant?-knowable v))
              (fn gss
                (glossesque-sys-map-key gss (fn gss /get-gs gss)
                  #:name 'unwrap-known
                  #:granted-key (dissectfn (known k) k)))
              
              #:inhabitant-get-identifiable-object-guard-wrapper-maybe-knowable
              inhabitant-get-identifiable-object-guard-wrapper-maybe-knowable
              (fn inhabitant
                (known /nothing))))
          (sequence* report-0 report-1 report-2+)
        /w- tgs-k-uninhabited-by-unknown
          (derive-tagged-glossesque-sys inhabitant?-knowable get-gs
            (fn v
              (if (unknown? v) (known #f)
              /inhabitant?-knowable v))
            get-gs)
        /sequence*
          report-0
          (custom-gloss-key-report-map report-1
            
            #:on-path-related-tagged-glossesque-sys-knowable
            (fn tgs-k
              (known /knowable-tagged-glossesque-sys))
            
            #:on-==-tagged-glossesque-sys-knowable
            tgs-k-uninhabited-by-unknown
            
            )
          (custom-gloss-key-reports-map report-2+
            #:on-tagged-glossesque-sys-knowable
            tgs-k-uninhabited-by-unknown)))
      
      ))
  
  )

(define/own-contract
  (on-path-related-wrapper-smoosh-result-yknow-maybe-yknow
    operands ymy)
  (->
    (listof path-related-wrapper?)
    (yknow/c (maybe/c (yknow/c path-related-wrapper?)))
    (yknow/c (maybe/c (yknow/c path-related-wrapper?))))
  (yknow-map ymy /fn ym
    (maybe-map ym /fn y
      (yknow-map y /fn result-value
        (w-loop next operands operands
          (expect operands (cons operand operands)
            (path-related-wrapper result-value)
          /dissect operand (path-related-wrapper operand-value)
          /if (eq? operand-value result-value)
            operand
          /next operands))))))

(define/own-contract (on-path-related-wrapper-hash-code-promise p)
  (-> (promise/c fixnum?) (promise/c fixnum?))
  (promise-map p /fn value-hash-code
    (hash-code-combine
      (equal-always-hash-code path-related-wrapper?)
      value-hash-code)))

(define/own-contract
  (path-related-wrapper-smoosh-reports-from-value-reports
    operands value-reports)
  (->
    (listof path-related-wrapper?)
    (endless-sequence/c smoosh-report?)
    (endless-sequence/c smoosh-report?))
  (dissect
    (smoosh-reports-map value-reports
      #:on-smoosh-result-yknow-maybe-yknow
      (fn ymy
        (on-path-related-wrapper-smoosh-result-yknow-maybe-yknow
          operands ymy)))
    (sequence* report-0 report-1+)
  /sequence*
    (constant-smoosh-report
      (smoosh-report-path-related-yknow-maybe-yknow report-0))
    report-1+))

(define/own-contract
  (path-related-wrapper-smoosh-and-comparison-of-two-reports-from-value-reports
    operands value-reports)
  (->
    (listof path-related-wrapper?)
    (endless-sequence/c smoosh-and-comparison-of-two-report?)
    (endless-sequence/c smoosh-and-comparison-of-two-report?))
  (dissect
    (smoosh-and-comparison-of-two-reports-map value-reports
      #:on-smoosh-result-yknow-maybe-yknow
      (fn ymy
        (on-path-related-wrapper-smoosh-result-yknow-maybe-yknow
          operands ymy)))
    (sequence* report-0 report-1+)
  /sequence*
    (constant-smoosh-and-comparison-of-two-report
      (smoosh-report-path-related-yknow-maybe-yknow
        (smoosh-and-comparison-of-two-report-get-smoosh-report
          report-0)))
    report-1+))

(define/own-contract
  (path-related-wrapper-smoosh-equal-hash-code-support-reports-from-value-reports
    value-reports)
  (-> (endless-sequence/c smoosh-equal-hash-code-support-report?)
    (endless-sequence/c smoosh-equal-hash-code-support-report?))
  (dissect
    (smoosh-equal-hash-code-support-reports-map value-reports
      #:on-hash-code-promise
      on-path-related-wrapper-hash-code-promise)
    (sequence* report-0 report-1+)
  /sequence*
    (constant-smoosh-equal-hash-code-support-report
      (smoosh-equal-hash-code-support-report-path-related-hash-code-promise
        report-0))
    report-1+))

(define/own-contract
  (path-related-wrapper-custom-gloss-key-reports-from-value-reports
    value-reports)
  (-> (endless-sequence/c custom-gloss-key-report?)
    (endless-sequence/c custom-gloss-key-report?))
  (dissect
    (custom-gloss-key-reports-map value-reports
      #:on-tagged-glossesque-sys-knowable
      (derive-tagged-glossesque-sys inhabitant?-knowable get-gs
        (fn v
          (expect v (path-related-wrapper v) (unknown)
          /inhabitant?-knowable v))
        (fn gss
          (glossesque-sys-map-key gss (fn gss /get-gs gss)
            #:name 'unwrap-path-related-wrapper
            #:granted-key (dissectfn (path-related-wrapper k) k)))
        
        #:inhabitant-get-identifiable-object-guard-wrapper-maybe-knowable
        inhabitant-get-identifiable-object-guard-wrapper-maybe-knowable
        (fn inhabitant
          (knowable-map
            (inhabitant-get-identifiable-object-guard-wrapper-maybe-knowable inhabitant)
          /fn guard-wrapper-m
            (maybe-map guard-wrapper-m /fn guard-wrapper
              (compose guard-wrapper /fn v
                (path-related-wrapper v)))))))
    (sequence* report-0 report-1+)
  /sequence*
    (constant-custom-gloss-key-report
      #:tagged-glossesque-sys-knowable
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
;     Otherwise, if the result of performing a level-0 path-related
;     smoosh on their values is a known success and any operand's
;     unwrapped value is `eq?` to it, then that operand.
;     
;     Otherwise, if it's a known success, that value wrapped up as a
;     `path-related-wrapper?` value.
;     
;     Otherwise, the same known failure or unknown result.
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
;     Otherwise, if the result of performing the same smoosh or check
;     on their values is a known success and we're doing a check, then
;     `#t`.
;     
;     Otherwise, if it's a known success and any operand's unwrapped
;     value is `eq?` to it, then that operand.
;     
;     Otherwise, if it's a known success, that value wrapped up as a
;     `path-related-wrapper?` value.
;     
;     Otherwise, the same known failure or unknown result.
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
          (list)
          (dynamic-type-get-smoosh-of-zero-reports any-dt)))
      
      #:get-smoosh-of-one-reports
      (fn self a
        (dissect self (path-related-wrapper-dynamic-type any-dt)
        /expect a (path-related-wrapper a-value)
          (uninformative-smoosh-reports)
        /path-related-wrapper-smoosh-reports-from-value-reports
          (list a)
          (dynamic-type-get-smoosh-of-one-reports any-dt a-value)))
      
      #:get-smoosh-and-comparison-of-two-reports-via-first
      (fn self a b
        (dissect self (path-related-wrapper-dynamic-type any-dt)
        /expect a (path-related-wrapper a-value)
          (uninformative-smoosh-and-comparison-of-two-reports)
        /expect b (path-related-wrapper b-value)
          (uninformative-smoosh-and-comparison-of-two-reports)
        /path-related-wrapper-smoosh-and-comparison-of-two-reports-from-value-reports
          (list a b)
          (dynamic-type-get-smoosh-and-comparison-of-two-reports-via-first
            any-dt a-value b-value)))
      
      #:get-smoosh-and-comparison-of-two-reports-via-second
      (fn self a b
        (dissect self (path-related-wrapper-dynamic-type any-dt)
        /expect a (path-related-wrapper a-value)
          (uninformative-smoosh-and-comparison-of-two-reports)
        /expect b (path-related-wrapper b-value)
          (uninformative-smoosh-and-comparison-of-two-reports)
        /path-related-wrapper-smoosh-and-comparison-of-two-reports-from-value-reports
          (list a b)
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
  (on-info-wrapper-smoosh-result-yknow-maybe-yknow operands ymy)
  (->
    (listof info-wrapper?)
    (yknow/c (maybe/c (yknow/c info-wrapper?)))
    (yknow/c (maybe/c (yknow/c info-wrapper?))))
  (yknow-map ymy /fn ym
    (maybe-map ym /fn y
      (yknow-map y /fn result-value
        (w-loop next operands operands
          (expect operands (cons operand operands)
            (info-wrapper result-value)
          /dissect operand (info-wrapper operand-value)
          /if (eq? operand-value result-value)
            operand
          /next operands))))))

(define/own-contract (on-info-wrapper-hash-code-promise p)
  (-> (promise/c fixnum?) (promise/c fixnum?))
  (promise-map p /fn value-hash-code
    (hash-code-combine
      (equal-always-hash-code info-wrapper?)
      value-hash-code)))

(define/own-contract
  (info-wrapper-smoosh-reports-from-value-reports
    operands value-reports)
  (-> (listof info-wrapper?) (endless-sequence/c smoosh-report?)
    (endless-sequence/c smoosh-report?))
  (dissect
    (smoosh-reports-map value-reports
      #:on-smoosh-result-yknow-maybe-yknow
      (fn ymy
        (on-info-wrapper-smoosh-result-yknow-maybe-yknow
          operands ymy)))
    (sequence* report-0 report-1+)
    report-1+))

(define/own-contract
  (info-wrapper-smoosh-and-comparison-of-two-reports-from-value-reports
    operands value-reports)
  (->
    (listof info-wrapper?)
    (endless-sequence/c smoosh-and-comparison-of-two-report?)
    (endless-sequence/c smoosh-and-comparison-of-two-report?))
  (dissect
    (smoosh-and-comparison-of-two-reports-map value-reports
      #:on-smoosh-result-yknow-maybe-yknow
      (fn ymy
        (on-info-wrapper-smoosh-result-yknow-maybe-yknow
          operands ymy)))
    (sequence* report-0 report-1+)
    report-1+))

(define/own-contract
  (info-wrapper-smoosh-equal-hash-code-support-reports-from-value-reports
    value-reports)
  (-> (endless-sequence/c smoosh-equal-hash-code-support-report?)
    (endless-sequence/c smoosh-equal-hash-code-support-report?))
  (dissect
    (smoosh-equal-hash-code-support-reports-map value-reports
      #:on-hash-code-promise on-info-wrapper-hash-code-promise)
    (sequence* report-0 report-1+)
    report-1+))

(define/own-contract
  (info-wrapper-custom-gloss-key-reports-from-value-reports
    value-reports)
  (-> (endless-sequence/c custom-gloss-key-report?)
    (endless-sequence/c custom-gloss-key-report?))
  (dissect
    (custom-gloss-key-reports-map value-reports
      #:on-tagged-glossesque-sys-knowable
      (derive-tagged-glossesque-sys inhabitant?-knowable get-gs
        (fn v
          (expect v (info-wrapper v) (unknown)
          /inhabitant?-knowable v))
        (fn gss
          (glossesque-sys-map-key gss (fn gss /get-gs gss)
            #:name 'unwrap-info-wrapper
            #:granted-key (dissectfn (info-wrapper k) k)))
        
        #:inhabitant-get-identifiable-object-guard-wrapper-maybe-knowable
        inhabitant-get-identifiable-object-guard-wrapper-maybe-knowable
        (fn inhabitant
          (knowable-map
            (inhabitant-get-identifiable-object-guard-wrapper-maybe-knowable inhabitant)
          /fn guard-wrapper-m
            (maybe-map guard-wrapper-m /fn guard-wrapper
              (compose guard-wrapper /fn v
                (info-wrapper v)))))))
    (sequence* report-0 report-1+)
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
;     Otherwise, if the result of processing the unwrapped values with
;     the same smoosh or check 1 level up is a known success and we're
;     doing a check, then `#t`.
;     
;     Otherwise, if it's a known success and any operand's unwrapped
;     value is `eq?` to it, then that operand.
;     
;     Otherwise, if it's a known success, that value wrapped up as an
;     `info-wrapper?` value.
;     
;     Otherwise, the same known failure or unknown result.
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
          (list)
          (dynamic-type-get-smoosh-of-zero-reports any-dt)))
      
      #:get-smoosh-of-one-reports
      (fn self a
        (dissect self (info-wrapper-dynamic-type any-dt)
        /expect a (info-wrapper a-value)
          (uninformative-smoosh-reports)
        /info-wrapper-smoosh-reports-from-value-reports
          (list a)
          (dynamic-type-get-smoosh-of-one-reports any-dt a-value)))
      
      #:get-smoosh-and-comparison-of-two-reports-via-first
      (fn self a b
        (dissect self (info-wrapper-dynamic-type any-dt)
        /expect a (info-wrapper a-value)
          (uninformative-smoosh-and-comparison-of-two-reports)
        /expect b (info-wrapper b-value)
          (uninformative-smoosh-and-comparison-of-two-reports)
        /info-wrapper-smoosh-and-comparison-of-two-reports-from-value-reports
          (list a b)
          (dynamic-type-get-smoosh-and-comparison-of-two-reports-via-first
            any-dt a-value b-value)))
      
      #:get-smoosh-and-comparison-of-two-reports-via-second
      (fn self a b
        (dissect self (info-wrapper-dynamic-type any-dt)
        /expect a (info-wrapper a-value)
          (uninformative-smoosh-and-comparison-of-two-reports)
        /expect b (info-wrapper b-value)
          (uninformative-smoosh-and-comparison-of-two-reports)
        /info-wrapper-smoosh-and-comparison-of-two-reports-from-value-reports
          (list a b)
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

(define (gloss-ref-entry/who who g k)
  (expect (gloss-ref-entry-maybe-knowable g k) (known entry-m)
    (raise-arguments-error who
      "tried to get a key that couldn't be verified equivalent to or distinct from all the existing keys"
      "gloss" g
      "key" k)
  /expect entry-m (just entry)
    (raise-arguments-error who
      "no value found for key"
      "gloss" g
      "key" k)
    entry))

(define/own-contract (gloss-ref-entry g k)
  (-> gloss? any/c (list/c any/c any/c))
  (gloss-ref-entry/who 'gloss-ref-entry g k))

(define/own-contract (gloss-ref g k)
  (-> gloss? any/c any/c)
  (dissect (gloss-ref-entry/who 'gloss-ref g k) (list k v)
    v))

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
; `make-expressly-smooshable-dynamic-type-impl-from-equal-always-list-isomorphism`
; and uses `maybe-min-yknow-zip*-map/indistinct`. Note that this
; instance's `inhabitant-shallowly-equal-always?-knowable` can result
; in a non-`known?` value if any key comparison does.
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
      
      #:inhabitant?-knowable (raw-knowable-predicate gloss?)
      
      #:->->list
      (fn a
        (w- keys (sequence->list /gloss-keys a)
        /fn b
          (append* /for/list ([k (in-list keys)])
            (dissect (gloss-ref-entry b k) (list a-k v)
            /list a-k v))))
      
      #:example-and-list->
      (fn example lst
        (make-gloss
          (for/list ([entry (in-slice 2 (in-list lst))])
            (dissect entry (list k v)
            /cons k v))))
      
      #:combine-element-hash-codes
      (fn element-hash-codes
        (hash-code-combine-unordered*
          (for/list
            ([entry (in-slice 2 (in-list element-hash-codes))])
            (dissect entry (list k v)
            /hash-code-combine k v))))
      
      #:inhabitant-shallowly-equal-always?-knowable
      (fn a b
        (gloss-equal-always?-knowable a b /fn a b /known #t))
      
      )
    (trivial))
  
  )

; Level 0:
;   <=, >=, path-related, join, meet:
;     If the operands are not both `eq-wrapper?` values, then unknown.
;     
;     Otherwise, if the operands are `equal-always?`, then the first
;     operand (or, for a check, `#t`).
;     
;     Otherwise, unknown.
;   ==:
;     If the operands are not both `eq-wrapper?` values, then unknown.
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
(define-imitation-simple-struct (eq-wrapper-dynamic-type?)
  eq-wrapper-dynamic-type
  'eq-wrapper-dynamic-type (current-inspector) (auto-write)
  
  (#:prop
    (make-expressly-smooshable-bundle-property-for-atom
      #:ignore-chaperones? #t
      #:inhabitant?-knowable (raw-knowable-predicate eq-wrapper?))
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
      
      #:inhabitant?-knowable
      (raw-knowable-predicate equal-always-wrapper?)
      
      )
    (trivial)))

(define/own-contract
  (on-indistinct-wrapper-smoosh-result-yknow-maybe-yknow operands ymy)
  (->
    (listof indistinct-wrapper?)
    (yknow/c (maybe/c (yknow/c indistinct-wrapper?)))
    (yknow/c (maybe/c (yknow/c indistinct-wrapper?))))
  (yknow-map ymy /fn ym
    (maybe-map ym /fn y
      (yknow-map y /fn result-value
        (w-loop next operands operands
          (expect operands (cons operand operands)
            (indistinct-wrapper result-value)
          /dissect operand (indistinct-wrapper operand-value)
          /if (eq? operand-value result-value)
            operand
          /next operands))))))

(define/own-contract (on-indistinct-wrapper-hash-code-promise p)
  (-> (promise/c fixnum?) (promise/c fixnum?))
  (promise-map p /fn value-hash-code
    (hash-code-combine
      (equal-always-hash-code indistinct-wrapper?)
      value-hash-code)))

(define/own-contract
  (indistinct-wrapper-smoosh-reports-from-value-reports
    operands value-reports)
  (-> (listof indistinct-wrapper?) (endless-sequence/c smoosh-report?)
    (endless-sequence/c smoosh-report?))
  (smoosh-reports-with-hesitation-at-discrepancies
    #:known-distinct? #f
    (smoosh-reports-map value-reports
      #:on-smoosh-result-yknow-maybe-yknow
      (fn ymy
        (on-indistinct-wrapper-smoosh-result-yknow-maybe-yknow
          operands ymy)))))

(define/own-contract
  (indistinct-wrapper-smoosh-and-comparison-of-two-reports-from-value-reports
    operands value-reports)
  (->
    (listof indistinct-wrapper?)
    (endless-sequence/c smoosh-and-comparison-of-two-report?)
    (endless-sequence/c smoosh-and-comparison-of-two-report?))
  (smoosh-and-comparison-of-two-reports-with-hesitation-at-discrepancies
    #:known-distinct? #f
    (smoosh-and-comparison-of-two-reports-map value-reports
      #:on-smoosh-result-yknow-maybe-yknow
      (fn ymy
        (on-indistinct-wrapper-smoosh-result-yknow-maybe-yknow
          operands ymy)))))

(define/own-contract
  (indistinct-wrapper-smoosh-equal-hash-code-support-reports-from-value-reports
    value-reports)
  (-> (endless-sequence/c smoosh-equal-hash-code-support-report?)
    (endless-sequence/c smoosh-equal-hash-code-support-report?))
  (smoosh-equal-hash-code-support-reports-map value-reports
    #:on-hash-code-promise on-indistinct-wrapper-hash-code-promise))

(define/own-contract
  (indistinct-wrapper-custom-gloss-key-reports-from-value-reports
    value-reports)
  (-> (endless-sequence/c custom-gloss-key-report?)
    (endless-sequence/c custom-gloss-key-report?))
  (custom-gloss-key-reports-map value-reports
    #:on-tagged-glossesque-sys-knowable
    (derive-tagged-glossesque-sys inhabitant?-knowable get-gs
      (fn v
        (expect v (indistinct-wrapper v) (unknown)
        /inhabitant?-knowable v))
      (fn gss
        (indistinct-glossesque-sys
          (glossesque-sys-map-key gss (fn gss /get-gs gss)
            #:name 'unwrap-indistinct-wrapper
            #:granted-key (dissectfn (indistinct-wrapper k) k))))
      
      #:inhabitant-get-identifiable-object-guard-wrapper-maybe-knowable
      inhabitant-get-identifiable-object-guard-wrapper-maybe-knowable
      (fn inhabitant
        (known /nothing)))))

; Level 0+:
;   <=, >=, path-related, join, meet, ==:
;     If the operands are not both `indistinct-wrapper?` values, then
;     unknown.
;     
;     Otherwise, if the result of processing the unwrapped values with
;     the same smoosh or check is a known success and we're doing a
;     check, then `#t`.
;     
;     Otherwise, if it's a known success and any operand's unwrapped
;     value is `eq?` to it, then that operand.
;     
;     Otherwise, if it's a known success, that value wrapped up as a
;     `indistinct-wrapper?` value.
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
          (list)
          (dynamic-type-get-smoosh-of-zero-reports any-dt)))
      
      #:get-smoosh-of-one-reports
      (fn self a
        (dissect self (indistinct-wrapper-dynamic-type any-dt)
        /expect a (indistinct-wrapper a-value)
          (uninformative-smoosh-reports)
        /indistinct-wrapper-smoosh-reports-from-value-reports
          (list a)
          (dynamic-type-get-smoosh-of-one-reports any-dt a-value)))
      
      #:get-smoosh-and-comparison-of-two-reports-via-first
      (fn self a b
        (dissect self (indistinct-wrapper-dynamic-type any-dt)
        /expect a (indistinct-wrapper a-value)
          (uninformative-smoosh-and-comparison-of-two-reports)
        /expect b (indistinct-wrapper b-value)
          (uninformative-smoosh-and-comparison-of-two-reports)
        /indistinct-wrapper-smoosh-and-comparison-of-two-reports-from-value-reports
          (list a b)
          (dynamic-type-get-smoosh-and-comparison-of-two-reports-via-first
            any-dt a-value b-value)))
      
      #:get-smoosh-and-comparison-of-two-reports-via-second
      (fn self a b
        (dissect self (indistinct-wrapper-dynamic-type any-dt)
        /expect a (indistinct-wrapper a-value)
          (uninformative-smoosh-and-comparison-of-two-reports)
        /expect b (indistinct-wrapper b-value)
          (uninformative-smoosh-and-comparison-of-two-reports)
        /indistinct-wrapper-smoosh-and-comparison-of-two-reports-from-value-reports
          (list a b)
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
      (list eq-wrapper? (fn any-dt /eq-wrapper-dynamic-type))
      (list
        equal-always-wrapper?
        (fn any-dt /equal-always-wrapper-dynamic-type))
      (list
        indistinct-wrapper?
        (fn any-dt /indistinct-wrapper-dynamic-type any-dt)))))

(define/own-contract (get-dynamic-type-with-any-dynamic-type any-dt v)
  (-> any/c any/c any/c)
  (get-dynamic-type (gloss-union-of-zero) any-dt v))

(define-imitation-simple-struct (default-any-dynamic-type?)
  default-any-dynamic-type
  'default-any-dynamic-type (current-inspector) (auto-write)
  
  (#:prop prop:expressly-smooshable-dynamic-type
    (make-expressly-smooshable-dynamic-type-impl
      
      #:get-smoosh-of-zero-reports
      (fn self
        (uninformative-smoosh-reports))
      
      #:get-smoosh-of-one-reports
      (fn self a
        (w- a-dt (get-dynamic-type-with-any-dynamic-type self a)
        /dynamic-type-get-smoosh-of-one-reports a-dt a))
      
      #:get-smoosh-and-comparison-of-two-reports-via-first
      (fn self a b
        (w- a-dt (get-dynamic-type-with-any-dynamic-type self a)
        /dynamic-type-get-smoosh-and-comparison-of-two-reports-via-first
          a-dt a b))
      
      #:get-smoosh-and-comparison-of-two-reports-via-second
      (fn self a b
        (w- b-dt (get-dynamic-type-with-any-dynamic-type self b)
        /dynamic-type-get-smoosh-and-comparison-of-two-reports-via-second
          b-dt a b))
      
      ))
  
  (#:prop prop:expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type
    (make-expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type-impl
      
      #:get-smoosh-equal-hash-code-support-reports
      (fn self a
        (w- a-dt (get-dynamic-type-with-any-dynamic-type self a)
        /dynamic-type-get-smoosh-equal-hash-code-support-reports
          a-dt a))
      
      ))
  
  (#:prop prop:expressly-custom-gloss-key-dynamic-type
    (make-expressly-custom-gloss-key-dynamic-type-impl
      
      #:get-custom-gloss-key-reports
      (fn self a
        (w- a-dt (get-dynamic-type-with-any-dynamic-type self a)
        /dynamic-type-get-custom-gloss-key-reports a-dt a))
      
      ))
  
  )
(ascribe-own-contract default-any-dynamic-type? (-> any/c boolean?))

; This parameter's value is a thunk (defaulting to
; `default-any-dynamic-type`) that obtains the "any" dynamic type
; value that the application entrypoint deems most appropriate to the
; entire application.
;
(define/own-contract current-any-dynamic-type
  (parameter/c (-> any/c))
  (make-parameter /fn /default-any-dynamic-type))

(define/own-contract (any-dynamic-type)
  (-> any/c)
  (/current-any-dynamic-type))


; NOTE: If we ever have a `glossesque-sys?` based on AVL trees, for
; the sake of types which are hard to write hash code behavior for but
; which do admit a total ordering, it would be more apparent why we
; bother with tries and don't just hash everything. However, the tries
; are still necessary for us because of other aspects of the
; framework's generality, namely the fact that we perform smoosh join
; and smoosh meet operations and the fact that our smoosh operations
; sometimes compute unknown results.

; Except where noted (TODO) or where not mentioned (e.g., where
; pertaining to a type that didn't exist at the time of writing this),
; we've implemented smooshing, better `gen:equal-mode+hash` equality,
; `prop:expressly-equipped-with-smoosh-equal-hash-code-support-dynamic-type`
; hashing, and `prop:expressly-custom-gloss-key-dynamic-type` behavior
; for these types:
;
;   - Certain immutable types that can result from the default Racket
;     reader and tend to need to be treated as decisively distinct
;     cases when parsing s-expression-based code. We call these
;     s-expression landmark values, and they're known to be distinct
;     from all other values except possibly values which specify
;     otherwise. Landmarks are contrasted with types that potentially
;     allow more suggestive or special-purpose ideas of what
;     distinctions matter, such as number and string types with
;     multiple representations for the same essential value:
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
;       orderings are. Cons cells with distinct elements aren't known
;       to be distinct.
;
;   - Various mutable types that can result from the default Racket
;     reader, as well as the mutable equivalents of the immutable
;     types that can result from the default Racket reader, where
;     those exist. These are instances of something we refer to as
;     "identifiable objects," and we consider any identifiable object
;     to be distinct from all other values except possibly values
;     which specify otherwise:
;
;     - (Done) Mutable strings, mutable byte strings, mutable boxes,
;       mutable vectors, prefab structs with mutable fields, and
;       mutable hash tables, all equatable and distinguishable in a
;       way consistent with `equal-always?` and information-ordered in
;       a way consistent with `chaperone-of?`.
;
;     - (Done) Flvectors and fxvectors, all equatable and
;       distinguishable in a way consistent with `eq?`. (TODO: As of
;       Racket 8.12 [cs], the implementation of `equal-always?` for
;       flvectors and fxvectors is incorrect. Once we're on 8.13 or
;       so, simplify the design by grouping these with the other
;       mutable data structures.)
;
;   - Various immutable types that can result from the default Racket
;     reader that aren't specifically needed for s-expression parsing.
;     We're referring to these as `base-literal?` values (though we
;     only define `base-literal-dynamic-type-case`, not its
;     `base-literal?` predicate). The following are known to be
;     distinct from each other, a choice which aims to clarify which
;     parts of an s-expression's data should be inspectable by a
;     typical parser and which parts should be considered subject to
;     more unsensational changes (such as replacing strings with
;     normalized strings):
;
;     - (Done) Booleans, which are known to be distinct from each
;       other. They're not known to be ordered, not even by the
;       convention from abstract algebra that `#f` < `#t`; booleans
;       are often used in programming as a convenient stand-in for a
;       two-valued enum type which has no defined ordering.
;
;     - (Done) Numbers with NaN parts, not even known to be equal to
;       themselves.
;
;     - (Done) Numbers with no NaN parts, with the ones whose
;       imaginary parts are `=` zero being ordered in a way that's
;       consistent with `<=` and `=` on their real parts.
;
;     - (Done) NaN extflonums, not even known to be equal to
;       themselves.
;
;     - (Done) Non-NaN extflonums, ordered in a way consistent with
;       `extfl<=` and `extfl=`, and information-ordered in a way
;       consistent with `chaperone-of?`.
;
;     - (Done) Characters, immutable strings, and immutable byte
;       strings, which are known to be equal to themselves when
;       they're `equal-always?` but not known to be distinct from each
;       other. There are many possible ways to normalize and collate
;       Unicode strings, and the only thing that's necessarily obvious
;       across all of those is that identical Unicode scalar sequences
;       are equal. Byte strings may seem easier to justify
;       distinguishing from each other according to a byte-by-byte
;       comparison, but the reader gives them a text representation
;       that a programmer may refactor into similar Unicode text
;       without realizing they've made a change.
;
;     - (Done) Regular expressions (`regexp?`) and compiled code
;       expressions (`compiled-expression?`), not even known to be
;       equal to themselves. Comparing them by their exact source code
;       isn't necessarily consistent with the intent of the programmer
;       who wrote that code, and comparing them by their behavior
;       isn't necessarily feasible.
;
;     - (Done) Immutable boxes, ordered according to the elements'
;       orderings and in a way that's consistent with a
;       `chaperone-of?` information ordering if the elements'
;       orderings are.
;
;     - (Done) Immutable vectors, ordered according to the elements'
;       orderings and in a way that's consistent with a
;       `chaperone-of?` information ordering if the elements'
;       orderings are. Vectors of different lengths are known to be
;       distinct from each other. Vectors of the same length with
;       distinct elements aren't known to be distinct.
;
;     - (Done) Prefab structs with no mutable fields, ordered
;       according to the elements' orderings and in a way that's
;       consistent with a `chaperone-of?` information ordering if the
;       elements' orderings are. Prefab structs with different keys
;       and/or different numbers of fields are known to be distinct
;       from each other. Prefab structs with the same keys and same
;       number of fields but with distinct elements aren't known to be
;       distinct.
;
;     - (Done) Immutable hash tables with various comparison
;       functions, ordered according to the keys' and values'
;       orderings and in a way that's consistent with a
;       `chaperone-of?` information ordering if the elements'
;       orderings are. Hash tables which use different comparison
;       functions or which have different sets of keys according to
;       their comparison function are known to be distinct from each
;       other. Hash tables which use the same comparison function and
;       which have teh same set of keys according to their comparison
;       function but with distinct elements aren't known to be
;       distinct.
;
;     - (TODO) Potentially others in future versions of Racket. The
;       above list is up-to-date as of Racket 8.12.
;
;   - Types defined by Lathe Comforts that this smooshing framework
;     uses:
;
;     - (Done) `maybe?` values, ordered according to the elements'
;       orderings and in a way that's consistent with a
;       `chaperone-of?` information ordering if the elements'
;       orderings are. A `just?` value and a `nothing?` value are
;       known to be distinct from each other.
;
;     - (Done) `trivial?` values.
;
;   - Types defined here in smoosh.rkt:
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
;     - (NOTE: We won't be defining smooshability for `yknow?`
;       objects. These are more like codata (computation types) than
;       data (concrete types), and while we often represent specific
;       instances of codata using concrete representations, different
;       representations of the same type of codata may potentially be
;       designed to work best with different smooshing algorithms.
;       Rather than presuming to be able to mediate collaboration
;       between all of these different algorithms, we choose to leave
;       it up to users who want to smoosh `yknow?` objects to define
;       what "a `yknow?` object that can be smooshed" means for their
;       own purposes.)
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
;       known to be distinct from each other. `gloss?` values which
;       have the same set of keys according to smoosh-ordering but
;       which have distinct elements aren't known to be distinct.
;
;     - (Done) `eq-wrapper?` values, all equatable and distinguishable
;       with each other according to the `eq?` behavior of their
;       wrapped value.
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
;     - (NOTE: Someday we may be interested in smooshing type-objects
;       that implement `gen:expressly-smooshable-dynamic-type`, or
;       even other type-objects like `(uninformative-dynamic-type)`
;       that don't expressly implement that interface. Ideally, this
;       smooshing might allow an expressive set of smooshed
;       type-objects to report that they're `<=` when they're related
;       by subtyping. These type-objects would tend not to have
;       meaningful details independent of the set of values that
;       inhabit them. This smooshability would be relevant mainly when
;       these type-objects appear as elements of data structures that
;       would be otherwise smooshable. However, type-objects like
;       these are really more like codata (computation types) than
;       data (concrete types). While we often represent specific
;       instances of codata using concrete representations, different
;       representations of the same codata may potentially be designed
;       to work best with different smooshing algorithms. Rather than
;       presuming to be able to mediate collaboration between all of
;       these different algorithms, we choose to leave it up to users
;       who want to smoosh type-objects to define what "a type-object
;       that can be smooshed" means for their own purposes.)
;
;   - (TODO) Types defined by Lathe Comforts even if this smooshing
;     framework doesn't use them:
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

; TODO SMOOSH: Here's a plan:
;
;   - (Done) Add a method
;     `(tagged-glossesque-sys-inhabitant-get-identifiable-object-guard-wrapper-maybe-knowable tgs v)`
;     to `tagged-glossesque-sys?` returning a result that most
;     instances will define to be `(known (nothing))` (meaning that
;     this inhabitant is positively known to be distinct from any
;     value of any type if that value is an info representative at
;     this info level and the value it's a representative of has
;     object identity). The result should only be
;     `(known (just guard-wrapper))` for some `guard-wrapper` if the
;     `tagged-glossesque-sys?` is the one described next or a
;     `path-related-wrapper`- or `info-wrapper`-adding adjustment
;     thereof. The value of `guard-wrapper` is a one-argument
;     procedure that produces the value that will be compared for
;     `equal-always?` on behalf of the original.
;
;   - (Done) Make a standard `tagged-glossesque-sys?` at each info
;     level index and each choice of "==" or "path-related" that
;     accepts any value where
;     `tagged-glossesque-sys-inhabitant-get-identifiable-object-guard-wrapper-maybe-knowable`
;     is `(known (just guard-wrapper))` at that index and choice, and
;     that positively rejects any value for which it's
;     `(known (nothing))`. (Other values have unknown
;     inhabitant-ness.) It compares operands by passing them through
;     their respective `guard-wrapper`s to obtain representatives and
;     then calling `equal-always?` on the representatives, returning a
;     known nothing if that comparison results in `#f`. If the
;     comparison results in `#t`, then it goes on to compare the
;     representatives using `atom-chaperone=?`, returning the first
;     operand if that result is `#t`. Otherwise, it returns an unknown
;     result.
;
;   - (Done) Have the `flvector?` and `fxvector?` dynamic types use
;     `tagged-glossesque-sys?` values which implement
;     `tagged-glossesque-sys-inhabitant-get-identifiable-object-guard-wrapper-maybe-knowable`
;     with `(known (just (fn v (lift-struct (eq-wrapper v)))))`. Have
;     the other mutable object dynamic types instantiate it with
;     `(known (just (fn v (lift-struct (equal-always-wrapper v)))))` or
;     `(known (just (fn v (lift-struct v))))`.
;
;   - (Done) Make an interface
;     `expressly-potentially-an-s-expression-landmark-dynamic-type?`
;     with a "method"
;     `(dynamic-type-value-s-expression-landmark?-knowable dt v)`
;     returning a result that defaults to `(known #f)`. The result
;     should only be `(known #t)` if the value's info-level-zero
;     `tagged-glossesque-sys?` has an `inhabitant?-knowable` predicate
;     which returns a `(known #f)` result for non-special-cased inputs
;     for which the
;     `dynamic-type-value-s-expression-landmark?-knowable` result is
;     `(known #f)` or `(known #t)`. Sometimes, a dynamic type may have
;     a `tagged-glossesque-sys?` whose prospective inhabitants may be
;     special-cased to have `inhabitant?-knowable` results other than
;     `(known #f)`; in that case, the prospective inhabitant's own
;     dynamic type must also specify a corresponding special case for
;     this dynamic type's values so that the results are consistent.
;
;   - (Done) Have the `cons?`, `null?`, `symbol?` and `keyword?`
;     dynamic types instantiate
;     `expressly-potentially-an-s-expression-landmark-dynamic-type?`
;     with `(known #t)`. Have their info-level-zero
;     `tagged-glossesque-sys?` values' `inhabitant?-knowable`
;     predicates give known rejections for non-special-cased values
;     which have a `(known #t)` or `(known #f)` result for
;     `dynamic-type-value-s-expression-landmark?-knowable`.
;     The special-cased values for the `symbol?` dynamic type are
;     `symbol?` values, which are never known to be distinct from
;     each other and are sometimes even known to be equal. Likewise
;     with the `keyword?` dynamic type. The special-cased values for
;     the `cons?` dynamic type are `cons?` values, which compare in a
;     specific way that can sometimes determine that they're known to
;     be equal. The special-cased values for the `null?` dynamic type
;     are `null?` values, which are always equal to each other.
;
;   - (Done) Change the places we're using
;     `(raw-knowable-predicate-by-appraisal foo? base-non-literal?)`
;     for some `foo?` so that they use `(raw-knowable-predicate foo?)`
;     instead. The difference should now be covered by the way the
;     identifiable objects' predicates exclude
;     `known-identifiable-object-or-not?` and the way the
;     s-expression landmark values' predicates exclude
;     `known-s-expression-landmark-or-not?`.
;
;   - Is that actually true? Don't we generally need comparison logic
;     on both sides to agree? Maybe we're not making enough use of
;     `yknow?` functionality; we probably need to make known
;     non-inhabitants yield known-unspecified smoosh results.
;
;   - We've probably now brought the `dynamic-type-case-by-cases`
;     types' logic for distinguishing cases out of consistency with
;     the ways we're distinguishing identifiable objects and
;     s-expression landmarks from other values. Discontinue our use of
;     `dynamic-type-case-by-cases`, or at least of its
;     `#:known-distinct?` argument.
;
;   - Make standard `tagged-glossesque-sys?` values that can be used
;     by user-defined `unknown?` types -- or better yet, a structure
;     type property bundle for that specific situation.
;
;   - Have the `knowable?` dynamic type use the standard
;     `tagged-glossesque-sys?` values.
;
;   - (Done) Migrate our `inhabitant?` knowable predicates to more
;     fundamental `inhabitant?-knowable` procedures and some helper
;     functions to produce them.
;
;     - Commentary: The primary point of knowable predicates is to
;       allow us to gradually upgrade APIs by replacing some
;       predicates in covariant positions with knowable predicates. In
;       contravariant positions, they serve the secondary benefit of
;       allowing clients to opt to pass in something of a more
;       familiar type (regular old predicates), ignoring some of the
;       complexity of the API when they don't need it. However,
;       there's a potential hazard here: If the client defines a plain
;       old predicate and passes it in as per a point-free style, and
;       then the client changes it to be a knowable predicate, they
;       might not realize the place they've passed it into is going to
;       use that now-present information for its own purposes.
;       Essentially I'm thinking this is a problem with having
;       open-world-extensible-union types in contravariant positions
;       altogether, and treating `procedure?` as if it's one of those
;       is probably asking for trouble considering how often
;       `procedure?` appears in contravariant positions. One potential
;       solution is to consider knowable predicates to be a proper
;       subtype of plain old predicates (and likewise consider yknow
;       predicates to be a proper subtype of knowable predicates), so
;       that upgrades can happen in covariant positions but plain old
;       predicates can't be used where knowable predicates are
;       expected. But at any rate, we can get closer to that vision
;       now by updating some of our covariant positions to use types
;       that have no subtyping to consider.

; NOTE:
;
; It seems we aren't using `yknow?` values and the `...-via-second`
; method in the smooshing framework for as much as we thought we would
; have. When a type is a custom gloss key, everything we know about
; its value's comparisons has to be encoded not only as a two-argument
; equality comparison between values but also as a membership relation
; check between a glossesque and a prospective key value. In lieu of
; encouraging user code to inspect the structure of specific
; `typed-glossesque-sys?` objects to determine whether a key belongs
; to them, we've been designing special-purpose protocols like
; `tagged-glossesque-sys-inhabitant-get-identifiable-object-guard-wrapper-maybe-knowable`
; that allow either value (or either value's
; `tagged-glossesque-sys-inhabitant?-knowable` check) to compute a
; fully specified result.
;
; There are potentially some situations in which taking advantage of
; the `...-via-second` dispatch will be possible. In particular,
; dynamic types that represent codata type formers probably won't even
; have known `tagged-glossesque-sys?` objects, so it will be easier to
; keep the two-argument smooshing implementations consistent with that
; nonexistent information. Then again, codata types will require an
; unusual amount of care to be smooshable at all since the underlying
; finite representation of the same codata can vary, so examples of
; nontrivially smooshable ones may be scarce.
;
; TODO: Consider whether we might want to remove `yknow?` values
; altogether. Probably not. They're a viable technique for other
; dispatch situations, which we'll probably start to see many of once
; we're writing extensible libraries using these techniques. The idea
; of using it the way we are seems appropriate, even if the actual
; need for it isn't there yet.
;
; TODO: Alternatively, consider replacing
; `tagged-glossesque-sys-inhabitant?-knowable` with
; `value-inhabits-tagged-glossesque-sys?-yknow-via-tagged-glossesque-sys`.
; In that case, `prop:expressly-custom-gloss-key-dynamic-type` values
; should also have a
; `value-inhabits-tagged-glossesque-sys?-yknow-via-value-dynamic-type`
; method for checking whether their inhabitants are inhabitants of a
; given `tagged-glossesque-sys?`, so that a
; `value-inhabits-tagged-glossesque-sys?-yknow` call has more than one
; candidate to dispatch to. Furthermore, this check might be more
; useful if, instead of computing a boolean, it computed an upgraded
; `tagged-glossesque-sys?` and a glossesque upgrader that extended a
; glossesque with support for the unfamiliar value.
