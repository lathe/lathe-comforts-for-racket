#lang parendown/slash racket/base

; lathe-comforts/tests/smoosh
;
; Unit tests of the smoosh framework.

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


(require /only-in racket/extflonum extflonum-available?)
(require /only-in racket/fixnum fxvector)
(require /only-in racket/flonum flvector)
(require /only-in rackunit check-eq? check-equal? check-pred)

(require lathe-comforts/private/shim)
(init-shim)

(require lathe-comforts)
(require lathe-comforts/maybe)
(require lathe-comforts/private/smoosh)
(require /only-in lathe-comforts/private/smoosh
  [info-wrapper iw]
  [path-related-wrapper pw])
(require lathe-comforts/sequence)
(require lathe-comforts/trivial)

; (We provide nothing from this module.)



(define/own-contract (gloss . args)
  (->* () (list?) gloss?)
  (unless (even? /length args)
    (raise-arguments-error 'gloss
      "expected an even number of arguments"
      "args" args))
  (make-gloss
    (for/list ([entry (in-slice 2 (in-list args))])
      (dissect entry (list k v)
      /cons k v))))


; A stricter version of `yknow-value-knowable` that raises an error if
; the given `yknow?` value is known-unspecified, as in
; `(make-yknow-from-promise-maybe-knowable-promise /delay/strict /known /nothing)`.
;
; TODO SMOOSH: Someday, write unit tests involving known-unspecified
; `yknow?` values too. For instance, these might arise as intermediate
; results of info-smooshing an `example-unknown?` value with a
; user-defined `unknown?` value, since only the user-defined value
; could specify the result, and the `example-unknown?` value leaves
; the result known-unspecified so that the other operand's
; specification can be used without fear of potential conflict.
; However, we might simply consider `unknown?` values to be info
; smooshable only when they're related in a `chaperone-of?`-like way,
; in which case we might just consider every user-defined `unknown?`
; value to be info-smoosh-distinct from every `example-unknown?`
; value, making this a bad example.
;
(define (yknow-value-strictly-knowable y)
  (knowable-map (force /yknow-value-promise-maybe-knowable-promise y)
  /fn pm
    (expect pm (just p)
      (error "expected the `yknow?` values we were testing to never be known-unspecified")
    /force p)))

(define (ymy->kmk ymy)
  (knowable-map (yknow-value-strictly-knowable ymy) /fn ym
    (maybe-map ym /fn y
      (yknow-value-strictly-knowable y))))

(define (smoosh-report-exercise report)
  (w- path-related-kmk
    (ymy->kmk /smoosh-report-path-related-yknow-maybe-yknow report)
  /w- join-kmk (ymy->kmk /smoosh-report-join-yknow-maybe-yknow report)
  /w- meet-kmk (ymy->kmk /smoosh-report-meet-yknow-maybe-yknow report)
  /w- ==-kmk (ymy->kmk /smoosh-report-==-yknow-maybe-yknow report)
  /begin
    (expect ==-kmk (known /just _) (void) /begin
      (mat path-related-kmk (known /just _) (void)
        (error "expected == to imply path-related"))
      (mat join-kmk (known /just _) (void)
        (error "expected == to imply join"))
      (mat meet-kmk (known /just _) (void)
        (error "expected == to imply meet")))
    (expect join-kmk (known /just _) (void) /begin
      (mat path-related-kmk (known /just _) (void)
        (error "expected join to imply path-related")))
    (expect meet-kmk (known /just _) (void) /begin
      (mat path-related-kmk (known /just _) (void)
        (error "expected meet to imply path-related")))
    report))

(define (smoosh-and-comparison-of-two-report-exercise report)
  (w- smoosh-report
    (smoosh-report-exercise
      (smoosh-and-comparison-of-two-report-get-smoosh-report report))
  /w- <=?-k
    (yknow-value-strictly-knowable
      (smoosh-and-comparison-of-two-report-<=?-yknow report))
  /w- >=?-k
    (yknow-value-strictly-knowable
      (smoosh-and-comparison-of-two-report->=?-yknow report))
  /w- path-related-ymk
    (yknow-value-strictly-knowable
      (smoosh-report-path-related-yknow-maybe-yknow smoosh-report))
  /w- join-ymk
    (yknow-value-strictly-knowable
      (smoosh-report-join-yknow-maybe-yknow smoosh-report))
  /w- meet-ymk
    (yknow-value-strictly-knowable
      (smoosh-report-meet-yknow-maybe-yknow smoosh-report))
  /w- ==-ymk
    (yknow-value-strictly-knowable
      (smoosh-report-==-yknow-maybe-yknow smoosh-report))
  /begin
    (expect ==-ymk (known /just _) (void) /begin
      (mat <=?-k (known #t) (void)
        (error "expected == to imply <="))
      (mat >=?-k (known #t) (void)
        (error "expected == to imply >=")))
    (expect <=?-k (known #t) (void) /begin
      (mat join-ymk (known /just _) (void)
        (error "expected <= to imply join"))
      (mat meet-ymk (known /just _) (void)
        (error "expected <= to imply meet")))
    (expect >=?-k (known #t) (void) /begin
      (mat join-ymk (known /just _) (void)
        (error "expected >= to imply join"))
      (mat meet-ymk (known /just _) (void)
        (error "expected >= to imply meet")))
    report))

(define (smoosh-and-comparison-of-two-reports-exercise reports)
  ; TODO: Also verify that the results for any info level beyond N are
  ; only known if the result of the == smoosh for info level N is a
  ; known success.
  (sequence-map
    (fn report /smoosh-and-comparison-of-two-report-exercise report)
    reports))


(define
  (smooshable-exercise-first-smoosh-and-comparison-of-two-report a b)
  (smoosh-and-comparison-of-two-report-get-smoosh-report
    (smoosh-and-comparison-of-two-report-exercise
      (sequence-first
        (dynamic-type-get-smoosh-and-comparison-of-two-reports
          (any-dynamic-type)
          a
          b)))))

(define (smooshable-==-exercise-knowable-maybe-knowable a b)
  (ymy->kmk
    (smoosh-report-==-yknow-maybe-yknow
      (smooshable-exercise-first-smoosh-and-comparison-of-two-report
        a
        b))))

(define (smooshable-join-exercise-knowable-maybe-knowable a b)
  (ymy->kmk
    (smoosh-report-join-yknow-maybe-yknow
      (smooshable-exercise-first-smoosh-and-comparison-of-two-report
        a
        b))))

(define (smooshable-meet-exercise-knowable-maybe-knowable a b)
  (ymy->kmk
    (smoosh-report-meet-yknow-maybe-yknow
      (smooshable-exercise-first-smoosh-and-comparison-of-two-report
        a
        b))))


(define s= smooshable-==-exercise-knowable-maybe-knowable)
(define sj smooshable-join-exercise-knowable-maybe-knowable)
(define sm smooshable-meet-exercise-knowable-maybe-knowable)


(define-values
  (imp-prop:arbitrary imp-prop:arbitrary? imp-prop:arbitrary-get)
  (make-impersonator-property 'imp-prop:arbitrary))

(define mstr1 (string #\a))
(define mstr2 (string #\b))
(define mbytes1 (bytes 0))
(define mbytes2 (bytes 0))
(define mbox1 (box 0))
(define mbox2 (box 0))
(define mbox1-chap
  (chaperone-box mbox1
    (fn mbox current-v current-v)
    (fn mbox new-v new-v)))
(define mbox1-chap2
  (chaperone-box mbox1
    (fn mbox current-v current-v)
    (fn mbox new-v new-v)))
(define mbox1-chap-chap
  (chaperone-box mbox1-chap
    (fn mbox current-v current-v)
    (fn mbox new-v new-v)))
(define mv1 (vector 0))
(define mv2 (vector 0))
(define mv1-with-prop
  (chaperone-vector mv1 #f #f imp-prop:arbitrary #t))
(define mv1-chap
  (chaperone-vector mv1
    (fn mv i current-v current-v)
    (fn mv i new-v new-v)))
(define mv1-chap2
  (chaperone-vector mv1
    (fn mv i current-v current-v)
    (fn mv i new-v new-v)))
(define mv1-chap-with-prop
  (chaperone-vector mv1-chap #f #f imp-prop:arbitrary #t))
(define mv1-chap-chap
  (chaperone-vector mv1-chap
    (fn mv i current-v current-v)
    (fn mv i new-v new-v)))
(struct mprefab ([field #:mutable]) #:prefab)
(define mprefab1 (mprefab 0))
(define mprefab2 (mprefab 0))
(define mprefab1-with-prop
  (chaperone-struct mprefab1 struct:mprefab
    imp-prop:arbitrary #t))
(define mprefab1-chap
  (chaperone-struct mprefab1 mprefab-field
    (fn mprefab current-v current-v)))
(define mprefab1-chap2
  (chaperone-struct mprefab1 mprefab-field
    (fn mprefab current-v current-v)))
(define mprefab1-chap-with-prop
  (chaperone-struct mprefab1-chap struct:mprefab
    imp-prop:arbitrary #t))
(define mprefab1-chap-chap
  (chaperone-struct mprefab1-chap mprefab-field
    (fn mprefab current-v current-v)))
(define mhash1 (make-hash /list /cons 0 0))
(define mhash2 (make-hash /list /cons 0 0))
(define mhash1-chap
  (chaperone-hash mhash1
    (fn mhash k /values k /fn mhash k current-v current-v)
    (fn mhash k new-v /values k new-v)
    (fn mhash k k)
    (fn mhash k k)))
(define mhash1-chap2
  (chaperone-hash mhash1
    (fn mhash k /values k /fn mhash k current-v current-v)
    (fn mhash k new-v /values k new-v)
    (fn mhash k k)
    (fn mhash k k)))
(define mhash1-chap-chap
  (chaperone-hash mhash1-chap
    (fn mhash k /values k /fn mhash k current-v current-v)
    (fn mhash k new-v /values k new-v)
    (fn mhash k k)
    (fn mhash k k)))
(define flv1 (flvector 0.0))
(define flv2 (flvector 0.0))
(define fxv1 (fxvector 0))
(define fxv2 (fxvector 0))
(define str1 "a")
(define str2 "b")
(define bts1 #"a")
(define bts2 #"b")
(define regexp1 #rx"")
(define expr1 (compile #'0))
(define ibox1 (box-immutable 0))
(define ibox1-chap
  (chaperone-box ibox1
    (fn ibox current-v current-v)
    (fn ibox new-v new-v)))
(define ibox1-chap2
  (chaperone-box ibox1
    (fn ibox current-v current-v)
    (fn ibox new-v new-v)))
(define ibox1-chap-chap
  (chaperone-box ibox1-chap
    (fn ibox current-v current-v)
    (fn ibox new-v new-v)))
(define iv1 (vector-immutable 0))
(define iv1-chap
  (chaperone-vector iv1
    (fn iv i current-v current-v)
    (fn iv i new-v new-v)))
(define iv1-chap2
  (chaperone-vector iv1
    (fn iv i current-v current-v)
    (fn iv i new-v new-v)))
(define iv1-chap-with-prop
  (chaperone-vector iv1-chap #f #f imp-prop:arbitrary #t))
(define iv1-chap-chap
  (chaperone-vector iv1-chap
    (fn iv i current-v current-v)
    (fn iv i new-v new-v)))
(struct iprefab (field1 field2) #:prefab)
(define iprefab1 (iprefab 0 0))
(define iprefab1-chap
  (chaperone-struct iprefab1 iprefab-field1
    (fn iprefab current-v current-v)))
(define iprefab1-chap2
  (chaperone-struct iprefab1 iprefab-field1
    (fn iprefab current-v current-v)))
(define iprefab1-chap-with-prop
  (chaperone-struct iprefab1-chap struct:iprefab
    imp-prop:arbitrary #t))
(define iprefab1-chap-chap
  (chaperone-struct iprefab1-chap iprefab-field1
    (fn iprefab current-v current-v)))
(define ihash1 (hash #f 0 #t 0.0))
(define ihash1-chap
  (chaperone-hash ihash1
    (fn mhash k /values k /fn mhash k current-v current-v)
    (fn mhash k new-v /values k new-v)
    (fn mhash k k)
    (fn mhash k k)))
(define ihash1-chap2
  (chaperone-hash ihash1
    (fn mhash k /values k /fn mhash k current-v current-v)
    (fn mhash k new-v /values k new-v)
    (fn mhash k k)
    (fn mhash k k)))
(define ihash1-chap-chap
  (chaperone-hash ihash1-chap
    (fn mhash k /values k /fn mhash k current-v current-v)
    (fn mhash k new-v /values k new-v)
    (fn mhash k k)
    (fn mhash k k)))
(define eaw1 (equal-always-wrapper /box-immutable 0))
(define eaw2 (equal-always-wrapper /box-immutable 0))
(define eaw-different (equal-always-wrapper /box-immutable 1))

; This is a pair of values whose smoosh join, smoosh meet, and
; path-related smoosh actually fail (rather than just having unknown
; results).
(define path-failing-1 (iw #t))
(define path-failing-2 (iw #f))


(check-equal?
  (s= mstr1 mstr1)
  (known /just /known mstr1)
  "Smoosh works on `eq?` mutable strings")

(check-equal?
  (s= mstr1 mstr2)
  (known /nothing)
  "Smoosh fails on non-`eq?` mutable strings")

(check-equal?
  (sj mstr1 mstr1)
  (known /just /known mstr1)
  "Smoosh join works on `eq?` mutable strings")

(check-pred
  unknown?
  (sj mstr1 mstr2)
  "Smoosh join is unknown on non-`eq?` mutable strings")

(check-equal?
  (sm mstr1 mstr1)
  (known /just /known mstr1)
  "Smoosh meet works on `eq?` mutable strings")

(check-pred
  unknown?
  (sm mstr1 mstr2)
  "Smoosh meet is unknown on non-`eq?` mutable strings")

(check-equal?
  (s= (pw mstr1) (pw mstr1))
  (known /just /known /pw mstr1)
  "Path-related smoosh works on `eq?` mutable strings")

(check-pred
  unknown?
  (s= (pw mstr1) (pw mstr2))
  "Path-related smoosh is unknown on non-`eq?` mutable strings")

(check-equal?
  (s= (iw mstr1) (iw mstr1))
  (known /just /known /iw mstr1)
  "Info smoosh works on `eq?` mutable strings")

(check-equal?
  (s= (iw mstr1) (iw mstr2))
  (known /nothing)
  "Info smoosh fails on non-`eq?` mutable strings")

(check-equal?
  (sj (iw mstr1) (iw mstr1))
  (known /just /known /iw mstr1)
  "Info smoosh join works on `eq?` mutable strings")

(check-equal?
  (sj (iw mstr1) (iw mstr2))
  (known /nothing)
  "Info smoosh join fails on non-`eq?` mutable strings")

(check-equal?
  (sm (iw mstr1) (iw mstr1))
  (known /just /known /iw mstr1)
  "Info smoosh meet works on `eq?` mutable strings")

(check-equal?
  (sm (iw mstr1) (iw mstr2))
  (known /nothing)
  "Info smoosh meet fails on non-`eq?` mutable strings")

(check-equal?
  (s= (pw /iw mstr1) (pw /iw mstr1))
  (known /just /known /pw /iw mstr1)
  "Path-related info smoosh works on `eq?` mutable strings")

(check-equal?
  (s= (pw /iw mstr1) (pw /iw mstr2))
  (known /nothing)
  "Path-related info smoosh fails on non-`eq?` mutable strings")


(check-equal?
  (s= mbytes1 mbytes1)
  (known /just /known mbytes1)
  "Smoosh works on `eq?` mutable byte strings")

(check-equal?
  (s= mbytes1 mbytes2)
  (known /nothing)
  "Smoosh fails on non-`eq?` mutable byte strings")

(check-equal?
  (sj mbytes1 mbytes1)
  (known /just /known mbytes1)
  "Smoosh join works on `eq?` mutable byte strings")

(check-pred
  unknown?
  (sj mbytes1 mbytes2)
  "Smoosh join is unknown on non-`eq?` mutable byte strings")

(check-equal?
  (sm mbytes1 mbytes1)
  (known /just /known mbytes1)
  "Smoosh meet works on `eq?` mutable byte strings")

(check-pred
  unknown?
  (sm mbytes1 mbytes2)
  "Smoosh meet is unknown on non-`eq?` mutable byte strings")

(check-equal?
  (s= (pw mbytes1) (pw mbytes1))
  (known /just /known /pw mbytes1)
  "Path-related smoosh works on `eq?` mutable byte strings")

(check-pred
  unknown?
  (s= (pw mbytes1) (pw mbytes2))
  "Path-related smoosh is unknown on non-`eq?` mutable byte strings")

(check-equal?
  (s= (iw mbytes1) (iw mbytes1))
  (known /just /known /iw mbytes1)
  "Info smoosh works on `eq?` mutable byte strings")

(check-equal?
  (s= (iw mbytes1) (iw mbytes2))
  (known /nothing)
  "Info smoosh fails on non-`eq?` mutable byte strings")

(check-equal?
  (sj (iw mbytes1) (iw mbytes1))
  (known /just /known /iw mbytes1)
  "Info smoosh join works on `eq?` mutable byte strings")

(check-equal?
  (sj (iw mbytes1) (iw mbytes2))
  (known /nothing)
  "Info smoosh join fails on non-`eq?` mutable byte strings")

(check-equal?
  (sm (iw mbytes1) (iw mbytes1))
  (known /just /known /iw mbytes1)
  "Info smoosh meet works on `eq?` mutable byte strings")

(check-equal?
  (sm (iw mbytes1) (iw mbytes2))
  (known /nothing)
  "Info smoosh meet fails on non-`eq?` mutable byte strings")

(check-equal?
  (s= (pw /iw mbytes1) (pw /iw mbytes1))
  (known /just /known /pw /iw mbytes1)
  "Path-related info smoosh works on `eq?` mutable byte strings")

(check-equal?
  (s= (pw /iw mbytes1) (pw /iw mbytes2))
  (known /nothing)
  "Path-related info smoosh fails on non-`eq?` mutable byte strings")


(check-equal?
  (s= mbox1 mbox1)
  (known /just /known mbox1)
  "Smoosh works on `equal-always?` mutable boxes")

(check-eq?
  (known-value /just-value /known-value /s= mbox1 mbox1)
  mbox1
  "Smoosh preserves `eq?` on `equal-always?` mutable boxes")

(check-equal?
  (s= mbox1-chap mbox1)
  (known /just /known mbox1-chap)
  "Smoosh works on `equal-always?` mutable boxes even when they're not `eq?`")

(check-eq?
  (known-value /just-value /known-value /s= mbox1-chap mbox1)
  mbox1-chap
  "Smoosh preserves `eq?` on `equal-always?` mutable boxes even when they're not `eq?`")

(check-equal?
  (s= mbox1 mbox2)
  (known /nothing)
  "Smoosh fails on non-`equal-always?` mutable boxes")

(check-equal?
  (sj mbox1 mbox1)
  (known /just /known mbox1)
  "Smoosh join works on `equal-always?` mutable boxes")

(check-eq?
  (known-value /just-value /known-value /sj mbox1 mbox1)
  mbox1
  "Smoosh join preserves `eq?` on `equal-always?` mutable boxes")

(check-equal?
  (sj mbox1-chap mbox1)
  (known /just /known mbox1-chap)
  "Smoosh join works on `equal-always?` mutable boxes even when they're not `eq?`")

(check-eq?
  (known-value /just-value /known-value /sj mbox1-chap mbox1)
  mbox1-chap
  "Smoosh join preserves `eq?` on `equal-always?` mutable boxes even when they're not `eq?`")

(check-pred
  unknown?
  (sj mbox1 mbox2)
  "Smoosh join is unknown on non-`equal-always?` mutable boxes")

(check-equal?
  (sm mbox1 mbox1)
  (known /just /known mbox1)
  "Smoosh meet works on `equal-always?` mutable boxes")

(check-eq?
  (known-value /just-value /known-value /sm mbox1 mbox1)
  mbox1
  "Smoosh meet preserves `eq?` on `equal-always?` mutable boxes")

(check-equal?
  (sm mbox1-chap mbox1)
  (known /just /known mbox1-chap)
  "Smoosh meet works on `equal-always?` mutable boxes even when they're not `eq?`")

(check-eq?
  (known-value /just-value /known-value /sm mbox1-chap mbox1)
  mbox1-chap
  "Smoosh meet preserves `eq?` on `equal-always?` mutable boxes even when they're not `eq?`")

(check-pred
  unknown?
  (sm mbox1 mbox2)
  "Smoosh meet is unknown on non-`equal-always?` mutable boxes")

(check-equal?
  (s= (pw mbox1) (pw mbox1))
  (known /just /known /pw mbox1)
  "Path-related smoosh works on `equal-always?` mutable boxes")

(check-eq?
  (path-related-wrapper-value /known-value /just-value /known-value
    (s= (pw mbox1) (pw mbox1)))
  mbox1
  "Path-related smoosh preserves `eq?` on `equal-always?` mutable boxes")

(check-equal?
  (s= (pw mbox1-chap) (pw mbox1))
  (known /just /known /pw mbox1-chap)
  "Path-related smoosh works on `equal-always?` mutable boxes even when they're not `eq?`")

(check-eq?
  (path-related-wrapper-value /known-value /just-value /known-value
    (s= (pw mbox1-chap) (pw mbox1)))
  mbox1-chap
  "Path-related smoosh preserves `eq?` on `equal-always?` mutable boxes even when they're not `eq?`")

(check-pred
  unknown?
  (s= (pw mbox1) (pw mbox2))
  "Path-related smoosh is unknown on non-`equal-always?` mutable boxes")

(check-equal?
  (s= (iw mbox1) (iw mbox1))
  (known /just /known /iw mbox1)
  "Info smoosh works on `eq?` mutable boxes")

(check-eq?
  (info-wrapper-value /known-value /just-value /known-value
    (s= (iw mbox1) (iw mbox1)))
  mbox1
  "Info smoosh preserves `eq?` on `eq?` mutable boxes")

(check-equal?
  (s= (iw mbox1) (iw mbox2))
  (known /nothing)
  "Info smoosh fails on non-`equal-always?` mutable boxes")

(check-pred
  unknown?
  (s= (iw mbox1-chap-chap) (iw mbox1-chap))
  "Info smoosh is unknown on non-`eq?` mutable boxes even when they're `chaperone-of?` in one direction")

(check-pred
  unknown?
  (s= (iw mbox1-chap) (iw mbox1-chap2))
  "Info smoosh is unknown on non-`eq?` mutable boxes even when they're `equal-always?`")

(check-equal?
  (sj (iw mbox1) (iw mbox1))
  (known /just /known /iw mbox1)
  "Info smoosh join works on `eq?` mutable boxes")

(check-eq?
  (info-wrapper-value /known-value /just-value /known-value
    (sj (iw mbox1) (iw mbox1)))
  mbox1
  "Info smoosh join preserves `eq?` on `eq?` mutable boxes")

(check-equal?
  (sj (iw mbox1-chap-chap) (iw mbox1-chap))
  (known /just /known /iw mbox1-chap-chap)
  "Info smoosh join works on `chaperone-of?` mutable boxes even when they're not `chaperone=?`")

(check-eq?
  (info-wrapper-value /known-value /just-value /known-value
    (sj (iw mbox1-chap-chap) (iw mbox1-chap)))
  mbox1-chap-chap
  "Info smoosh join preserves `eq?` on `chaperone-of?` mutable boxes even when they're not `chaperone=?`")

(check-equal?
  (sj (iw mbox1) (iw mbox2))
  (known /nothing)
  "Info smoosh join fails on non-`eq?` mutable boxes")

(check-pred
  unknown?
  (sj (iw mbox1-chap) (iw mbox1-chap2))
  "Info smoosh join is unknown on non-`eq?` mutable boxes even when they're `equal-always?`")

(check-equal?
  (sm (iw mbox1) (iw mbox1))
  (known /just /known /iw mbox1)
  "Info smoosh meet works on `eq?` mutable boxes")

(check-eq?
  (info-wrapper-value /known-value /just-value /known-value
    (sm (iw mbox1) (iw mbox1)))
  mbox1
  "Info smoosh meet preserves `eq?` on `eq?` mutable boxes")

(check-equal?
  (sm (iw mbox1-chap-chap) (iw mbox1-chap))
  (known /just /known /iw mbox1-chap)
  "Info smoosh meet works on `chaperone-of?` mutable boxes even when they're not `chaperone=?`")

(check-eq?
  (info-wrapper-value /known-value /just-value /known-value
    (sm (iw mbox1-chap-chap) (iw mbox1-chap)))
  mbox1-chap
  "Info smoosh meet preserves `eq?` on `chaperone-of?` mutable boxes even when they're not `chaperone=?`")

(check-equal?
  (sm (iw mbox1) (iw mbox2))
  (known /nothing)
  "Info smoosh meet fails on non-`eq?` mutable boxes")

(check-pred
  unknown?
  (sm (iw mbox1-chap) (iw mbox1-chap2))
  "Info smoosh meet is unknown on non-`eq?` mutable boxes even when they're `equal-always?`")

(check-equal?
  (s= (pw /iw mbox1) (pw /iw mbox1))
  (known /just /known /pw /iw mbox1)
  "Path-related info smoosh works on `equal-always?` mutable boxes")

(check-eq?
  (info-wrapper-value /path-related-wrapper-value
    (known-value /just-value /known-value
      (s= (pw /iw mbox1) (pw /iw mbox1))))
  mbox1
  "Path-related info smoosh preserves `eq?` on `equal-always?` mutable boxes")

(check-equal?
  (s= (pw /iw mbox1-chap-chap) (pw /iw mbox1-chap))
  (known /just /known /pw /iw mbox1-chap-chap)
  "Path-related info smoosh works on `equal-always?` mutable boxes even when they're not `eq?`")

(check-eq?
  (info-wrapper-value /path-related-wrapper-value
    (known-value /just-value /known-value
      (s= (pw /iw mbox1-chap-chap) (pw /iw mbox1-chap))))
  mbox1-chap-chap
  "Path-related info smoosh preserves `eq?` on `equal-always?` mutable boxes even when they're not `eq?`")

(check-equal?
  (s= (pw /iw mbox1-chap) (pw /iw mbox1-chap2))
  (known /just /known /pw /iw mbox1-chap)
  "Path-related info smoosh works on `equal-always?` mutable boxes even when they're not `chaperone=?`")

(check-eq?
  (info-wrapper-value /path-related-wrapper-value
    (known-value /just-value /known-value
      (s= (pw /iw mbox1-chap) (pw /iw mbox1-chap2))))
  mbox1-chap
  "Path-related info smoosh preserves `eq?` on `equal-always?` mutable boxes even when they're not `chaperone=?`")

(check-equal?
  (s= (pw /iw mbox1) (pw /iw mbox2))
  (known /nothing)
  "Path-related info smoosh fails on non-`equal-always?` mutable boxes")


(check-equal?
  (s= mv1 mv1)
  (known /just /known mv1)
  "Smoosh works on `equal-always?` mutable vectors")

(check-eq?
  (known-value /just-value /known-value /s= mv1 mv1)
  mv1
  "Smoosh preserves `eq?` on `equal-always?` mutable vectors")

(check-equal?
  (s= mv1-chap-with-prop mv1-with-prop)
  (known /just /known mv1-chap-with-prop)
  "Smoosh works on `equal-always?` mutable vectors even when they're not `eq?`")

(check-eq?
  (known-value /just-value /known-value
    (s= mv1-chap-with-prop mv1-with-prop))
  mv1-chap-with-prop
  "Smoosh preserves `eq?` on `equal-always?` mutable vectors even when they're not `eq?`")

(check-equal?
  (s= mv1 mv2)
  (known /nothing)
  "Smoosh fails on non-`equal-always?` mutable vectors")

(check-equal?
  (sj mv1 mv1)
  (known /just /known mv1)
  "Smoosh join works on `equal-always?` mutable vectors")

(check-eq?
  (known-value /just-value /known-value /sj mv1 mv1)
  mv1
  "Smoosh join preserves `eq?` on `equal-always?` mutable vectors")

(check-equal?
  (sj mv1-chap-with-prop mv1-with-prop)
  (known /just /known mv1-chap-with-prop)
  "Smoosh join works on `equal-always?` mutable vectors even when they're not `eq?`")

(check-eq?
  (known-value /just-value /known-value
    (sj mv1-chap-with-prop mv1-with-prop))
  mv1-chap-with-prop
  "Smoosh join preserves `eq?` on `equal-always?` mutable vectors even when they're not `eq?`")

(check-pred
  unknown?
  (sj mv1 mv2)
  "Smoosh join is unknown on non-`equal-always?` mutable vectors")

(check-equal?
  (sm mv1 mv1)
  (known /just /known mv1)
  "Smoosh meet works on `equal-always?` mutable vectors")

(check-eq?
  (known-value /just-value /known-value /sm mv1 mv1)
  mv1
  "Smoosh meet preserves `eq?` on `equal-always?` mutable vectors")

(check-equal?
  (sm mv1-chap-with-prop mv1-with-prop)
  (known /just /known mv1-chap-with-prop)
  "Smoosh meet works on `equal-always?` mutable vectors even when they're not `eq?`")

(check-eq?
  (known-value /just-value /known-value
    (sm mv1-chap-with-prop mv1-with-prop))
  mv1-chap-with-prop
  "Smoosh meet preserves `eq?` on `equal-always?` mutable vectors even when they're not `eq?`")

(check-pred
  unknown?
  (sm mv1 mv2)
  "Smoosh meet is unknown on non-`equal-always?` mutable vectors")

(check-equal?
  (s= (pw mv1) (pw mv1))
  (known /just /known /pw mv1)
  "Path-related smoosh works on `equal-always?` mutable vectors")

(check-eq?
  (path-related-wrapper-value /known-value /just-value /known-value
    (s= (pw mv1) (pw mv1)))
  mv1
  "Path-related smoosh preserves `eq?` on `equal-always?` mutable vectors")

(check-equal?
  (s= (pw mv1-chap-with-prop) (pw mv1-with-prop))
  (known /just /known /pw mv1-chap-with-prop)
  "Path-related smoosh works on `equal-always?` mutable vectors even when they're not `eq?`")

(check-eq?
  (path-related-wrapper-value /known-value /just-value /known-value
    (s= (pw mv1-chap-with-prop) (pw mv1-with-prop)))
  mv1-chap-with-prop
  "Path-related smoosh preserves `eq?` on `equal-always?` mutable vectors even when they're not `eq?`")

(check-pred
  unknown?
  (s= (pw mv1) (pw mv2))
  "Path-related smoosh is unknown on non-`equal-always?` mutable vectors")

(check-equal?
  (s= (iw mv1) (iw mv1))
  (known /just /known /iw mv1)
  "Info smoosh works on `chaperone=?` mutable vectors")

(check-eq?
  (info-wrapper-value /known-value /just-value /known-value
    (s= (iw mv1) (iw mv1)))
  mv1
  "Info smoosh preserves `eq?` on `chaperone=?` mutable vectors")

(check-equal?
  (s= (iw mv1) (iw mv2))
  (known /nothing)
  "Info smoosh fails on non-`equal-always?` mutable vectors")

(check-pred
  unknown?
  (s= (iw mv1-chap-chap) (iw mv1-chap-with-prop))
  "Info smoosh is unknown on non-`chaperone=?` mutable vectors even when they're `chaperone-of?` in one direction")

(check-pred
  unknown?
  (s= (iw mv1-chap) (iw mv1-chap2))
  "Info smoosh is unknown on non-`chaperone=?` mutable vectors even when they're `equal-always?`")

(check-equal?
  (sj (iw mv1) (iw mv1))
  (known /just /known /iw mv1)
  "Info smoosh join works on `chaperone=?` mutable vectors")

(check-eq?
  (info-wrapper-value /known-value /just-value /known-value
    (sj (iw mv1) (iw mv1)))
  mv1
  "Info smoosh join preserves `eq?` on `chaperone=?` mutable vectors")

(check-equal?
  (sj (iw mv1-chap-chap) (iw mv1-chap-with-prop))
  (known /just /known /iw mv1-chap-chap)
  "Info smoosh join works on `chaperone-of?` mutable vectors even when they're not `chaperone=?`")

(check-eq?
  (info-wrapper-value /known-value /just-value /known-value
    (sj (iw mv1-chap-chap) (iw mv1-chap-with-prop)))
  mv1-chap-chap
  "Info smoosh join preserves `eq?` on `chaperone-of?` mutable vectors even when they're not `chaperone=?`")

(check-equal?
  (sj (iw mv1) (iw mv2))
  (known /nothing)
  "Info smoosh join fails on non-`equal-always?` mutable vectors")

(check-pred
  unknown?
  (sj (iw mv1-chap) (iw mv1-chap2))
  "Info smoosh join is unknown on non-`chaperone-of?` mutable vectors even when they're `equal-always?`")

(check-equal?
  (sm (iw mv1) (iw mv1))
  (known /just /known /iw mv1)
  "Info smoosh meet works on `chaperone=?` mutable vectors")

(check-eq?
  (info-wrapper-value /known-value /just-value /known-value
    (sm (iw mv1) (iw mv1)))
  mv1
  "Info smoosh meet preserves `eq?` on `chaperone=?` mutable vectors")

(check-equal?
  (sm (iw mv1-chap-chap) (iw mv1-chap-with-prop))
  (known /just /known /iw mv1-chap-with-prop)
  "Info smoosh meet works on `chaperone-of?` mutable vectors even when they're not `chaperone=?`")

(check-eq?
  (info-wrapper-value /known-value /just-value /known-value
    (sm (iw mv1-chap-chap) (iw mv1-chap-with-prop)))
  mv1-chap-with-prop
  "Info smoosh meet preserves `eq?` on `chaperone-of?` mutable vectors even when they're not `chaperone=?`")

(check-equal?
  (sm (iw mv1) (iw mv2))
  (known /nothing)
  "Info smoosh meet fails on non-`equal-always?` mutable vectors")

(check-pred
  unknown?
  (sm (iw mv1-chap) (iw mv1-chap2))
  "Info smoosh meet is unknown on non-`chaperone-of?` mutable vectors even when they're `equal-always?`")

(check-equal?
  (s= (pw /iw mv1) (pw /iw mv1))
  (known /just /known /pw /iw mv1)
  "Path-related info smoosh works on `equal-always?` mutable vectors")

(check-eq?
  (info-wrapper-value /path-related-wrapper-value
    (known-value /just-value /known-value
      (s= (pw /iw mv1) (pw /iw mv1))))
  mv1
  "Path-related info smoosh preserves `eq?` on `equal-always?` mutable vectors")

(check-equal?
  (s= (pw /iw mv1-chap-chap) (pw /iw mv1-chap-with-prop))
  (known /just /known /pw /iw mv1-chap-chap)
  "Path-related info smoosh works on `equal-always?` mutable vectors even when they're not `eq?`")

(check-eq?
  (info-wrapper-value /path-related-wrapper-value
    (known-value /just-value /known-value
      (s= (pw /iw mv1-chap-chap) (pw /iw mv1-chap-with-prop))))
  mv1-chap-chap
  "Path-related info smoosh preserves `eq?` on `equal-always?` mutable vectors even when they're not `eq?`")

(check-equal?
  (s= (pw /iw mv1-chap) (pw /iw mv1-chap2))
  (known /just /known /pw /iw mv1-chap)
  "Path-related info smoosh works on `equal-always?` mutable vectors even when they're not `chaperone=?`")

(check-eq?
  (info-wrapper-value /path-related-wrapper-value
    (known-value /just-value /known-value
      (s= (pw /iw mv1-chap) (pw /iw mv1-chap2))))
  mv1-chap
  "Path-related info smoosh preserves `eq?` on `equal-always?` mutable vectors even when they're not `chaperone=?`")

(check-equal?
  (s= (pw /iw mv1) (pw /iw mv2))
  (known /nothing)
  "Path-related info smoosh fails on non-`equal-always?` mutable vectors")


(check-equal?
  (s= mprefab1 mprefab1)
  (known /just /known mprefab1)
  "Smoosh works on `equal-always?` mutable prefab structs")

(check-eq?
  (known-value /just-value /known-value /s= mprefab1 mprefab1)
  mprefab1
  "Smoosh preserves `eq?` on `equal-always?` mutable prefab structs")

(check-equal?
  (s= mprefab1-chap-with-prop mprefab1-with-prop)
  (known /just /known mprefab1-chap-with-prop)
  "Smoosh works on `equal-always?` mutable prefab structs even when they're not `eq?`")

(check-eq?
  (known-value /just-value /known-value
    (s= mprefab1-chap-with-prop mprefab1-with-prop))
  mprefab1-chap-with-prop
  "Smoosh preserves `eq?` on `equal-always?` mutable prefab structs even when they're not `eq?`")

(check-equal?
  (s= mprefab1 mprefab2)
  (known /nothing)
  "Smoosh fails on non-`equal-always?` mutable prefab structs")

(check-equal?
  (sj mprefab1 mprefab1)
  (known /just /known mprefab1)
  "Smoosh join works on `equal-always?` mutable prefab structs")

(check-eq?
  (known-value /just-value /known-value /sj mprefab1 mprefab1)
  mprefab1
  "Smoosh join preserves `eq?` on `equal-always?` mutable prefab structs")

(check-equal?
  (sj mprefab1-chap-with-prop mprefab1-with-prop)
  (known /just /known mprefab1-chap-with-prop)
  "Smoosh join works on `equal-always?` mutable prefab structs even when they're not `eq?`")

(check-eq?
  (known-value /just-value /known-value
    (sj mprefab1-chap-with-prop mprefab1-with-prop))
  mprefab1-chap-with-prop
  "Smoosh join preserves `eq?` on `equal-always?` mutable prefab structs even when they're not `eq?`")

(check-pred
  unknown?
  (sj mprefab1 mprefab2)
  "Smoosh join is unknown on non-`equal-always?` mutable prefab structs")

(check-equal?
  (sm mprefab1 mprefab1)
  (known /just /known mprefab1)
  "Smoosh meet works on `equal-always?` mutable prefab structs")

(check-eq?
  (known-value /just-value /known-value /sm mprefab1 mprefab1)
  mprefab1
  "Smoosh meet preserves `eq?` on `equal-always?` mutable prefab structs")

(check-equal?
  (sm mprefab1-chap-with-prop mprefab1-with-prop)
  (known /just /known mprefab1-chap-with-prop)
  "Smoosh meet works on `equal-always?` mutable prefab structs even when they're not `eq?`")

(check-eq?
  (known-value /just-value /known-value
    (sm mprefab1-chap-with-prop mprefab1-with-prop))
  mprefab1-chap-with-prop
  "Smoosh meet preserves `eq?` on `equal-always?` mutable prefab structs even when they're not `eq?`")

(check-pred
  unknown?
  (sm mprefab1 mprefab2)
  "Smoosh meet is unknown on non-`equal-always?` mutable prefab structs")

(check-equal?
  (s= (pw mprefab1) (pw mprefab1))
  (known /just /known /pw mprefab1)
  "Path-related smoosh works on `equal-always?` mutable prefab structs")

(check-eq?
  (path-related-wrapper-value /known-value /just-value /known-value
    (s= (pw mprefab1) (pw mprefab1)))
  mprefab1
  "Path-related smoosh preserves `eq?` on `equal-always?` mutable prefab structs")

(check-equal?
  (s= (pw mprefab1-chap-with-prop) (pw mprefab1-with-prop))
  (known /just /known /pw mprefab1-chap-with-prop)
  "Path-related smoosh works on `equal-always?` mutable prefab structs even when they're not `eq?`")

(check-eq?
  (path-related-wrapper-value /known-value /just-value /known-value
    (s= (pw mprefab1-chap-with-prop) (pw mprefab1-with-prop)))
  mprefab1-chap-with-prop
  "Path-related smoosh preserves `eq?` on `equal-always?` mutable prefab structs even when they're not `eq?`")

(check-pred
  unknown?
  (s= (pw mprefab1) (pw mprefab2))
  "Path-related smoosh is unknown on non-`equal-always?` mutable prefab structs")

(check-equal?
  (s= (iw mprefab1) (iw mprefab1))
  (known /just /known /iw mprefab1)
  "Info smoosh works on `chaperone=?` mutable prefab structs")

(check-eq?
  (info-wrapper-value /known-value /just-value /known-value
    (s= (iw mprefab1) (iw mprefab1)))
  mprefab1
  "Info smoosh preserves `eq?` on `chaperone=?` mutable prefab structs")

(check-equal?
  (s= (iw mprefab1) (iw mprefab2))
  (known /nothing)
  "Info smoosh fails on non-`equal-always?` mutable prefab structs")

(check-pred
  unknown?
  (s= (iw mprefab1-chap-chap) (iw mprefab1-chap-with-prop))
  "Info smoosh is unknown on non-`chaperone=?` mutable prefab structs even when they're `chaperone-of?` in one direction")

(check-pred
  unknown?
  (s= (iw mprefab1-chap) (iw mprefab1-chap2))
  "Info smoosh is unknown on non-`chaperone=?` mutable prefab structs even when they're `equal-always?`")

(check-equal?
  (sj (iw mprefab1) (iw mprefab1))
  (known /just /known /iw mprefab1)
  "Info smoosh join works on `chaperone=?` mutable prefab structs")

(check-eq?
  (info-wrapper-value /known-value /just-value /known-value
    (sj (iw mprefab1) (iw mprefab1)))
  mprefab1
  "Info smoosh join preserves `eq?` on `chaperone=?` mutable prefab structs")

(check-equal?
  (sj (iw mprefab1-chap-chap) (iw mprefab1-chap-with-prop))
  (known /just /known /iw mprefab1-chap-chap)
  "Info smoosh join works on `chaperone-of?` mutable prefab structs even when they're not `chaperone=?`")

(check-eq?
  (info-wrapper-value /known-value /just-value /known-value
    (sj (iw mprefab1-chap-chap) (iw mprefab1-chap-with-prop)))
  mprefab1-chap-chap
  "Info smoosh join preserves `eq?` on `chaperone-of?` mutable prefab structs even when they're not `chaperone=?`")

(check-equal?
  (sj (iw mprefab1) (iw mprefab2))
  (known /nothing)
  "Info smoosh join fails on non-`equal-always?` mutable prefab structs")

(check-pred
  unknown?
  (sj (iw mprefab1-chap) (iw mprefab1-chap2))
  "Info smoosh join is unknown on non-`chaperone-of?` mutable prefab structs even when they're `equal-always?`")

(check-equal?
  (sm (iw mprefab1) (iw mprefab1))
  (known /just /known /iw mprefab1)
  "Info smoosh meet works on `chaperone=?` mutable prefab structs")

(check-eq?
  (info-wrapper-value /known-value /just-value /known-value
    (sm (iw mprefab1) (iw mprefab1)))
  mprefab1
  "Info smoosh meet preserves `eq?` on `chaperone=?` mutable prefab structs")

(check-equal?
  (sm (iw mprefab1-chap-chap) (iw mprefab1-chap-with-prop))
  (known /just /known /iw mprefab1-chap-with-prop)
  "Info smoosh meet works on `chaperone-of?` mutable prefab structs even when they're not `chaperone=?`")

(check-eq?
  (info-wrapper-value /known-value /just-value /known-value
    (sm (iw mprefab1-chap-chap) (iw mprefab1-chap-with-prop)))
  mprefab1-chap-with-prop
  "Info smoosh meet preserves `eq?` on `chaperone-of?` mutable prefab structs even when they're not `chaperone=?`")

(check-equal?
  (sm (iw mprefab1) (iw mprefab2))
  (known /nothing)
  "Info smoosh meet fails on non-`equal-always?` mutable prefab structs")

(check-pred
  unknown?
  (sm (iw mprefab1-chap) (iw mprefab1-chap2))
  "Info smoosh meet is unknown on non-`chaperone-of?` mutable prefab structs even when they're `equal-always?`")

(check-equal?
  (s= (pw /iw mprefab1) (pw /iw mprefab1))
  (known /just /known /pw /iw mprefab1)
  "Path-related info smoosh works on `equal-always?` mutable prefab structs")

(check-eq?
  (info-wrapper-value /path-related-wrapper-value
    (known-value /just-value /known-value
      (s= (pw /iw mprefab1) (pw /iw mprefab1))))
  mprefab1
  "Path-related info smoosh preserves `eq?` on `equal-always?` mutable prefab structs")

(check-equal?
  (s= (pw /iw mprefab1-chap-chap) (pw /iw mprefab1-chap-with-prop))
  (known /just /known /pw /iw mprefab1-chap-chap)
  "Path-related info smoosh works on `equal-always?` mutable prefab structs even when they're not `eq?`")

(check-eq?
  (info-wrapper-value /path-related-wrapper-value
    (known-value /just-value /known-value
      (s=
        (pw /iw mprefab1-chap-chap)
        (pw /iw mprefab1-chap-with-prop))))
  mprefab1-chap-chap
  "Path-related info smoosh preserves `eq?` on `equal-always?` mutable prefab structs even when they're not `eq?`")

(check-equal?
  (s= (pw /iw mprefab1-chap) (pw /iw mprefab1-chap2))
  (known /just /known /pw /iw mprefab1-chap)
  "Path-related info smoosh works on `equal-always?` mutable prefab structs even when they're not `chaperone=?`")

(check-eq?
  (info-wrapper-value /path-related-wrapper-value
    (known-value /just-value /known-value
      (s= (pw /iw mprefab1-chap) (pw /iw mprefab1-chap2))))
  mprefab1-chap
  "Path-related info smoosh preserves `eq?` on `equal-always?` mutable prefab structs even when they're not `chaperone=?`")

(check-equal?
  (s= (pw /iw mprefab1) (pw /iw mprefab2))
  (known /nothing)
  "Path-related info smoosh fails on non-`equal-always?` mutable prefab structs")


(check-equal?
  (s= mhash1 mhash1)
  (known /just /known mhash1)
  "Smoosh works on `equal-always?` mutable hash tables")

(check-eq?
  (known-value /just-value /known-value /s= mhash1 mhash1)
  mhash1
  "Smoosh preserves `eq?` on `equal-always?` mutable hash tables")

(check-equal?
  (s= mhash1-chap mhash1)
  (known /just /known mhash1-chap)
  "Smoosh works on `equal-always?` mutable hash tables even when they're not `eq?`")

(check-eq?
  (known-value /just-value /known-value /s= mhash1-chap mhash1)
  mhash1-chap
  "Smoosh preserves `eq?` on `equal-always?` mutable hash tables even when they're not `eq?`")

(check-equal?
  (s= mhash1 mhash2)
  (known /nothing)
  "Smoosh fails on non-`equal-always?` mutable hash tables")

(check-equal?
  (sj mhash1 mhash1)
  (known /just /known mhash1)
  "Smoosh join works on `equal-always?` mutable hash tables")

(check-eq?
  (known-value /just-value /known-value /sj mhash1 mhash1)
  mhash1
  "Smoosh join preserves `eq?` on `equal-always?` mutable hash tables")

(check-equal?
  (sj mhash1-chap mhash1)
  (known /just /known mhash1-chap)
  "Smoosh join works on `equal-always?` mutable hash tables even when they're not `eq?`")

(check-eq?
  (known-value /just-value /known-value /sj mhash1-chap mhash1)
  mhash1-chap
  "Smoosh join preserves `eq?` on `equal-always?` mutable hash tables even when they're not `eq?`")

(check-pred
  unknown?
  (sj mhash1 mhash2)
  "Smoosh join is unknown on non-`equal-always?` mutable hash tables")

(check-equal?
  (sm mhash1 mhash1)
  (known /just /known mhash1)
  "Smoosh meet works on `equal-always?` mutable hash tables")

(check-eq?
  (known-value /just-value /known-value /sm mhash1 mhash1)
  mhash1
  "Smoosh meet preserves `eq?` on `equal-always?` mutable hash tables")

(check-equal?
  (sm mhash1-chap mhash1)
  (known /just /known mhash1-chap)
  "Smoosh meet works on `equal-always?` mutable hash tables even when they're not `eq?`")

(check-eq?
  (known-value /just-value /known-value /sm mhash1-chap mhash1)
  mhash1-chap
  "Smoosh meet preserves `eq?` on `equal-always?` mutable hash tables even when they're not `eq?`")

(check-pred
  unknown?
  (sm mhash1 mhash2)
  "Smoosh meet is unknown on non-`equal-always?` mutable hash tables")

(check-equal?
  (s= (pw mhash1) (pw mhash1))
  (known /just /known /pw mhash1)
  "Path-related smoosh works on `equal-always?` mutable hash tables")

(check-eq?
  (path-related-wrapper-value /known-value /just-value /known-value
    (s= (pw mhash1) (pw mhash1)))
  mhash1
  "Path-related smoosh preserves `eq?` on `equal-always?` mutable hash tables")

(check-equal?
  (s= (pw mhash1-chap) (pw mhash1))
  (known /just /known /pw mhash1-chap)
  "Path-related smoosh works on `equal-always?` mutable hash tables even when they're not `eq?`")

(check-eq?
  (path-related-wrapper-value /known-value /just-value /known-value
    (s= (pw mhash1-chap) (pw mhash1)))
  mhash1-chap
  "Path-related smoosh preserves `eq?` on `equal-always?` mutable hash tables even when they're not `eq?`")

(check-pred
  unknown?
  (s= (pw mhash1) (pw mhash2))
  "Path-related smoosh is unknown on non-`equal-always?` mutable hash tables")

(check-equal?
  (s= (iw mhash1) (iw mhash1))
  (known /just /known /iw mhash1)
  "Info smoosh works on `eq?` mutable hash tables")

(check-eq?
  (info-wrapper-value /known-value /just-value /known-value
    (s= (iw mhash1) (iw mhash1)))
  mhash1
  "Info smoosh preserves `eq?` on `eq?` mutable hash tables")

(check-equal?
  (s= (iw mhash1) (iw mhash2))
  (known /nothing)
  "Info smoosh fails on non-`equal-always?` mutable hash tables")

(check-pred
  unknown?
  (s= (iw mhash1-chap-chap) (iw mhash1-chap))
  "Info smoosh is unknown on non-`eq?` mutable hash tables even when they're `chaperone-of?` in one direction")

(check-pred
  unknown?
  (s= (iw mhash1-chap) (iw mhash1-chap2))
  "Info smoosh is unknown on non-`eq?` mutable hash tables even when they're `equal-always?`")

(check-equal?
  (sj (iw mhash1) (iw mhash1))
  (known /just /known /iw mhash1)
  "Info smoosh join works on `eq?` mutable hash tables")

(check-eq?
  (info-wrapper-value /known-value /just-value /known-value
    (sj (iw mhash1) (iw mhash1)))
  mhash1
  "Info smoosh join preserves `eq?` on `eq?` mutable hash tables")

(check-equal?
  (sj (iw mhash1-chap-chap) (iw mhash1-chap))
  (known /just /known /iw mhash1-chap-chap)
  "Info smoosh join works on `chaperone-of?` mutable hash tables even when they're not `chaperone=?`")

(check-eq?
  (info-wrapper-value /known-value /just-value /known-value
    (sj (iw mhash1-chap-chap) (iw mhash1-chap)))
  mhash1-chap-chap
  "Info smoosh join preserves `eq?` on `chaperone-of?` mutable hash tables even when they're not `chaperone=?`")

(check-equal?
  (sj (iw mhash1) (iw mhash2))
  (known /nothing)
  "Info smoosh join fails on non-`eq?` mutable hash tables")

(check-pred
  unknown?
  (sj (iw mhash1-chap) (iw mhash1-chap2))
  "Info smoosh join is unknown on non-`eq?` mutable hash tables even when they're `equal-always?`")

(check-equal?
  (sm (iw mhash1) (iw mhash1))
  (known /just /known /iw mhash1)
  "Info smoosh meet works on `eq?` mutable hash tables")

(check-eq?
  (info-wrapper-value /known-value /just-value /known-value
    (sm (iw mhash1) (iw mhash1)))
  mhash1
  "Info smoosh meet preserves `eq?` on `eq?` mutable hash tables")

(check-equal?
  (sm (iw mhash1-chap-chap) (iw mhash1-chap))
  (known /just /known /iw mhash1-chap)
  "Info smoosh meet works on `chaperone-of?` mutable hash tables even when they're not `chaperone=?`")

(check-eq?
  (info-wrapper-value /known-value /just-value /known-value
    (sm (iw mhash1-chap-chap) (iw mhash1-chap)))
  mhash1-chap
  "Info smoosh meet preserves `eq?` on `chaperone-of?` mutable hash tables even when they're not `chaperone=?`")

(check-equal?
  (sm (iw mhash1) (iw mhash2))
  (known /nothing)
  "Info smoosh meet fails on non-`eq?` mutable hash tables")

(check-pred
  unknown?
  (sm (iw mhash1-chap) (iw mhash1-chap2))
  "Info smoosh meet is unknown on non-`eq?` mutable hash tables even when they're `equal-always?`")

(check-equal?
  (s= (pw /iw mhash1) (pw /iw mhash1))
  (known /just /known /pw /iw mhash1)
  "Path-related info smoosh works on `equal-always?` mutable hash tables")

(check-eq?
  (info-wrapper-value /path-related-wrapper-value
    (known-value /just-value /known-value
      (s= (pw /iw mhash1) (pw /iw mhash1))))
  mhash1
  "Path-related info smoosh preserves `eq?` on `equal-always?` mutable hash tables")

(check-equal?
  (s= (pw /iw mhash1-chap-chap) (pw /iw mhash1-chap))
  (known /just /known /pw /iw mhash1-chap-chap)
  "Path-related info smoosh works on `equal-always?` mutable hash tables even when they're not `eq?`")

(check-eq?
  (info-wrapper-value /path-related-wrapper-value
    (known-value /just-value /known-value
      (s= (pw /iw mhash1-chap-chap) (pw /iw mhash1-chap))))
  mhash1-chap-chap
  "Path-related info smoosh preserves `eq?` on `equal-always?` mutable hash tables even when they're not `eq?`")

(check-equal?
  (s= (pw /iw mhash1-chap) (pw /iw mhash1-chap2))
  (known /just /known /pw /iw mhash1-chap)
  "Path-related info smoosh works on `equal-always?` mutable hash tables even when they're not `chaperone=?`")

(check-eq?
  (info-wrapper-value /path-related-wrapper-value
    (known-value /just-value /known-value
      (s= (pw /iw mhash1-chap) (pw /iw mhash1-chap2))))
  mhash1-chap
  "Path-related info smoosh preserves `eq?` on `equal-always?` mutable hash tables even when they're not `chaperone=?`")

(check-equal?
  (s= (pw /iw mhash1) (pw /iw mhash2))
  (known /nothing)
  "Path-related info smoosh fails on non-`equal-always?` mutable hash tables")


(check-equal?
  (s= flv1 flv1)
  (known /just /known flv1)
  "Smoosh works on `eq?` flvectors")

(check-equal?
  (s= flv1 flv2)
  (known /nothing)
  "Smoosh fails on non-`eq?` flvectors")

(check-equal?
  (sj flv1 flv1)
  (known /just /known flv1)
  "Smoosh join works on `eq?` flvectors")

(check-pred
  unknown?
  (sj flv1 flv2)
  "Smoosh join is unknown on non-`eq?` flvectors")

(check-equal?
  (sm flv1 flv1)
  (known /just /known flv1)
  "Smoosh meet works on `eq?` flvectors")

(check-pred
  unknown?
  (sm flv1 flv2)
  "Smoosh meet is unknown on non-`eq?` flvectors")

(check-equal?
  (s= (pw flv1) (pw flv1))
  (known /just /known /pw flv1)
  "Path-related smoosh works on `eq?` flvectors")

(check-pred
  unknown?
  (s= (pw flv1) (pw flv2))
  "Path-related smoosh is unknown on non-`eq?` flvectors")

(check-equal?
  (s= (iw flv1) (iw flv1))
  (known /just /known /iw flv1)
  "Info smoosh works on `eq?` flvectors")

(check-equal?
  (s= (iw flv1) (iw flv2))
  (known /nothing)
  "Info smoosh fails on non-`eq?` flvectors")

(check-equal?
  (sj (iw flv1) (iw flv1))
  (known /just /known /iw flv1)
  "Info smoosh join works on `eq?` flvectors")

(check-equal?
  (sj (iw flv1) (iw flv2))
  (known /nothing)
  "Info smoosh join fails on non-`eq?` flvectors")

(check-equal?
  (sm (iw flv1) (iw flv1))
  (known /just /known /iw flv1)
  "Info smoosh meet works on `eq?` flvectors")

(check-equal?
  (sm (iw flv1) (iw flv2))
  (known /nothing)
  "Info smoosh meet fails on non-`eq?` flvectors")

(check-equal?
  (s= (pw /iw flv1) (pw /iw flv1))
  (known /just /known /pw /iw flv1)
  "Path-related info smoosh works on `eq?` flvectors")

(check-equal?
  (s= (pw /iw flv1) (pw /iw flv2))
  (known /nothing)
  "Path-related info smoosh fails on non-`eq?` flvectors")


(check-equal?
  (s= fxv1 fxv1)
  (known /just /known fxv1)
  "Smoosh works on `eq?` fxvectors")

(check-equal?
  (s= fxv1 fxv2)
  (known /nothing)
  "Smoosh fails on non-`eq?` fxvectors")

(check-equal?
  (sj fxv1 fxv1)
  (known /just /known fxv1)
  "Smoosh join works on `eq?` fxvectors")

(check-pred
  unknown?
  (sj fxv1 fxv2)
  "Smoosh join is unknown on non-`eq?` fxvectors")

(check-equal?
  (sm fxv1 fxv1)
  (known /just /known fxv1)
  "Smoosh meet works on `eq?` fxvectors")

(check-pred
  unknown?
  (sm fxv1 fxv2)
  "Smoosh meet is unknown on non-`eq?` fxvectors")

(check-equal?
  (s= (pw fxv1) (pw fxv1))
  (known /just /known /pw fxv1)
  "Path-related smoosh works on `eq?` fxvectors")

(check-pred
  unknown?
  (s= (pw fxv1) (pw fxv2))
  "Path-related smoosh is unknown on non-`eq?` fxvectors")

(check-equal?
  (s= (iw fxv1) (iw fxv1))
  (known /just /known /iw fxv1)
  "Info smoosh works on `eq?` fxvectors")

(check-equal?
  (s= (iw fxv1) (iw fxv2))
  (known /nothing)
  "Info smoosh fails on non-`eq?` fxvectors")

(check-equal?
  (sj (iw fxv1) (iw fxv1))
  (known /just /known /iw fxv1)
  "Info smoosh join works on `eq?` fxvectors")

(check-equal?
  (sj (iw fxv1) (iw fxv2))
  (known /nothing)
  "Info smoosh join fails on non-`eq?` fxvectors")

(check-equal?
  (sm (iw fxv1) (iw fxv1))
  (known /just /known /iw fxv1)
  "Info smoosh meet works on `eq?` fxvectors")

(check-equal?
  (sm (iw fxv1) (iw fxv2))
  (known /nothing)
  "Info smoosh meet fails on non-`eq?` fxvectors")

(check-equal?
  (s= (pw /iw fxv1) (pw /iw fxv1))
  (known /just /known /pw /iw fxv1)
  "Path-related info smoosh works on `eq?` fxvectors")

(check-equal?
  (s= (pw /iw fxv1) (pw /iw fxv2))
  (known /nothing)
  "Path-related info smoosh fails on non-`eq?` fxvectors")


(check-equal?
  (s= 'a 'a)
  (known /just /known 'a)
  "Smoosh works on equal symbols")

(check-equal?
  (s= 'a 'b)
  (known /nothing)
  "Smoosh fails on unequal symbols")

(check-equal?
  (sj 'a 'a)
  (known /just /known 'a)
  "Smoosh join works on equal symbols")

(check-pred
  unknown?
  (sj 'a 'b)
  "Smoosh join is unknown on unequal symbols")

(check-equal?
  (sm 'a 'a)
  (known /just /known 'a)
  "Smoosh meet works on equal symbols")

(check-pred
  unknown?
  (sm 'a 'b)
  "Smoosh meet is unknown on unequal symbols")

(check-equal?
  (s= (pw 'a) (pw 'a))
  (known /just /known /pw 'a)
  "Path-related smoosh works on equal symbols")

(check-pred
  unknown?
  (s= (pw 'a) (pw 'b))
  "Path-related smoosh is unknown on unequal symbols")

(check-equal?
  (s= (iw 'a) (iw 'a))
  (known /just /known /iw 'a)
  "Info smoosh works on equal symbols")

(check-equal?
  (s= (iw 'a) (iw 'b))
  (known /nothing)
  "Info smoosh fails on unequal symbols")

(check-equal?
  (sj (iw 'a) (iw 'a))
  (known /just /known /iw 'a)
  "Info smoosh join works on equal symbols")

(check-equal?
  (sj (iw 'a) (iw 'b))
  (known /nothing)
  "Info smoosh join fails on unequal symbols")

(check-equal?
  (sm (iw 'a) (iw 'a))
  (known /just /known /iw 'a)
  "Info smoosh meet works on equal symbols")

(check-equal?
  (sm (iw 'a) (iw 'b))
  (known /nothing)
  "Info smoosh meet fails on unequal symbols")

(check-equal?
  (s= (pw /iw 'a) (pw /iw 'a))
  (known /just /known /pw /iw 'a)
  "Path-related info smoosh works on equal symbols")

(check-equal?
  (s= (pw /iw 'a) (pw /iw 'b))
  (known /nothing)
  "Path-related info smoosh fails on unequal symbols")


(check-equal?
  (s= '#:a '#:a)
  (known /just /known '#:a)
  "Smoosh works on equal keywords")

(check-equal?
  (s= '#:a '#:b)
  (known /nothing)
  "Smoosh fails on unequal keywords")

(check-equal?
  (sj '#:a '#:a)
  (known /just /known '#:a)
  "Smoosh join works on equal keywords")

(check-pred
  unknown?
  (sj '#:a '#:b)
  "Smoosh join is unknown on unequal keywords")

(check-equal?
  (sm '#:a '#:a)
  (known /just /known '#:a)
  "Smoosh meet works on equal keywords")

(check-pred
  unknown?
  (sm '#:a '#:b)
  "Smoosh meet is unknown on unequal keywords")

(check-equal?
  (s= (pw '#:a) (pw '#:a))
  (known /just /known /pw '#:a)
  "Path-related smoosh works on equal keywords")

(check-pred
  unknown?
  (s= (pw '#:a) (pw '#:b))
  "Path-related smoosh is unknown on unequal keywords")

(check-equal?
  (s= (iw '#:a) (iw '#:a))
  (known /just /known /iw '#:a)
  "Info smoosh works on equal keywords")

(check-equal?
  (s= (iw '#:a) (iw '#:b))
  (known /nothing)
  "Info smoosh fails on unequal keywords")

(check-equal?
  (sj (iw '#:a) (iw '#:a))
  (known /just /known /iw '#:a)
  "Info smoosh join works on equal keywords")

(check-equal?
  (sj (iw '#:a) (iw '#:b))
  (known /nothing)
  "Info smoosh join fails on unequal keywords")

(check-equal?
  (sm (iw '#:a) (iw '#:a))
  (known /just /known /iw '#:a)
  "Info smoosh meet works on equal keywords")

(check-equal?
  (sm (iw '#:a) (iw '#:b))
  (known /nothing)
  "Info smoosh meet fails on unequal keywords")

(check-equal?
  (s= (pw /iw '#:a) (pw /iw '#:a))
  (known /just /known /pw /iw '#:a)
  "Path-related info smoosh works on equal keywords")

(check-equal?
  (s= (pw /iw '#:a) (pw /iw '#:b))
  (known /nothing)
  "Path-related info smoosh fails on unequal keywords")


(check-equal?
  (s= (list) (list))
  (known /just /known /list)
  "Smoosh works on empty lists")

(check-equal?
  (sj (list) (list))
  (known /just /known /list)
  "Smoosh join works on empty lists")

(check-equal?
  (sm (list) (list))
  (known /just /known /list)
  "Smoosh meet works on empty lists")

(check-equal?
  (s= (pw /list) (pw /list))
  (known /just /known /pw /list)
  "Path-related smoosh works on empty lists")

(check-equal?
  (s= (iw /list) (iw /list))
  (known /just /known /iw /list)
  "Info smoosh works on empty lists")

(check-equal?
  (sj (iw /list) (iw /list))
  (known /just /known /iw /list)
  "Info smoosh join works on empty lists")

(check-equal?
  (sm (iw /list) (iw /list))
  (known /just /known /iw /list)
  "Info smoosh meet works on empty lists")

(check-equal?
  (s= (pw /iw /list) (pw /iw /list))
  (known /just /known /pw /iw /list)
  "Path-related info smoosh works on empty lists")


(check-equal?
  (s= (cons 0 0.0) (cons 0.0 0))
  (known /just /known /cons 0 0.0)
  "Smoosh works on equal cons cells")

(w- obj (cons 0 0.0)
  (check-eq?
    (known-value /just-value /known-value /s= obj (cons 0.0 0))
    obj
    "Smoosh preserves `eq?` when possible on equal cons cells"))

(check-equal?
  (s= (cons 0 0) (cons 1 0))
  (known /nothing)
  "Smoosh fails on unequal cons cells")

(check-equal?
  (sj (cons 0 0.0) (cons 0.0 0))
  (known /just /known /cons 0 0.0)
  "Smoosh join works on equal cons cells")

(w- obj (cons 0 0.0)
  (check-eq?
    (known-value /just-value /known-value /sj obj (cons 0.0 0))
    obj
    "Smoosh join preserves `eq?` when possible on equal cons cells"))

(check-equal?
  (sj (cons 1 0) (cons 0.0 1+0.0i))
  (known /just /known /cons 1 1+0.0i)
  "Smoosh join works on unequal, comparable cons cells")

(check-pred
  unknown?
  (sj (cons 0 0+i) (cons 0 1+i))
  "Smoosh join is unknown on unequal, uncomparable cons cells")

(check-equal?
  (sm (cons 0 0.0) (cons 0.0 0))
  (known /just /known /cons 0 0.0)
  "Smoosh meet works on equal cons cells")

(w- obj (cons 0 0.0)
  (check-eq?
    (known-value /just-value /known-value /sm obj (cons 0.0 0))
    obj
    "Smoosh meet preserves `eq?` when possible on equal cons cells"))

(check-equal?
  (sm (cons 1 0) (cons 0.0 1+0.0i))
  (known /just /known /cons 0.0 0)
  "Smoosh meet works on unequal, comparable cons cells")

(check-pred
  unknown?
  (sm (cons 0 0+i) (cons 0 1+i))
  "Smoosh meet is unknown on unequal, uncomparable cons cells")

(check-equal?
  (s= (pw /cons 0 0.0) (pw /cons 0.0 0))
  (known /just /known /pw /cons 0 0.0)
  "Path-related smoosh works on equal cons cells")

(w- obj (pw /cons 0 0.0)
  (check-eq?
    (known-value /just-value /known-value /s= obj (pw /cons 0.0 0))
    obj
    "Path-related smoosh preserves `eq?` when possible on equal cons cells"))

(check-equal?
  (s= (pw /cons 0 0.0) (pw /cons 1.0 1+0.0i))
  (known /just /known /pw /cons 0 0.0)
  "Path-related smoosh works on cons cells with path-related elements")

(w- obj (pw /cons 0 0.0)
  (check-eq?
    (known-value /just-value /known-value
      (s= obj (pw /cons 1.0 1+0.0i)))
    obj
    "Path-related smoosh preserves `eq?` when possible on cons cells with path-related elements"))

(check-pred
  unknown?
  (s= (pw /cons 0 0+i) (pw /cons 0 1+i))
  "Path-related smoosh is unknown on cons cells with at least one pair of corresponding elements whose path-relatedness is unknown and no pairs whose path-relatedness is known false")

(check-equal?
  (s= (pw /cons path-failing-1 0+i) (pw /cons path-failing-2 1+i))
  (known /nothing)
  "Path-related smoosh fails on cons cells with at least one pair of corresponding elements whose path-related smoosh fails, even if another pair's path-related smoosh result is unknown")

(check-equal?
  (s= (iw /cons 0 0) (iw /cons 0 0))
  (known /just /known /iw /cons 0 0)
  "Info smoosh works on cons cells whose elements are info smooshable")

(w- obj (iw /cons 0 0)
  (check-eq?
    (known-value /just-value /known-value /s= obj (iw /cons 0 0))
    obj
    "Info smoosh preserves `eq?` when possible on cons cells whose elements are info smooshable"))

(check-equal?
  (s= (iw /cons 0 0) (iw /cons 0 0.0))
  (known /nothing)
  "Info smoosh fails on cons cells with a pair of corresponding elements whose info smoosh fails")

(check-equal?
  (sj (iw /cons 0 0) (iw /cons 0 0))
  (known /just /known /iw /cons 0 0)
  "Info smoosh join works on cons cells whose elements are info smoosh joinable")

(w- obj (iw /cons 0 0)
  (check-eq?
    (known-value /just-value /known-value /sj obj (iw /cons 0 0))
    obj
    "Info smoosh join preserves `eq?` when possible on cons cells whose elements are info smoosh joinable"))

(check-equal?
  (sj (iw /cons 0 0) (iw /cons 0 0.0))
  (known /nothing)
  "Info smoosh join fails on cons cells with at least one pair of corresponding elements whose info smoosh join fails")

(check-equal?
  (sm (iw /cons 0 0) (iw /cons 0 0))
  (known /just /known /iw /cons 0 0)
  "Info smoosh meet works on cons cells whose elements are info smoosh meetable")

(w- obj (iw /cons 0 0)
  (check-eq?
    (known-value /just-value /known-value /sm obj (iw /cons 0 0))
    obj
    "Info smoosh meet preserves `eq?` when possible on cons cells whose elements are info smoosh meetable"))

(check-equal?
  (sm (iw /cons 0 0) (iw /cons 0 0.0))
  (known /nothing)
  "Info smoosh meet fails on cons cells with at least one pair of corresponding elements whose info smoosh meet fails")

(check-equal?
  (s= (pw /iw /cons 0 0) (pw /iw /cons 0 0))
  (known /just /known /pw /iw /cons 0 0)
  "Path-related info smoosh works on cons cells whose elements are path-related info smooshable")

(w- obj (pw /iw /cons 0 0)
  (check-eq?
    (known-value /just-value /known-value /s= obj (pw /iw /cons 0 0))
    obj
    "Path-related info smoosh preserves `eq?` when possible on cons cells whose elements are path-related info smooshable"))

(check-equal?
  (s= (pw /iw /cons 0 0) (pw /iw /cons 0 0.0))
  (known /nothing)
  "Path-related info smoosh fails on cons cells with at least one pair of corresponding elements whose path-related info smoosh fails")


(check-equal?
  (s= 'a '#:b)
  (known /nothing)
  "Smoosh fails on interactions between different types of base syntactic atom")

(check-pred
  unknown?
  (sj 'a '#:b)
  "Smoosh join is unknown on interactions between different types of base syntactic atom")

(check-pred
  unknown?
  (sm 'a '#:b)
  "Smoosh meet is unknown on interactions between different types of base syntactic atom")

(check-pred
  unknown?
  (s= (pw 'a) (pw '#:b))
  "Path-related smoosh is unknown on interactions between different types of base syntactic atom")

(check-equal?
  (s= (iw 'a) (iw '#:b))
  (known /nothing)
  "Info smoosh fails on interactions between different types of base syntactic atom")

(check-equal?
  (sj (iw 'a) (iw '#:b))
  (known /nothing)
  "Info smoosh join fails on interactions between different types of base syntactic atom")

(check-equal?
  (sm (iw 'a) (iw '#:b))
  (known /nothing)
  "Info smoosh meet fails on interactions between different types of base syntactic atom")

(check-equal?
  (s= (pw /iw 'a) (pw /iw '#:b))
  (known /nothing)
  "Path-related info smoosh fails on interactions between different types of base syntactic atom")


(check-equal?
  (s= #f #f)
  (known /just /known #f)
  "Smoosh works on equal booleans")

(check-equal?
  (s= #f #t)
  (known /nothing)
  "Smoosh fails on unequal booleans")

(check-equal?
  (sj #f #f)
  (known /just /known #f)
  "Smoosh join works on equal booleans")

(check-pred
  unknown?
  (sj #f #t)
  "Smoosh join is unknown on unequal booleans")

(check-equal?
  (sm #f #f)
  (known /just /known #f)
  "Smoosh meet works on equal booleans")

(check-pred
  unknown?
  (sm #f #t)
  "Smoosh meet is unknown on unequal booleans")

(check-equal?
  (s= (pw #f) (pw #f))
  (known /just /known /pw #f)
  "Path-related smoosh works on equal booleans")

(check-pred
  unknown?
  (s= (pw #f) (pw #t))
  "Path-related smoosh is unknown on unequal booleans")

(check-equal?
  (s= (iw #f) (iw #f))
  (known /just /known /iw #f)
  "Info smoosh works on equal booleans")

(check-equal?
  (s= (iw #f) (iw #t))
  (known /nothing)
  "Info smoosh fails on unequal booleans")

(check-equal?
  (sj (iw #f) (iw #f))
  (known /just /known /iw #f)
  "Info smoosh join works on equal booleans")

(check-equal?
  (sj (iw #f) (iw #t))
  (known /nothing)
  "Info smoosh join fails on unequal booleans")

(check-equal?
  (sm (iw #f) (iw #f))
  (known /just /known /iw #f)
  "Info smoosh meet works on equal booleans")

(check-equal?
  (sm (iw #f) (iw #t))
  (known /nothing)
  "Info smoosh meet fails on unequal booleans")

(check-equal?
  (s= (pw /iw #f) (pw /iw #f))
  (known /just /known /pw /iw #f)
  "Path-related info smoosh works on equal booleans")

(check-equal?
  (s= (pw /iw #f) (pw /iw #t))
  (known /nothing)
  "Path-related info smoosh fails on unequal booleans")


(check-pred
  unknown?
  (s= +nan.0 +nan.0)
  "Smoosh is unknown on real NaNs")

(check-pred
  unknown?
  (s= 0+nan.0i 0+nan.0i)
  "Smoosh is unknown on imaginary NaNs")

(check-pred
  unknown?
  (s= +nan.0 0+nan.0i)
  "Smoosh is unknown on distinct NaNs")

(check-pred
  unknown?
  (sj +nan.0 +nan.0)
  "Smoosh join is unknown on real NaNs")

(check-pred
  unknown?
  (sj 0+nan.0i 0+nan.0i)
  "Smoosh join is unknown on imaginary NaNs")

(check-pred
  unknown?
  (sj +nan.0 0+nan.0i)
  "Smoosh join is unknown on distinct NaNs")

(check-pred
  unknown?
  (sm +nan.0 +nan.0)
  "Smoosh meet is unknown on real NaNs")

(check-pred
  unknown?
  (sm 0+nan.0i 0+nan.0i)
  "Smoosh meet is unknown on imaginary NaNs")

(check-pred
  unknown?
  (sm +nan.0 0+nan.0i)
  "Smoosh meet is unknown on distinct NaNs")

(check-pred
  unknown?
  (s= (pw +nan.0) (pw +nan.0))
  "Path-related smoosh is unknown on real NaNs")

(check-pred
  unknown?
  (s= (pw 0+nan.0i) (pw 0+nan.0i))
  "Path-related smoosh is unknown on imaginary NaNs")

(check-pred
  unknown?
  (s= (pw +nan.0) (pw 0+nan.0i))
  "Path-related smoosh is unknown on distinct NaNs")

(check-pred
  unknown?
  (s= (iw +nan.0) (iw +nan.0))
  "Info smoosh is unknown on real NaNs")

(check-pred
  unknown?
  (s= (iw 0+nan.0i) (iw 0+nan.0i))
  "Info smoosh is unknown on imaginary NaNs")

(check-pred
  unknown?
  (s= (iw +nan.0) (iw 0+nan.0i))
  "Info smoosh is unknown on distinct NaNs")

(check-pred
  unknown?
  (sj (iw +nan.0) (iw +nan.0))
  "Info smoosh join is unknown on real NaNs")

(check-pred
  unknown?
  (sj (iw 0+nan.0i) (iw 0+nan.0i))
  "Info smoosh join is unknown on imaginary NaNs")

(check-pred
  unknown?
  (sj (iw +nan.0) (iw 0+nan.0i))
  "Info smoosh join is unknown on distinct NaNs")

(check-pred
  unknown?
  (sm (iw +nan.0) (iw +nan.0))
  "Info smoosh meet is unknown on real NaNs")

(check-pred
  unknown?
  (sm (iw 0+nan.0i) (iw 0+nan.0i))
  "Info smoosh meet is unknown on imaginary NaNs")

(check-pred
  unknown?
  (sm (iw +nan.0) (iw 0+nan.0i))
  "Info smoosh meet is unknown on distinct NaNs")

(check-pred
  unknown?
  (s= (pw /iw +nan.0) (pw /iw +nan.0))
  "Path-related info smoosh is unknown on real NaNs")

(check-pred
  unknown?
  (s= (pw /iw 0+nan.0i) (pw /iw 0+nan.0i))
  "Path-related info smoosh is unknown on imaginary NaNs")

(check-pred
  unknown?
  (s= (pw /iw +nan.0) (pw /iw 0+nan.0i))
  "Path-related info smoosh is unknown on distinct NaNs")


(check-equal?
  (s= 0 0.0)
  (known /just /known 0)
  "Smoosh works on equal numbers")

(check-equal?
  (s= 0 1)
  (known /nothing)
  "Smoosh fails on unequal numbers")

(check-equal?
  (sj 0 0.0)
  (known /just /known 0)
  "Smoosh join works on equal numbers")

(check-equal?
  (sj 0 1+0.0i)
  (known /just /known 1+0.0i)
  "Smoosh join works on unequal, comparable numbers")

(check-pred
  unknown?
  (sj 0+i 1+i)
  "Smoosh join is unknown on unequal, uncomparable numbers")

(check-equal?
  (sm 0 0.0)
  (known /just /known 0)
  "Smoosh meet works on equal numbers")

(check-equal?
  (sm 0 1+0.0i)
  (known /just /known 0)
  "Smoosh meet works on unequal, comparable numbers")

(check-pred
  unknown?
  (sm 0+i 1+i)
  "Smoosh meet is unknown on unequal, uncomparable numbers")

(check-equal?
  (s= (pw 0) (pw 0.0))
  (known /just /known /pw 0)
  "Path-related smoosh works on equal, zero-imaginary-part numbers")

(check-equal?
  (s= (pw 0) (pw 1+0.0i))
  (known /just /known /pw 0)
  "Path-related smoosh works on unequal, zero-imaginary-part numbers")

(check-equal?
  (s= (pw 0+i) (pw 0.0+1.0i))
  (known /just /known /pw 0+i)
  "Path-related smoosh works on equal, nonzero-imaginary-part numbers")

(check-pred
  unknown?
  (s= (pw 0+i) (pw 1+i))
  "Path-related smoosh is unknown on unequal, nonzero-imaginary-part numbers")

(check-equal?
  (s= (iw 0) (iw 0))
  (known /just /known /iw 0)
  "Info smoosh works on `equal-always?` numbers")

(check-equal?
  (s= (iw 0) (iw 0.0))
  (known /nothing)
  "Info smoosh fails on non-`equal-always?` numbers")

(check-equal?
  (sj (iw 0) (iw 0))
  (known /just /known /iw 0)
  "Info smoosh join works on `equal-always?` numbers")

(check-equal?
  (sj (iw 0) (iw 0.0))
  (known /nothing)
  "Info smoosh join fails on non-`equal-always?` numbers")

(check-equal?
  (sm (iw 0) (iw 0))
  (known /just /known /iw 0)
  "Info smoosh meet works on `equal-always?` numbers")

(check-equal?
  (sm (iw 0) (iw 0.0))
  (known /nothing)
  "Info smoosh meet fails on non-`equal-always?` numbers")

(check-equal?
  (s= (pw /iw 0) (pw /iw 0))
  (known /just /known /pw /iw 0)
  "Path-related info smoosh works on `equal-always?` numbers")

(check-equal?
  (s= (pw /iw 0) (pw /iw 0.0))
  (known /nothing)
  "Path-related info smoosh fails on non-`equal-always?` numbers")


(check-pred
  unknown?
  (if (extflonum-available?)
    (s= +nan.t +nan.t)
    (unknown))
  "Smoosh is unknown on extflonum NaNs")

(check-pred
  unknown?
  (if (extflonum-available?)
    (sj +nan.t +nan.t)
    (unknown))
  "Smoosh join is unknown on extflonum NaNs")

(check-pred
  unknown?
  (if (extflonum-available?)
    (sm +nan.t +nan.t)
    (unknown))
  "Smoosh meet is unknown on extflonum NaNs")

(check-pred
  unknown?
  (if (extflonum-available?)
    (s= (pw +nan.t) (pw +nan.t))
    (unknown))
  "Path-related smoosh is unknown on extflonum NaNs")

(check-pred
  unknown?
  (if (extflonum-available?)
    (s= (iw +nan.t) (iw +nan.t))
    (unknown))
  "Info smoosh is unknown on extflonum NaNs")

(check-pred
  unknown?
  (if (extflonum-available?)
    (sj (iw +nan.t) (iw +nan.t))
    (unknown))
  "Info smoosh join is unknown on extflonum NaNs")

(check-pred
  unknown?
  (if (extflonum-available?)
    (sm (iw +nan.t) (iw +nan.t))
    (unknown))
  "Info smoosh meet is unknown on extflonum NaNs")

(check-pred
  unknown?
  (if (extflonum-available?)
    (s= (pw /iw +nan.t) (pw /iw +nan.t))
    (unknown))
  "Path-related info smoosh is unknown on extflonum NaNs")


(check-equal?
  (and (extflonum-available?) (s= -0t0 0t0))
  (and (extflonum-available?) (known /just /known -0t0))
  "Smoosh works on equal extflonums")

(check-equal?
  (if (extflonum-available?)
    (s= 0t0 1t0)
    (known /nothing))
  (known /nothing)
  "Smoosh fails on unequal extflonums")

(check-equal?
  (and (extflonum-available?) (sj -0t0 0t0))
  (and (extflonum-available?) (known /just /known -0t0))
  "Smoosh join works on equal extflonums")

(check-equal?
  (and (extflonum-available?) (sj 0t0 1t0))
  (and (extflonum-available?) (known /just /known 1t0))
  "Smoosh join works on unequal extflonums")

(check-equal?
  (and (extflonum-available?) (sm -0t0 0t0))
  (and (extflonum-available?) (known /just /known -0t0))
  "Smoosh meet works on equal extflonums")

(check-equal?
  (and (extflonum-available?) (sm 0t0 1t0))
  (and (extflonum-available?) (known /just /known 0t0))
  "Smoosh meet works on unequal extflonums")

(check-equal?
  (and (extflonum-available?) (s= (pw -0t0) (pw 0t0)))
  (and (extflonum-available?) (known /just /known /pw -0t0))
  "Path-related smoosh works on equal extflonums")

(check-equal?
  (and (extflonum-available?) (s= (pw 0t0) (pw 1t0)))
  (and (extflonum-available?) (known /just /known /pw 0t0))
  "Path-related smoosh works on unequal extflonums")

(check-equal?
  (and (extflonum-available?) (s= (iw 0t0) (iw 0t0)))
  (and (extflonum-available?) (known /just /known /iw 0t0))
  "Info smoosh works on `equal-always?` extflonums")

(check-equal?
  (if (extflonum-available?)
    (s= (iw -0t0) (iw 0t0))
    (known /nothing))
  (known /nothing)
  "Info smoosh fails on non-`equal-always?` extflonums")

(check-equal?
  (and (extflonum-available?) (sj (iw 0t0) (iw 0t0)))
  (and (extflonum-available?) (known /just /known /iw 0t0))
  "Info smoosh join works on `equal-always?` extflonums")

(check-equal?
  (if (extflonum-available?)
    (sj (iw -0t0) (iw 0t0))
    (known /nothing))
  (known /nothing)
  "Info smoosh join fails on non-`equal-always?` extflonums")

(check-equal?
  (and (extflonum-available?) (sm (iw 0t0) (iw 0t0)))
  (and (extflonum-available?) (known /just /known /iw 0t0))
  "Info smoosh meet works on `equal-always?` extflonums")

(check-equal?
  (if (extflonum-available?)
    (sm (iw -0t0) (iw 0t0))
    (known /nothing))
  (known /nothing)
  "Info smoosh meet fails on non-`equal-always?` extflonums")

(check-equal?
  (and (extflonum-available?) (s= (pw /iw 0t0) (pw /iw 0t0)))
  (and (extflonum-available?) (known /just /known /pw /iw 0t0))
  "Path-related info smoosh works on `equal-always?` extflonums")

(check-equal?
  (if (extflonum-available?)
    (s= (pw /iw -0t0) (pw /iw 0t0))
    (known /nothing))
  (known /nothing)
  "Path-related info smoosh fails on non-`equal-always?` extflonums")


(check-equal?
  (s= #\a #\a)
  (known /just /known #\a)
  "Smoosh works on equal characters")

(check-pred
  unknown?
  (s= #\a #\b)
  "Smoosh is unknown on unequal characters")

(check-equal?
  (sj #\a #\a)
  (known /just /known #\a)
  "Smoosh join works on equal characters")

(check-pred
  unknown?
  (sj #\a #\b)
  "Smoosh join is unknown on unequal characters")

(check-equal?
  (sm #\a #\a)
  (known /just /known #\a)
  "Smoosh meet works on equal characters")

(check-pred
  unknown?
  (sm #\a #\b)
  "Smoosh meet is unknown on unequal characters")

(check-equal?
  (s= (pw #\a) (pw #\a))
  (known /just /known /pw #\a)
  "Path-related smoosh works on equal characters")

(check-pred
  unknown?
  (s= (pw #\a) (pw #\b))
  "Path-related smoosh is unknown on unequal characters")

(check-equal?
  (s= (iw #\a) (iw #\a))
  (known /just /known /iw #\a)
  "Info smoosh works on equal characters")

(check-pred
  unknown?
  (s= (iw #\a) (iw #\b))
  "Info smoosh is unknown on unequal characters")

(check-equal?
  (sj (iw #\a) (iw #\a))
  (known /just /known /iw #\a)
  "Info smoosh join works on equal characters")

(check-pred
  unknown?
  (sj (iw #\a) (iw #\b))
  "Info smoosh join is unknown on unequal characters")

(check-equal?
  (sm (iw #\a) (iw #\a))
  (known /just /known /iw #\a)
  "Info smoosh meet works on equal characters")

(check-pred
  unknown?
  (sm (iw #\a) (iw #\b))
  "Info smoosh meet is unknown on unequal characters")

(check-equal?
  (s= (pw /iw #\a) (pw /iw #\a))
  (known /just /known /pw /iw #\a)
  "Path-related info smoosh works on equal characters")

(check-pred
  unknown?
  (s= (pw /iw #\a) (pw /iw #\b))
  "Path-related info smoosh is unknown on unequal characters")


(check-equal?
  (s= str1 str1)
  (known /just /known str1)
  "Smoosh works on equal immutable strings")

(check-pred
  unknown?
  (s= str1 str2)
  "Smoosh is unknown on unequal immutable strings")

(check-equal?
  (sj str1 str1)
  (known /just /known str1)
  "Smoosh join works on equal immutable strings")

(check-pred
  unknown?
  (sj str1 str2)
  "Smoosh join is unknown on unequal immutable strings")

(check-equal?
  (sm str1 str1)
  (known /just /known str1)
  "Smoosh meet works on equal immutable strings")

(check-pred
  unknown?
  (sm str1 str2)
  "Smoosh meet is unknown on unequal immutable strings")

(check-equal?
  (s= (pw str1) (pw str1))
  (known /just /known /pw str1)
  "Path-related smoosh works on equal immutable strings")

(check-pred
  unknown?
  (s= (pw str1) (pw str2))
  "Path-related smoosh is unknown on unequal immutable strings")

(check-equal?
  (s= (iw str1) (iw str1))
  (known /just /known /iw str1)
  "Info smoosh works on equal immutable strings")

(check-pred
  unknown?
  (s= (iw str1) (iw str2))
  "Info smoosh is unknown on unequal immutable strings")

(check-equal?
  (sj (iw str1) (iw str1))
  (known /just /known /iw str1)
  "Info smoosh join works on equal immutable strings")

(check-pred
  unknown?
  (sj (iw str1) (iw str2))
  "Info smoosh join is unknown on unequal immutable strings")

(check-equal?
  (sm (iw str1) (iw str1))
  (known /just /known /iw str1)
  "Info smoosh meet works on equal immutable strings")

(check-pred
  unknown?
  (sm (iw str1) (iw str2))
  "Info smoosh meet is unknown on unequal immutable strings")

(check-equal?
  (s= (pw /iw str1) (pw /iw str1))
  (known /just /known /pw /iw str1)
  "Path-related info smoosh works on equal immutable strings")

(check-pred
  unknown?
  (s= (pw /iw str1) (pw /iw str2))
  "Path-related info smoosh is unknown on unequal immutable strings")


(check-equal?
  (s= bts1 bts1)
  (known /just /known bts1)
  "Smoosh works on equal immutable byte strings")

(check-pred
  unknown?
  (s= bts1 bts2)
  "Smoosh is unknown on unequal immutable byte strings")

(check-equal?
  (sj bts1 bts1)
  (known /just /known bts1)
  "Smoosh join works on equal immutable byte strings")

(check-pred
  unknown?
  (sj bts1 bts2)
  "Smoosh join is unknown on unequal immutable byte strings")

(check-equal?
  (sm bts1 bts1)
  (known /just /known bts1)
  "Smoosh meet works on equal immutable byte strings")

(check-pred
  unknown?
  (sm bts1 bts2)
  "Smoosh meet is unknown on unequal immutable byte strings")

(check-equal?
  (s= (pw bts1) (pw bts1))
  (known /just /known /pw bts1)
  "Path-related smoosh works on equal immutable byte strings")

(check-pred
  unknown?
  (s= (pw bts1) (pw bts2))
  "Path-related smoosh is unknown on unequal immutable byte strings")

(check-equal?
  (s= (iw bts1) (iw bts1))
  (known /just /known /iw bts1)
  "Info smoosh works on equal immutable byte strings")

(check-pred
  unknown?
  (s= (iw bts1) (iw bts2))
  "Info smoosh is unknown on unequal immutable byte strings")

(check-equal?
  (sj (iw bts1) (iw bts1))
  (known /just /known /iw bts1)
  "Info smoosh join works on equal immutable byte strings")

(check-pred
  unknown?
  (sj (iw bts1) (iw bts2))
  "Info smoosh join is unknown on unequal immutable byte strings")

(check-equal?
  (sm (iw bts1) (iw bts1))
  (known /just /known /iw bts1)
  "Info smoosh meet works on equal immutable byte strings")

(check-pred
  unknown?
  (sm (iw bts1) (iw bts2))
  "Info smoosh meet is unknown on unequal immutable byte strings")

(check-equal?
  (s= (pw /iw bts1) (pw /iw bts1))
  (known /just /known /pw /iw bts1)
  "Path-related info smoosh works on equal immutable byte strings")

(check-pred
  unknown?
  (s= (pw /iw bts1) (pw /iw bts2))
  "Path-related info smoosh is unknown on unequal immutable byte strings")


(check-pred
  unknown?
  (s= regexp1 regexp1)
  "Smoosh is unknown on regular expressions")

(check-pred
  unknown?
  (sj regexp1 regexp1)
  "Smoosh join is unknown on regular expressions")

(check-pred
  unknown?
  (sm regexp1 regexp1)
  "Smoosh meet is unknown on regular expressions")

(check-pred
  unknown?
  (s= (pw regexp1) (pw regexp1))
  "Path-related smoosh is unknown on regular expressions")

(check-pred
  unknown?
  (s= (iw regexp1) (iw regexp1))
  "Info smoosh is unknown on regular expressions")

(check-pred
  unknown?
  (sj (iw regexp1) (iw regexp1))
  "Info smoosh join is unknown on regular expressions")

(check-pred
  unknown?
  (sm (iw regexp1) (iw regexp1))
  "Info smoosh meet is unknown on regular expressions")

(check-pred
  unknown?
  (s= (pw /iw regexp1) (pw /iw regexp1))
  "Path-related info smoosh is unknown on regular expressions")


(check-pred
  unknown?
  (s= expr1 expr1)
  "Smoosh is unknown on compiled code expressions")

(check-pred
  unknown?
  (sj expr1 expr1)
  "Smoosh join is unknown on compiled code expressions")

(check-pred
  unknown?
  (sm expr1 expr1)
  "Smoosh meet is unknown on compiled code expressions")

(check-pred
  unknown?
  (s= (pw expr1) (pw expr1))
  "Path-related smoosh is unknown on compiled code expressions")

(check-pred
  unknown?
  (s= (iw expr1) (iw expr1))
  "Info smoosh is unknown on compiled code expressions")

(check-pred
  unknown?
  (sj (iw expr1) (iw expr1))
  "Info smoosh join is unknown on compiled code expressions")

(check-pred
  unknown?
  (sm (iw expr1) (iw expr1))
  "Info smoosh meet is unknown on compiled code expressions")

(check-pred
  unknown?
  (s= (pw /iw expr1) (pw /iw expr1))
  "Path-related info smoosh is unknown on compiled code expressions")


(check-equal?
  (s= (box-immutable 0) (box-immutable 0.0))
  (known /just /known /box-immutable 0)
  "Smoosh works on equal immutable boxes")

(w- obj (box-immutable 0)
  (check-eq?
    (known-value /just-value /known-value /s= obj (box-immutable 0.0))
    obj
    "Smoosh preserves `eq?` when possible on equal immutable boxes"))

(check-equal?
  (s= (box-immutable 0) (box-immutable 1))
  (known /nothing)
  "Smoosh fails on unequal immutable boxes")

(check-equal?
  (sj (box-immutable 0) (box-immutable 0.0))
  (known /just /known /box-immutable 0)
  "Smoosh join works on equal immutable boxes")

(w- obj (box-immutable 0)
  (check-eq?
    (known-value /just-value /known-value /sj obj (box-immutable 0.0))
    obj
    "Smoosh join preserves `eq?` when possible on equal immutable boxes"))

(check-equal?
  (sj (box-immutable 1) (box-immutable 0.0))
  (known /just /known /box-immutable 1)
  "Smoosh join works on unequal, comparable immutable boxes")

(check-pred
  unknown?
  (sj (box-immutable 0+i) (box-immutable 1+i))
  "Smoosh join is unknown on unequal, uncomparable immutable boxes")

(check-equal?
  (sm (box-immutable 0) (box-immutable 0.0))
  (known /just /known /box-immutable 0)
  "Smoosh meet works on equal immutable boxes")

(w- obj (box-immutable 0)
  (check-eq?
    (known-value /just-value /known-value /sm obj (box-immutable 0.0))
    obj
    "Smoosh meet preserves `eq?` when possible on equal immutable boxes"))

(check-equal?
  (sm (box-immutable 1) (box-immutable 0.0))
  (known /just /known /box-immutable 0.0)
  "Smoosh meet works on unequal, comparable immutable boxes")

(check-pred
  unknown?
  (sm (box-immutable 0+i) (box-immutable 1+i))
  "Smoosh meet is unknown on unequal, uncomparable immutable boxes")

(check-equal?
  (s= (pw /box-immutable 0) (pw /box-immutable 0.0))
  (known /just /known /pw /box-immutable 0)
  "Path-related smoosh works on equal immutable boxes")

(w- obj (pw /box-immutable 0)
  (check-eq?
    (known-value /just-value /known-value
      (s= obj (pw /box-immutable 0.0)))
    obj
    "Path-related smoosh preserves `eq?` when possible on equal immutable boxes"))

(check-equal?
  (s= (pw /box-immutable 0) (pw /box-immutable 1.0))
  (known /just /known /pw /box-immutable 0)
  "Path-related smoosh works on immutable boxes with path-related elements")

(w- obj (pw /box-immutable 0)
  (check-eq?
    (known-value /just-value /known-value
      (s= obj (pw /box-immutable 1.0)))
    obj
    "Path-related smoosh preserves `eq?` when possible on immutable boxes with path-related elements"))

(check-pred
  unknown?
  (s= (pw /box-immutable 0+i) (pw /box-immutable 1+i))
  "Path-related smoosh is unknown on immutable boxes with at least one pair of corresponding elements whose path-relatedness is unknown and no pairs whose path-relatedness is known false")

(check-equal?
  (s= (iw /box-immutable 0) (iw /box-immutable 0))
  (known /just /known /iw /box-immutable 0)
  "Info smoosh works on shallowly `chaperone=?` immutable boxes whose elements are info smooshable")

(w- obj (iw /box-immutable 0)
  (check-eq?
    (known-value /just-value /known-value
      (s= obj (iw /box-immutable 0)))
    obj
    "Info smoosh preserves `eq?` when possible on shallowly `chaperone=?` immutable boxes whose elements are info smooshable"))

(check-equal?
  (s= (iw /box-immutable 0) (iw /box-immutable 0.0))
  (known /nothing)
  "Info smoosh fails on shallowly `chaperone=?` immutable boxes with a pair of corresponding elements whose info smoosh fails")

(check-pred
  unknown?
  (s= (iw ibox1-chap-chap) (iw ibox1-chap))
  "Info smoosh is unknown on non-shallowly-`chaperone=?` immutable boxes even when they're shallowly `chaperone-of?` in one direction and have elements which are info smooshable")

(check-pred
  unknown?
  (s= (iw ibox1-chap) (iw ibox1-chap2))
  "Info smoosh is unknown on non-shallowly-`chaperone=?` immutable boxes even when they're shallowly `equal-always?` and have elements which are info smooshable")

(check-equal?
  (sj (iw /box-immutable 0) (iw /box-immutable 0))
  (known /just /known /iw /box-immutable 0)
  "Info smoosh join works on shallowly `chaperone=?` immutable boxes whose elements are info smoosh joinable")

(w- obj (iw /box-immutable 0)
  (check-eq?
    (known-value /just-value /known-value
      (sj obj (iw /box-immutable 0)))
    obj
    "Info smoosh join preserves `eq?` when possible on shallowly `chaperone=?` immutable boxes whose elements are info smoosh joinable"))

(check-equal?
  (sj (iw ibox1-chap-chap) (iw ibox1-chap))
  (known /just /known /iw ibox1-chap-chap)
  "Info smoosh join works on shallowly `chaperone-of?` immutable boxes even when they're not shallowly `chaperone=?`")

(check-eq?
  (info-wrapper-value /known-value /just-value /known-value
    (sj (iw ibox1-chap-chap) (iw ibox1-chap)))
  ibox1-chap-chap
  "Info smoosh join preserves `eq?` on shallowly `chaperone-of?` immutable boxes even when they're not shallowly `chaperone=?`")

(check-equal?
  (sj (iw /box-immutable 0) (iw /box-immutable 0.0))
  (known /nothing)
  "Info smoosh join fails on immutable boxes with at least one pair of corresponding elements whose info smoosh join fails")

(check-pred
  unknown?
  (sj (iw ibox1-chap) (iw ibox1-chap2))
  "Info smoosh join is unknown on non-shallowly-`chaperone-of?` immutable boxes even when they're `equal-always?`")

(check-equal?
  (sm (iw /box-immutable 0) (iw /box-immutable 0))
  (known /just /known /iw /box-immutable 0)
  "Info smoosh meet works on shallowly `chaperone=?` immutable boxes whose elements are info smoosh meetable")

(w- obj (iw /box-immutable 0)
  (check-eq?
    (known-value /just-value /known-value
      (sm obj (iw /box-immutable 0)))
    obj
    "Info smoosh meet preserves `eq?` when possible on shallowly `chaperone=?` immutable boxes whose elements are info smoosh meetable"))

(check-equal?
  (sm (iw ibox1-chap-chap) (iw ibox1-chap))
  (known /just /known /iw ibox1-chap)
  "Info smoosh meet works on shallowly `chaperone-of?` immutable boxes even when they're not shallowly `chaperone=?`")

(check-eq?
  (info-wrapper-value /known-value /just-value /known-value
    (sm (iw ibox1-chap-chap) (iw ibox1-chap)))
  ibox1-chap
  "Info smoosh meet preserves `eq?` on shallowly `chaperone-of?` immutable boxes even when they're not shallowly `chaperone=?`")

(check-equal?
  (sm (iw /box-immutable 0) (iw /box-immutable 0.0))
  (known /nothing)
  "Info smoosh meet fails on immutable boxes with at least one pair of corresponding elements whose info smoosh meet fails")

(check-pred
  unknown?
  (sm (iw ibox1-chap) (iw ibox1-chap2))
  "Info smoosh meet is unknown on non-shallowly-`chaperone-of?` immutable boxes even when they're `equal-always?`")

(check-equal?
  (s= (pw /iw /box-immutable 0) (pw /iw /box-immutable 0))
  (known /just /known /pw /iw /box-immutable 0)
  "Path-related info smoosh works on immutable boxes whose elements are path-related info smooshable")

(w- obj (pw /iw /box-immutable 0)
  (check-eq?
    (known-value /just-value /known-value
      (s= obj (pw /iw /box-immutable 0)))
    obj
    "Path-related info smoosh preserves `eq?` when possible on immutable boxes whose elements are path-related info smooshable"))

(check-equal?
  (s= (pw /iw ibox1-chap-chap) (pw /iw ibox1-chap))
  (known /just /known /pw /iw ibox1-chap-chap)
  "Path-related info smoosh works on `equal-always?` immutable boxes even when they're only shallowly `chaperone-of?` in one direction")

(check-eq?
  (info-wrapper-value /path-related-wrapper-value
    (known-value /just-value /known-value
      (s= (pw /iw ibox1-chap-chap) (pw /iw ibox1-chap))))
  ibox1-chap-chap
  "Path-related info smoosh preserves `eq?` on `equal-always?` immutable boxes even when they're only shallowly `chaperone-of?` in one direction")

(check-equal?
  (s= (pw /iw ibox1-chap) (pw /iw ibox1-chap2))
  (known /just /known /pw /iw ibox1-chap)
  "Path-related info smoosh works on `equal-always?` immutable boxes even when they're not shallowly `chaperone=?` in either direction")

(check-eq?
  (info-wrapper-value /path-related-wrapper-value
    (known-value /just-value /known-value
      (s= (pw /iw ibox1-chap) (pw /iw ibox1-chap2))))
  ibox1-chap
  "Path-related info smoosh preserves `eq?` on `equal-always?` immutable boxes even when they're not shallowly `chaperone=?` in either direction")

(check-equal?
  (s= (pw /iw /box-immutable 0) (pw /iw /box-immutable 0.0))
  (known /nothing)
  "Path-related info smoosh fails on immutable boxes with at least one pair of corresponding elements whose path-related info smoosh fails")


(check-equal?
  (s= (vector-immutable 0 0.0) (vector-immutable 0.0 0))
  (known /just /known /vector-immutable 0 0.0)
  "Smoosh works on equal immutable vectors")

(w- obj (vector-immutable 0 0.0)
  (check-eq?
    (known-value /just-value /known-value
      (s= obj (vector-immutable 0.0 0)))
    obj
    "Smoosh preserves `eq?` when possible on equal immutable vectors"))

(check-equal?
  (s= (vector-immutable 0 0) (vector-immutable 1 0))
  (known /nothing)
  "Smoosh fails on unequal immutable vectors")

(check-equal?
  (sj (vector-immutable 0 0.0) (vector-immutable 0.0 0))
  (known /just /known /vector-immutable 0 0.0)
  "Smoosh join works on equal immutable vectors")

(w- obj (vector-immutable 0 0.0)
  (check-eq?
    (known-value /just-value /known-value
      (sj obj (vector-immutable 0.0 0)))
    obj
    "Smoosh join preserves `eq?` when possible on equal immutable vectors"))

(check-equal?
  (sj (vector-immutable 1 0) (vector-immutable 0.0 1+0.0i))
  (known /just /known /vector-immutable 1 1+0.0i)
  "Smoosh join works on unequal, comparable immutable vectors")

(check-pred
  unknown?
  (sj (vector-immutable 0 0+i) (vector-immutable 0 1+i))
  "Smoosh join is unknown on unequal, uncomparable immutable vectors")

(check-equal?
  (sm (vector-immutable 0 0.0) (vector-immutable 0.0 0))
  (known /just /known /vector-immutable 0 0.0)
  "Smoosh meet works on equal immutable vectors")

(w- obj (vector-immutable 0 0.0)
  (check-eq?
    (known-value /just-value /known-value
      (sm obj (vector-immutable 0.0 0)))
    obj
    "Smoosh meet preserves `eq?` when possible on equal immutable vectors"))

(check-equal?
  (sm (vector-immutable 1 0) (vector-immutable 0.0 1+0.0i))
  (known /just /known /vector-immutable 0.0 0)
  "Smoosh meet works on unequal, comparable immutable vectors")

(check-pred
  unknown?
  (sm (vector-immutable 0 0+i) (vector-immutable 0 1+i))
  "Smoosh meet is unknown on unequal, uncomparable immutable vectors")

(check-equal?
  (s= (pw /vector-immutable 0 0.0) (pw /vector-immutable 0.0 0))
  (known /just /known /pw /vector-immutable 0 0.0)
  "Path-related smoosh works on equal immutable vectors")

(w- obj (pw /vector-immutable 0 0.0)
  (check-eq?
    (known-value /just-value /known-value
      (s= obj (pw /vector-immutable 0.0 0)))
    obj
    "Path-related smoosh preserves `eq?` when possible on equal immutable vectors"))

(check-equal?
  (s= (pw /vector-immutable 0 0.0) (pw /vector-immutable 1.0 1+0.0i))
  (known /just /known /pw /vector-immutable 0 0.0)
  "Path-related smoosh works on immutable vectors with path-related elements")

(w- obj (pw /vector-immutable 0 0.0)
  (check-eq?
    (known-value /just-value /known-value
      (s= obj (pw /vector-immutable 1.0 1+0.0i)))
    obj
    "Path-related smoosh preserves `eq?` when possible on immutable vectors with path-related elements"))

(check-pred
  unknown?
  (s= (pw /vector-immutable 0 0+i) (pw /vector-immutable 0 1+i))
  "Path-related smoosh is unknown on immutable vectors with at least one pair of corresponding elements whose path-relatedness is unknown and no pairs whose path-relatedness is known false")

(check-equal?
  (s=
    (pw /vector-immutable path-failing-1 0+i)
    (pw /vector-immutable path-failing-2 1+i))
  (known /nothing)
  "Path-related smoosh fails on immutable vectors with at least one pair of corresponding elements whose path-related smoosh fails, even if another pair's path-related smoosh result is unknown")

(check-equal?
  (s= (iw /vector-immutable 0 0) (iw /vector-immutable 0 0))
  (known /just /known /iw /vector-immutable 0 0)
  "Info smoosh works on shallowly `chaperone=?` immutable vectors whose elements are info smooshable")

(w- obj (iw /vector-immutable 0 0)
  (check-eq?
    (known-value /just-value /known-value
      (s= obj (iw /vector-immutable 0 0)))
    obj
    "Info smoosh preserves `eq?` when possible on shallowly `chaperone=?` immutable vectors whose elements are info smooshable"))

(check-equal?
  (s= (iw /vector-immutable 0 0) (iw /vector-immutable 0 0.0))
  (known /nothing)
  "Info smoosh fails on shallowly `chaperone=?` immutable vectors with a pair of corresponding elements whose info smoosh fails")

(check-pred
  unknown?
  (s= (iw iv1-chap-chap) (iw iv1-chap-with-prop))
  "Info smoosh is unknown on non-shallowly-`chaperone=?` immutable vectors even when they're shallowly `chaperone-of?` in one direction and have elements which are info smooshable")

(check-pred
  unknown?
  (s= (iw iv1-chap) (iw iv1-chap2))
  "Info smoosh is unknown on non-shallowly-`chaperone=?` immutable vectors even when they're shallowly `equal-always?` and have elements which are info smooshable")

(check-equal?
  (sj (iw /vector-immutable 0 0) (iw /vector-immutable 0 0))
  (known /just /known /iw /vector-immutable 0 0)
  "Info smoosh join works on shallowly `chaperone=?` immutable vectors whose elements are info smoosh joinable")

(w- obj (iw /vector-immutable 0 0)
  (check-eq?
    (known-value /just-value /known-value
      (sj obj (iw /vector-immutable 0 0)))
    obj
    "Info smoosh join preserves `eq?` when possible on shallowly `chaperone=?` immutable vectors whose elements are info smoosh joinable"))

(check-equal?
  (sj (iw iv1-chap-chap) (iw iv1-chap-with-prop))
  (known /just /known /iw iv1-chap-chap)
  "Info smoosh join works on shallowly `chaperone-of?` immutable vectors even when they're not shallowly `chaperone=?`")

(check-eq?
  (info-wrapper-value /known-value /just-value /known-value
    (sj (iw iv1-chap-chap) (iw iv1-chap-with-prop)))
  iv1-chap-chap
  "Info smoosh join preserves `eq?` on shallowly `chaperone-of?` immutable vectors even when they're not shallowly `chaperone=?`")

(check-equal?
  (sj (iw /vector-immutable 0 0) (iw /vector-immutable 0 0.0))
  (known /nothing)
  "Info smoosh join fails on immutable vectors with at least one pair of corresponding elements whose info smoosh join fails")

(check-pred
  unknown?
  (sj (iw iv1-chap) (iw iv1-chap2))
  "Info smoosh join is unknown on non-shallowly-`chaperone-of?` immutable vectors even when they're `equal-always?`")

(check-equal?
  (sm (iw /vector-immutable 0 0) (iw /vector-immutable 0 0))
  (known /just /known /iw /vector-immutable 0 0)
  "Info smoosh meet works on shallowly `chaperone=?` immutable vectors whose elements are info smoosh meetable")

(w- obj (iw /vector-immutable 0 0)
  (check-eq?
    (known-value /just-value /known-value
      (sm obj (iw /vector-immutable 0 0)))
    obj
    "Info smoosh meet preserves `eq?` when possible on shallowly `chaperone=?` immutable vectors whose elements are info smoosh meetable"))

(check-equal?
  (sm (iw iv1-chap-chap) (iw iv1-chap-with-prop))
  (known /just /known /iw iv1-chap-with-prop)
  "Info smoosh meet works on shallowly `chaperone-of?` immutable vectors even when they're not shallowly `chaperone=?`")

(check-eq?
  (info-wrapper-value /known-value /just-value /known-value
    (sm (iw iv1-chap-chap) (iw iv1-chap-with-prop)))
  iv1-chap-with-prop
  "Info smoosh meet preserves `eq?` on shallowly `chaperone-of?` immutable vectors even when they're not shallowly `chaperone=?`")

(check-equal?
  (sm (iw /vector-immutable 0 0) (iw /vector-immutable 0 0.0))
  (known /nothing)
  "Info smoosh meet fails on immutable vectors with at least one pair of corresponding elements whose info smoosh meet fails")

(check-pred
  unknown?
  (sm (iw iv1-chap) (iw iv1-chap2))
  "Info smoosh meet is unknown on non-shallowly-`chaperone-of?` immutable vectors even when they're `equal-always?`")

(check-equal?
  (s= (pw /iw /vector-immutable 0 0) (pw /iw /vector-immutable 0 0))
  (known /just /known /pw /iw /vector-immutable 0 0)
  "Path-related info smoosh works on immutable vectors whose elements are path-related info smooshable")

(w- obj (pw /iw /vector-immutable 0 0)
  (check-eq?
    (known-value /just-value /known-value
      (s= obj (pw /iw /vector-immutable 0 0)))
    obj
    "Path-related info smoosh preserves `eq?` when possible on immutable vectors whose elements are path-related info smooshable"))

(check-equal?
  (s= (pw /iw iv1-chap-chap) (pw /iw iv1-chap-with-prop))
  (known /just /known /pw /iw iv1-chap-chap)
  "Path-related info smoosh works on `equal-always?` immutable vectors even when they're only shallowly `chaperone-of?` in one direction")

(check-eq?
  (info-wrapper-value /path-related-wrapper-value
    (known-value /just-value /known-value
      (s= (pw /iw iv1-chap-chap) (pw /iw iv1-chap-with-prop))))
  iv1-chap-chap
  "Path-related info smoosh preserves `eq?` on `equal-always?` immutable vectors even when they're only shallowly `chaperone-of?` in one direction")

(check-equal?
  (s= (pw /iw iv1-chap) (pw /iw iv1-chap2))
  (known /just /known /pw /iw iv1-chap)
  "Path-related info smoosh works on `equal-always?` immutable vectors even when they're not shallowly `chaperone=?` in either direction")

(check-eq?
  (info-wrapper-value /path-related-wrapper-value
    (known-value /just-value /known-value
      (s= (pw /iw iv1-chap) (pw /iw iv1-chap2))))
  iv1-chap
  "Path-related info smoosh preserves `eq?` on `equal-always?` immutable vectors even when they're not shallowly `chaperone=?` in either direction")

(check-equal?
  (s= (pw /iw /vector-immutable 0 0) (pw /iw /vector-immutable 0 0.0))
  (known /nothing)
  "Path-related info smoosh fails on immutable vectors with at least one pair of corresponding elements whose path-related info smoosh fails")


(check-equal?
  (s= (iprefab 0 0.0) (iprefab 0.0 0))
  (known /just /known /iprefab 0 0.0)
  "Smoosh works on equal immutable prefab structs")

(w- obj (iprefab 0 0.0)
  (check-eq?
    (known-value /just-value /known-value /s= obj (iprefab 0.0 0))
    obj
    "Smoosh preserves `eq?` when possible on equal immutable prefab structs"))

(check-equal?
  (s= (iprefab 0 0) (iprefab 1 0))
  (known /nothing)
  "Smoosh fails on unequal immutable prefab structs")

(check-equal?
  (sj (iprefab 0 0.0) (iprefab 0.0 0))
  (known /just /known /iprefab 0 0.0)
  "Smoosh join works on equal immutable prefab structs")

(w- obj (iprefab 0 0.0)
  (check-eq?
    (known-value /just-value /known-value /sj obj (iprefab 0.0 0))
    obj
    "Smoosh join preserves `eq?` when possible on equal immutable prefab structs"))

(check-equal?
  (sj (iprefab 1 0) (iprefab 0.0 1+0.0i))
  (known /just /known /iprefab 1 1+0.0i)
  "Smoosh join works on unequal, comparable immutable prefab structs")

(check-pred
  unknown?
  (sj (iprefab 0 0+i) (iprefab 0 1+i))
  "Smoosh join is unknown on unequal, uncomparable immutable prefab structs")

(check-equal?
  (sm (iprefab 0 0.0) (iprefab 0.0 0))
  (known /just /known /iprefab 0 0.0)
  "Smoosh meet works on equal immutable prefab structs")

(w- obj (iprefab 0 0.0)
  (check-eq?
    (known-value /just-value /known-value /sm obj (iprefab 0.0 0))
    obj
    "Smoosh meet preserves `eq?` when possible on equal immutable prefab structs"))

(check-equal?
  (sm (iprefab 1 0) (iprefab 0.0 1+0.0i))
  (known /just /known /iprefab 0.0 0)
  "Smoosh meet works on unequal, comparable immutable prefab structs")

(check-pred
  unknown?
  (sm (iprefab 0 0+i) (iprefab 0 1+i))
  "Smoosh meet is unknown on unequal, uncomparable immutable prefab structs")

(check-equal?
  (s= (pw /iprefab 0 0.0) (pw /iprefab 0.0 0))
  (known /just /known /pw /iprefab 0 0.0)
  "Path-related smoosh works on equal immutable prefab structs")

(w- obj (pw /iprefab 0 0.0)
  (check-eq?
    (known-value /just-value /known-value
      (s= obj (pw /iprefab 0.0 0)))
    obj
    "Path-related smoosh preserves `eq?` when possible on equal immutable prefab structs"))

(check-equal?
  (s= (pw /iprefab 0 0.0) (pw /iprefab 1.0 1+0.0i))
  (known /just /known /pw /iprefab 0 0.0)
  "Path-related smoosh works on immutable prefab structs with path-related elements")

(w- obj (pw /iprefab 0 0.0)
  (check-eq?
    (known-value /just-value /known-value
      (s= obj (pw /iprefab 1.0 1+0.0i)))
    obj
    "Path-related smoosh preserves `eq?` when possible on immutable prefab structs with path-related elements"))

(check-pred
  unknown?
  (s= (pw /iprefab 0 0+i) (pw /iprefab 0 1+i))
  "Path-related smoosh is unknown on immutable prefab structs with at least one pair of corresponding elements whose path-relatedness is unknown and no pairs whose path-relatedness is known false")

(check-equal?
  (s=
    (pw /iprefab path-failing-1 0+i)
    (pw /iprefab path-failing-2 1+i))
  (known /nothing)
  "Path-related smoosh fails on immutable prefab structs with at least one pair of corresponding elements whose path-related smoosh fails, even if another pair's path-related smoosh result is unknown")

(check-equal?
  (s= (iw /iprefab 0 0) (iw /iprefab 0 0))
  (known /just /known /iw /iprefab 0 0)
  "Info smoosh works on shallowly `chaperone=?` immutable prefab structs whose elements are info smooshable")

(w- obj (iw /iprefab 0 0)
  (check-eq?
    (known-value /just-value /known-value /s= obj (iw /iprefab 0 0))
    obj
    "Info smoosh preserves `eq?` when possible on shallowly `chaperone=?` immutable prefab structs whose elements are info smooshable"))

(check-equal?
  (s= (iw /iprefab 0 0) (iw /iprefab 0 0.0))
  (known /nothing)
  "Info smoosh fails on shallowly `chaperone=?` immutable prefab structs with a pair of corresponding elements whose info smoosh fails")

(check-pred
  unknown?
  (s= (iw iprefab1-chap-chap) (iw iprefab1-chap-with-prop))
  "Info smoosh is unknown on non-shallowly-`chaperone=?` immutable prefab structs even when they're shallowly `chaperone-of?` in one direction and have elements which are info smooshable")

(check-pred
  unknown?
  (s= (iw iprefab1-chap) (iw iprefab1-chap2))
  "Info smoosh is unknown on non-shallowly-`chaperone=?` immutable prefab structs even when they're shallowly `equal-always?` and have elements which are info smooshable")

(check-equal?
  (sj (iw /iprefab 0 0) (iw /iprefab 0 0))
  (known /just /known /iw /iprefab 0 0)
  "Info smoosh join works on shallowly `chaperone=?` immutable prefab structs whose elements are info smoosh joinable")

(w- obj (iw /iprefab 0 0)
  (check-eq?
    (known-value /just-value /known-value /sj obj (iw /iprefab 0 0))
    obj
    "Info smoosh join preserves `eq?` when possible on shallowly `chaperone=?` immutable prefab structs whose elements are info smoosh joinable"))

(check-equal?
  (sj (iw iprefab1-chap-chap) (iw iprefab1-chap-with-prop))
  (known /just /known /iw iprefab1-chap-chap)
  "Info smoosh join works on shallowly `chaperone-of?` immutable prefab structs even when they're not shallowly `chaperone=?`")

(check-eq?
  (info-wrapper-value /known-value /just-value /known-value
    (sj (iw iprefab1-chap-chap) (iw iprefab1-chap-with-prop)))
  iprefab1-chap-chap
  "Info smoosh join preserves `eq?` on shallowly `chaperone-of?` immutable prefab structs even when they're not shallowly `chaperone=?`")

(check-equal?
  (sj (iw /iprefab 0 0) (iw /iprefab 0 0.0))
  (known /nothing)
  "Info smoosh join fails on immutable prefab structs with at least one pair of corresponding elements whose info smoosh join fails")

(check-pred
  unknown?
  (sj (iw iprefab1-chap) (iw iprefab1-chap2))
  "Info smoosh join is unknown on non-shallowly-`chaperone-of?` immutable prefab structs even when they're `equal-always?`")

(check-equal?
  (sm (iw /iprefab 0 0) (iw /iprefab 0 0))
  (known /just /known /iw /iprefab 0 0)
  "Info smoosh meet works on shallowly `chaperone=?` immutable prefab structs whose elements are info smoosh meetable")

(w- obj (iw /iprefab 0 0)
  (check-eq?
    (known-value /just-value /known-value /sm obj (iw /iprefab 0 0))
    obj
    "Info smoosh meet preserves `eq?` when possible on shallowly `chaperone=?` immutable prefab structs whose elements are info smoosh meetable"))

(check-equal?
  (sm (iw iprefab1-chap-chap) (iw iprefab1-chap-with-prop))
  (known /just /known /iw iprefab1-chap-with-prop)
  "Info smoosh meet works on shallowly `chaperone-of?` immutable prefab structs even when they're not shallowly `chaperone=?`")

(check-eq?
  (info-wrapper-value /known-value /just-value /known-value
    (sm (iw iprefab1-chap-chap) (iw iprefab1-chap-with-prop)))
  iprefab1-chap-with-prop
  "Info smoosh meet preserves `eq?` on shallowly `chaperone-of?` immutable prefab structs even when they're not shallowly `chaperone=?`")

(check-equal?
  (sm (iw /iprefab 0 0) (iw /iprefab 0 0.0))
  (known /nothing)
  "Info smoosh meet fails on immutable prefab structs with at least one pair of corresponding elements whose info smoosh meet fails")

(check-pred
  unknown?
  (sm (iw iprefab1-chap) (iw iprefab1-chap2))
  "Info smoosh meet is unknown on non-shallowly-`chaperone-of?` immutable prefab structs even when they're `equal-always?`")

(check-equal?
  (s= (pw /iw /iprefab 0 0) (pw /iw /iprefab 0 0))
  (known /just /known /pw /iw /iprefab 0 0)
  "Path-related info smoosh works on immutable prefab structs whose elements are path-related info smooshable")

(w- obj (pw /iw /iprefab 0 0)
  (check-eq?
    (known-value /just-value /known-value
      (s= obj (pw /iw /iprefab 0 0)))
    obj
    "Path-related info smoosh preserves `eq?` when possible on immutable prefab structs whose elements are path-related info smooshable"))

(check-equal?
  (s= (pw /iw iprefab1-chap-chap) (pw /iw iprefab1-chap-with-prop))
  (known /just /known /pw /iw iprefab1-chap-chap)
  "Path-related info smoosh works on `equal-always?` immutable prefab structs even when they're only shallowly `chaperone-of?` in one direction")

(check-eq?
  (info-wrapper-value /path-related-wrapper-value
    (known-value /just-value /known-value
      (s= (pw /iw iprefab1-chap-chap) (pw /iw iprefab1-chap-with-prop))))
  iprefab1-chap-chap
  "Path-related info smoosh preserves `eq?` on `equal-always?` immutable prefab structs even when they're only shallowly `chaperone-of?` in one direction")

(check-equal?
  (s= (pw /iw iprefab1-chap) (pw /iw iprefab1-chap2))
  (known /just /known /pw /iw iprefab1-chap)
  "Path-related info smoosh works on `equal-always?` immutable prefab structs even when they're not shallowly `chaperone=?` in either direction")

(check-eq?
  (info-wrapper-value /path-related-wrapper-value
    (known-value /just-value /known-value
      (s= (pw /iw iprefab1-chap) (pw /iw iprefab1-chap2))))
  iprefab1-chap
  "Path-related info smoosh preserves `eq?` on `equal-always?` immutable prefab structs even when they're not shallowly `chaperone=?` in either direction")

(check-equal?
  (s= (pw /iw /iprefab 0 0) (pw /iw /iprefab 0 0.0))
  (known /nothing)
  "Path-related info smoosh fails on immutable prefab structs with at least one pair of corresponding elements whose path-related info smoosh fails")


(check-equal?
  (s= (hash #f 0 #t 0.0) (hash #f 0.0 #t 0))
  (known /just /known /hash #f 0 #t 0.0)
  "Smoosh works on equal immutable hash tables")

(w- obj (hash #f 0 #t 0.0)
  (check-eq?
    (known-value /just-value /known-value /s= obj (hash #f 0.0 #t 0))
    obj
    "Smoosh preserves `eq?` when possible on equal immutable hash tables"))

(check-equal?
  (s= (hash #f 0 #t 0) (hash #f 1 #t 0))
  (known /nothing)
  "Smoosh fails on unequal immutable hash tables")

(check-equal?
  (sj (hash #f 0 #t 0.0) (hash #f 0.0 #t 0))
  (known /just /known /hash #f 0 #t 0.0)
  "Smoosh join works on equal immutable hash tables")

(w- obj (hash #f 0 #t 0.0)
  (check-eq?
    (known-value /just-value /known-value /sj obj (hash #f 0.0 #t 0))
    obj
    "Smoosh join preserves `eq?` when possible on equal immutable hash tables"))

(check-equal?
  (sj (hash #f 1 #t 0) (hash #f 0.0 #t 1+0.0i))
  (known /just /known /hash #f 1 #t 1+0.0i)
  "Smoosh join works on unequal, comparable immutable hash tables")

(check-pred
  unknown?
  (sj (hash #f 0 #t 0+i) (hash #f 0 #t 1+i))
  "Smoosh join is unknown on unequal, uncomparable immutable hash tables")

(check-equal?
  (sm (hash #f 0 #t 0.0) (hash #f 0.0 #t 0))
  (known /just /known /hash #f 0 #t 0.0)
  "Smoosh meet works on equal immutable hash tables")

(w- obj (hash #f 0 #t 0.0)
  (check-eq?
    (known-value /just-value /known-value /sm obj (hash #f 0.0 #t 0))
    obj
    "Smoosh meet preserves `eq?` when possible on equal immutable hash tables"))

(check-equal?
  (sm (hash #f 1 #t 0) (hash #f 0.0 #t 1+0.0i))
  (known /just /known /hash #f 0.0 #t 0)
  "Smoosh meet works on unequal, comparable immutable hash tables")

(check-pred
  unknown?
  (sm (hash #f 0 #t 0+i) (hash #f 0 #t 1+i))
  "Smoosh meet is unknown on unequal, uncomparable immutable hash tables")

(check-equal?
  (s= (pw /hash #f 0 #t 0.0) (pw /hash #f 0.0 #t 0))
  (known /just /known /pw /hash #f 0 #t 0.0)
  "Path-related smoosh works on equal immutable hash tables")

(w- obj (pw /hash #f 0 #t 0.0)
  (check-eq?
    (known-value /just-value /known-value
      (s= obj (pw /hash #f 0.0 #t 0)))
    obj
    "Path-related smoosh preserves `eq?` when possible on equal immutable hash tables"))

(check-equal?
  (s= (pw /hash #f 0 #t 0.0) (pw /hash #f 1.0 #t 1+0.0i))
  (known /just /known /pw /hash #f 0 #t 0.0)
  "Path-related smoosh works on immutable hash tables with path-related elements")

(w- obj (pw /hash #f 0 #t 0.0)
  (check-eq?
    (known-value /just-value /known-value
      (s= obj (pw /hash #f 1.0 #t 1+0.0i)))
    obj
    "Path-related smoosh preserves `eq?` when possible on immutable hash tables with path-related elements"))

(check-pred
  unknown?
  (s= (pw /hash #f 0 #t 0+i) (pw /hash #f 0 #t 1+i))
  "Path-related smoosh is unknown on immutable hash tables with at least one pair of corresponding elements whose path-relatedness is unknown and no pairs whose path-relatedness is known false")

(check-equal?
  (s=
    (pw /hash #f path-failing-1 #t 0+i)
    (pw /hash #f path-failing-2 #t 1+i))
  (known /nothing)
  "Path-related smoosh fails on immutable hash tables with at least one pair of corresponding elements whose path-related smoosh fails, even if another pair's path-related smoosh result is unknown")

(check-equal?
  (s= (iw /hash #f 0 #t 0) (iw /hash #f 0 #t 0))
  (known /just /known /iw /hash #f 0 #t 0)
  "Info smoosh works on shallowly `chaperone=?` immutable hash tables whose elements are info smooshable")

(w- obj (iw /hash #f 0 #t 0)
  (check-eq?
    (known-value /just-value /known-value
      (s= obj (iw /hash #f 0 #t 0)))
    obj
    "Info smoosh preserves `eq?` when possible on shallowly `chaperone=?` immutable hash tables whose elements are info smooshable"))

(check-equal?
  (s= (iw /hash #f 0 #t 0) (iw /hash #f 0 #t 0.0))
  (known /nothing)
  "Info smoosh fails on shallowly `chaperone=?` immutable hash tables with a pair of corresponding elements whose info smoosh fails")

(check-pred
  unknown?
  (s= (iw ihash1-chap-chap) (iw ihash1-chap))
  "Info smoosh is unknown on non-shallowly-`chaperone=?` immutable hash tables even when they're shallowly `chaperone-of?` in one direction and have elements which are info smooshable")

(check-pred
  unknown?
  (s= (iw ihash1-chap) (iw ihash1-chap2))
  "Info smoosh is unknown on non-shallowly-`chaperone=?` immutable hash tables even when they're shallowly `equal-always?` and have elements which are info smooshable")

(check-equal?
  (sj (iw /hash #f 0 #t 0) (iw /hash #f 0 #t 0))
  (known /just /known /iw /hash #f 0 #t 0)
  "Info smoosh join works on shallowly `chaperone=?` immutable hash tables whose elements are info smoosh joinable")

(w- obj (iw /hash #f 0 #t 0)
  (check-eq?
    (known-value /just-value /known-value
      (sj obj (iw /hash #f 0 #t 0)))
    obj
    "Info smoosh join preserves `eq?` when possible on shallowly `chaperone=?` immutable hash tables whose elements are info smoosh joinable"))

(check-equal?
  (sj (iw ihash1-chap-chap) (iw ihash1-chap))
  (known /just /known /iw ihash1-chap-chap)
  "Info smoosh join works on shallowly `chaperone-of?` immutable hash tables even when they're not shallowly `chaperone=?`")

(check-eq?
  (info-wrapper-value /known-value /just-value /known-value
    (sj (iw ihash1-chap-chap) (iw ihash1-chap)))
  ihash1-chap-chap
  "Info smoosh join preserves `eq?` on shallowly `chaperone-of?` immutable hash tables even when they're not shallowly `chaperone=?`")

(check-equal?
  (sj (iw /hash #f 0 #t 0) (iw /hash #f 0 #t 0.0))
  (known /nothing)
  "Info smoosh join fails on immutable hash tables with at least one pair of corresponding elements whose info smoosh join fails")

(check-pred
  unknown?
  (sj (iw ihash1-chap) (iw ihash1-chap2))
  "Info smoosh join is unknown on non-shallowly-`chaperone-of?` immutable hash tables even when they're `equal-always?`")

(check-equal?
  (sm (iw /hash #f 0 #t 0) (iw /hash #f 0 #t 0))
  (known /just /known /iw /hash #f 0 #t 0)
  "Info smoosh meet works on shallowly `chaperone=?` immutable hash tables whose elements are info smoosh meetable")

(w- obj (iw /hash #f 0 #t 0)
  (check-eq?
    (known-value /just-value /known-value
      (sm obj (iw /hash #f 0 #t 0)))
    obj
    "Info smoosh meet preserves `eq?` when possible on shallowly `chaperone=?` immutable hash tables whose elements are info smoosh meetable"))

(check-equal?
  (sm (iw ihash1-chap-chap) (iw ihash1-chap))
  (known /just /known /iw ihash1-chap)
  "Info smoosh meet works on shallowly `chaperone-of?` immutable hash tables even when they're not shallowly `chaperone=?`")

(check-eq?
  (info-wrapper-value /known-value /just-value /known-value
    (sm (iw ihash1-chap-chap) (iw ihash1-chap)))
  ihash1-chap
  "Info smoosh meet preserves `eq?` on shallowly `chaperone-of?` immutable hash tables even when they're not shallowly `chaperone=?`")

(check-equal?
  (sm (iw /hash #f 0 #t 0) (iw /hash #f 0 #t 0.0))
  (known /nothing)
  "Info smoosh meet fails on immutable hash tables with at least one pair of corresponding elements whose info smoosh meet fails")

(check-pred
  unknown?
  (sm (iw ihash1-chap) (iw ihash1-chap2))
  "Info smoosh meet is unknown on non-shallowly-`chaperone-of?` immutable hash tables even when they're `equal-always?`")

(check-equal?
  (s= (pw /iw /hash #f 0 #t 0) (pw /iw /hash #f 0 #t 0))
  (known /just /known /pw /iw /hash #f 0 #t 0)
  "Path-related info smoosh works on immutable hash tables whose elements are path-related info smooshable")

(w- obj (pw /iw /hash #f 0 #t 0)
  (check-eq?
    (known-value /just-value /known-value
      (s= obj (pw /iw /hash #f 0 #t 0)))
    obj
    "Path-related info smoosh preserves `eq?` when possible on immutable hash tables whose elements are path-related info smooshable"))

(check-equal?
  (s= (pw /iw ihash1-chap-chap) (pw /iw ihash1-chap))
  (known /just /known /pw /iw ihash1-chap-chap)
  "Path-related info smoosh works on `equal-always?` immutable hash tables even when they're only shallowly `chaperone-of?` in one direction")

(check-eq?
  (info-wrapper-value /path-related-wrapper-value
    (known-value /just-value /known-value
      (s= (pw /iw ihash1-chap-chap) (pw /iw ihash1-chap))))
  ihash1-chap-chap
  "Path-related info smoosh preserves `eq?` on `equal-always?` immutable hash tables even when they're only shallowly `chaperone-of?` in one direction")

(check-equal?
  (s= (pw /iw ihash1-chap) (pw /iw ihash1-chap2))
  (known /just /known /pw /iw ihash1-chap)
  "Path-related info smoosh works on `equal-always?` immutable hash tables even when they're not shallowly `chaperone=?` in either direction")

(check-eq?
  (info-wrapper-value /path-related-wrapper-value
    (known-value /just-value /known-value
      (s= (pw /iw ihash1-chap) (pw /iw ihash1-chap2))))
  ihash1-chap
  "Path-related info smoosh preserves `eq?` on `equal-always?` immutable hash tables even when they're not shallowly `chaperone=?` in either direction")

(check-equal?
  (s= (pw /iw /hash #f 0 #t 0) (pw /iw /hash #f 0 #t 0.0))
  (known /nothing)
  "Path-related info smoosh fails on immutable hash tables with at least one pair of corresponding elements whose path-related info smoosh fails")


(check-equal?
  (s= (nothing) (nothing))
  (known /just /known /nothing)
  "Smoosh works on `nothing?` values")

(check-equal?
  (sj (nothing) (nothing))
  (known /just /known /nothing)
  "Smoosh join works on `nothing?` values")

(check-equal?
  (sm (nothing) (nothing))
  (known /just /known /nothing)
  "Smoosh meet works on `nothing?` values")

(check-equal?
  (s= (pw /nothing) (pw /nothing))
  (known /just /known /pw /nothing)
  "Path-related smoosh works on `nothing?` values")

(check-equal?
  (s= (iw /nothing) (iw /nothing))
  (known /just /known /iw /nothing)
  "Info smoosh works on `nothing?` values")

(check-equal?
  (sj (iw /nothing) (iw /nothing))
  (known /just /known /iw /nothing)
  "Info smoosh join works on `nothing?` values")

(check-equal?
  (sm (iw /nothing) (iw /nothing))
  (known /just /known /iw /nothing)
  "Info smoosh meet works on `nothing?` values")

(check-equal?
  (s= (pw /iw /nothing) (pw /iw /nothing))
  (known /just /known /pw /iw /nothing)
  "Path-related info smoosh works on `nothing?` values")


(check-equal?
  (s= (just 0) (just 0.0))
  (known /just /known /just 0)
  "Smoosh works on equal `just?` values")

(w- obj (just 0)
  (check-eq?
    (known-value /just-value /known-value /s= obj (just 0.0))
    obj
    "Smoosh preserves `eq?` when possible on equal `just?` values"))

(check-equal?
  (s= (just 0) (just 1))
  (known /nothing)
  "Smoosh fails on unequal `just?` values")

(check-equal?
  (sj (just 0) (just 0.0))
  (known /just /known /just 0)
  "Smoosh join works on equal `just?` values")

(w- obj (just 0)
  (check-eq?
    (known-value /just-value /known-value /sj obj (just 0.0))
    obj
    "Smoosh join preserves `eq?` when possible on equal `just?` values"))

(check-equal?
  (sj (just 1) (just 0.0))
  (known /just /known /just 1)
  "Smoosh join works on unequal, comparable `just?` values")

(check-pred
  unknown?
  (sj (just 0+i) (just 1+i))
  "Smoosh join is unknown on unequal, uncomparable `just?` values")

(check-equal?
  (sm (just 0) (just 0.0))
  (known /just /known /just 0)
  "Smoosh meet works on equal `just?` values")

(w- obj (just 0)
  (check-eq?
    (known-value /just-value /known-value /sm obj (just 0.0))
    obj
    "Smoosh meet preserves `eq?` when possible on equal `just?` values"))

(check-equal?
  (sm (just 1) (just 0.0))
  (known /just /known /just 0.0)
  "Smoosh meet works on unequal, comparable `just?` values")

(check-pred
  unknown?
  (sm (just 0+i) (just 1+i))
  "Smoosh meet is unknown on unequal, uncomparable `just?` values")

(check-equal?
  (s= (pw /just 0) (pw /just 0.0))
  (known /just /known /pw /just 0)
  "Path-related smoosh works on equal `just?` values")

(w- obj (pw /just 0)
  (check-eq?
    (known-value /just-value /known-value
      (s= obj (pw /just 0.0)))
    obj
    "Path-related smoosh preserves `eq?` when possible on equal `just?` values"))

(check-equal?
  (s= (pw /just 0) (pw /just 1.0))
  (known /just /known /pw /just 0)
  "Path-related smoosh works on `just?` values with path-related elements")

(w- obj (pw /just 0)
  (check-eq?
    (known-value /just-value /known-value
      (s= obj (pw /just 1.0)))
    obj
    "Path-related smoosh preserves `eq?` when possible on `just?` values with path-related elements"))

(check-pred
  unknown?
  (s= (pw /just 0+i) (pw /just 1+i))
  "Path-related smoosh is unknown on `just?` values with at least one pair of corresponding elements whose path-relatedness is unknown and no pairs whose path-relatedness is known false")

(check-equal?
  (s= (iw /just 0) (iw /just 0))
  (known /just /known /iw /just 0)
  "Info smoosh works on `just?` values whose elements are info smooshable")

(w- obj (iw /just 0)
  (check-eq?
    (known-value /just-value /known-value
      (s= obj (iw /just 0)))
    obj
    "Info smoosh preserves `eq?` when possible on `just?` values whose elements are info smooshable"))

(check-equal?
  (s= (iw /just 0) (iw /just 0.0))
  (known /nothing)
  "Info smoosh fails on `just?` values with a pair of corresponding elements whose info smoosh fails")

(check-equal?
  (sj (iw /just 0) (iw /just 0))
  (known /just /known /iw /just 0)
  "Info smoosh join works on `just?` values whose elements are info smoosh joinable")

(w- obj (iw /just 0)
  (check-eq?
    (known-value /just-value /known-value
      (sj obj (iw /just 0)))
    obj
    "Info smoosh join preserves `eq?` when possible on `just?` values whose elements are info smoosh joinable"))

(check-equal?
  (sj (iw /just 0) (iw /just 0.0))
  (known /nothing)
  "Info smoosh join fails on `just?` values with at least one pair of corresponding elements whose info smoosh join fails")

(check-equal?
  (sm (iw /just 0) (iw /just 0))
  (known /just /known /iw /just 0)
  "Info smoosh meet works on shallowly `chaperone=?` `just?` values whose elements are info smoosh meetable")

(w- obj (iw /just 0)
  (check-eq?
    (known-value /just-value /known-value
      (sm obj (iw /just 0)))
    obj
    "Info smoosh meet preserves `eq?` when possible on `just?` values whose elements are info smoosh meetable"))

(check-equal?
  (sm (iw /just 0) (iw /just 0.0))
  (known /nothing)
  "Info smoosh meet fails on `just?` values with at least one pair of corresponding elements whose info smoosh meet fails")

(check-equal?
  (s= (pw /iw /just 0) (pw /iw /just 0))
  (known /just /known /pw /iw /just 0)
  "Path-related info smoosh works on `just?` values whose elements are path-related info smooshable")

(w- obj (pw /iw /just 0)
  (check-eq?
    (known-value /just-value /known-value
      (s= obj (pw /iw /just 0)))
    obj
    "Path-related info smoosh preserves `eq?` when possible on `just?` values whose elements are path-related info smooshable"))

(check-equal?
  (s= (pw /iw /just 0) (pw /iw /just 0.0))
  (known /nothing)
  "Path-related info smoosh fails on `just?` values with at least one pair of corresponding elements whose path-related info smoosh fails")


(check-equal?
  (s= (nothing) (just 0))
  (known /nothing)
  "Smoosh fails on distinctive `maybe?` values")

(check-pred
  unknown?
  (sj (nothing) (just 0))
  "Smoosh join is unknown on distinctive `maybe?` values")

(check-pred
  unknown?
  (sm (nothing) (just 0))
  "Smoosh meet is unknown on distinctive `maybe?` values")

(check-pred
  unknown?
  (s= (pw /nothing) (pw /just 0))
  "Path-related smoosh is unknown on distinctive `maybe?` values")

(check-equal?
  (s= (iw /nothing) (iw /just 0))
  (known /nothing)
  "Info smoosh fails on distinctive `maybe?` values")

(check-equal?
  (sj (iw /nothing) (iw /just 0))
  (known /nothing)
  "Info smoosh join fails on distinctive `maybe?` values")

(check-equal?
  (sm (iw /nothing) (iw /just 0))
  (known /nothing)
  "Info smoosh meet fails on distinctive `maybe?` values")

(check-equal?
  (s= (pw /iw /nothing) (pw /iw /just 0))
  (known /nothing)
  "Path-related info smoosh fails on distinctive `maybe?` values")


(check-equal?
  (s= (trivial) (trivial))
  (known /just /known /trivial)
  "Smoosh works on `trivial?` values")

(check-equal?
  (sj (trivial) (trivial))
  (known /just /known /trivial)
  "Smoosh join works on `trivial?` values")

(check-equal?
  (sm (trivial) (trivial))
  (known /just /known /trivial)
  "Smoosh meet works on `trivial?` values")

(check-equal?
  (s= (pw /trivial) (pw /trivial))
  (known /just /known /pw /trivial)
  "Path-related smoosh works on `trivial?` values")

(check-equal?
  (s= (iw /trivial) (iw /trivial))
  (known /just /known /iw /trivial)
  "Info smoosh works on `trivial?` values")

(check-equal?
  (sj (iw /trivial) (iw /trivial))
  (known /just /known /iw /trivial)
  "Info smoosh join works on `trivial?` values")

(check-equal?
  (sm (iw /trivial) (iw /trivial))
  (known /just /known /iw /trivial)
  "Info smoosh meet works on `trivial?` values")

(check-equal?
  (s= (pw /iw /trivial) (pw /iw /trivial))
  (known /just /known /pw /iw /trivial)
  "Path-related info smoosh works on `trivial?` values")


(check-pred
  unknown?
  (known-value /just-value /known-value /s= (unknown) (unknown))
  "Smoosh works on `example-unknown?` values")

(check-pred
  unknown?
  (known-value /just-value /known-value /sj (unknown) (unknown))
  "Smoosh join works on `example-unknown?` values")

(check-pred
  unknown?
  (known-value /just-value /known-value /sm (unknown) (unknown))
  "Smoosh meet works on `example-unknown?` values")

(check-pred
  unknown?
  (path-related-wrapper-value /known-value /just-value /known-value
    (s= (pw /unknown) (pw /unknown)))
  "Path-related smoosh works on `example-unknown?` values")

(check-pred
  unknown?
  (info-wrapper-value /known-value /just-value /known-value
    (s= (iw /unknown) (iw /unknown)))
  "Info smoosh works on `example-unknown?` values")

(check-pred
  unknown?
  (info-wrapper-value /known-value /just-value /known-value
    (sj (iw /unknown) (iw /unknown)))
  "Info smoosh join works on `example-unknown?` values")

(check-pred
  unknown?
  (info-wrapper-value /known-value /just-value /known-value
    (sm (iw /unknown) (iw /unknown)))
  "Info smoosh meet works on `example-unknown?` values")

(check-pred
  unknown?
  (info-wrapper-value /path-related-wrapper-value
    (known-value /just-value /known-value
      (s= (pw /iw /unknown) (pw /iw /unknown))))
  "Path-related info smoosh works on `example-unknown?` values")


(check-equal?
  (s= (known 0) (known 0.0))
  (known /just /known /known 0)
  "Smoosh works on equal `known?` values")

(w- obj (known 0)
  (check-eq?
    (known-value /just-value /known-value /s= obj (known 0.0))
    obj
    "Smoosh preserves `eq?` when possible on equal `known?` values"))

(check-equal?
  (s= (known 0) (known 1))
  (known /nothing)
  "Smoosh fails on unequal `known?` values")

(check-equal?
  (sj (known 0) (known 0.0))
  (known /just /known /known 0)
  "Smoosh join works on equal `known?` values")

(w- obj (known 0)
  (check-eq?
    (known-value /just-value /known-value /sj obj (known 0.0))
    obj
    "Smoosh join preserves `eq?` when possible on equal `known?` values"))

(check-equal?
  (sj (known 1) (known 0.0))
  (known /just /known /known 1)
  "Smoosh join works on unequal, comparable `known?` values")

(check-pred
  unknown?
  (sj (known 0+i) (known 1+i))
  "Smoosh join is unknown on unequal, uncomparable `known?` values")

(check-equal?
  (sm (known 0) (known 0.0))
  (known /just /known /known 0)
  "Smoosh meet works on equal `known?` values")

(w- obj (known 0)
  (check-eq?
    (known-value /just-value /known-value /sm obj (known 0.0))
    obj
    "Smoosh meet preserves `eq?` when possible on equal `known?` values"))

(check-equal?
  (sm (known 1) (known 0.0))
  (known /just /known /known 0.0)
  "Smoosh meet works on unequal, comparable `known?` values")

(check-pred
  unknown?
  (sm (known 0+i) (known 1+i))
  "Smoosh meet is unknown on unequal, uncomparable `known?` values")

(check-equal?
  (s= (pw /known 0) (pw /known 0.0))
  (known /just /known /pw /known 0)
  "Path-related smoosh works on equal `known?` values")

(w- obj (pw /known 0)
  (check-eq?
    (known-value /just-value /known-value
      (s= obj (pw /known 0.0)))
    obj
    "Path-related smoosh preserves `eq?` when possible on equal `known?` values"))

(check-equal?
  (s= (pw /known 0) (pw /known 1.0))
  (known /just /known /pw /known 0)
  "Path-related smoosh works on `known?` values with path-related elements")

(w- obj (pw /known 0)
  (check-eq?
    (known-value /just-value /known-value
      (s= obj (pw /known 1.0)))
    obj
    "Path-related smoosh preserves `eq?` when possible on `known?` values with path-related elements"))

(check-pred
  unknown?
  (s= (pw /known 0+i) (pw /known 1+i))
  "Path-related smoosh is unknown on `known?` values with at least one pair of corresponding elements whose path-relatedness is unknown and no pairs whose path-relatedness is known false")

(check-equal?
  (s= (iw /known 0) (iw /known 0))
  (known /just /known /iw /known 0)
  "Info smoosh works on `known?` values whose elements are info smooshable")

(w- obj (iw /known 0)
  (check-eq?
    (known-value /just-value /known-value
      (s= obj (iw /known 0)))
    obj
    "Info smoosh preserves `eq?` when possible on `known?` values whose elements are info smooshable"))

(check-equal?
  (s= (iw /known 0) (iw /known 0.0))
  (known /nothing)
  "Info smoosh fails on `known?` values with a pair of corresponding elements whose info smoosh fails")

(check-equal?
  (sj (iw /known 0) (iw /known 0))
  (known /just /known /iw /known 0)
  "Info smoosh join works on `known?` values whose elements are info smoosh joinable")

(w- obj (iw /known 0)
  (check-eq?
    (known-value /just-value /known-value
      (sj obj (iw /known 0)))
    obj
    "Info smoosh join preserves `eq?` when possible on `known?` values whose elements are info smoosh joinable"))

(check-equal?
  (sj (iw /known 0) (iw /known 0.0))
  (known /nothing)
  "Info smoosh join fails on `known?` values with at least one pair of corresponding elements whose info smoosh join fails")

(check-equal?
  (sm (iw /known 0) (iw /known 0))
  (known /just /known /iw /known 0)
  "Info smoosh meet works on `known?` values whose elements are info smoosh meetable")

(w- obj (iw /known 0)
  (check-eq?
    (known-value /just-value /known-value
      (sm obj (iw /known 0)))
    obj
    "Info smoosh meet preserves `eq?` when possible on `known?` values whose elements are info smoosh meetable"))

(check-equal?
  (sm (iw /known 0) (iw /known 0.0))
  (known /nothing)
  "Info smoosh meet fails on `known?` values with at least one pair of corresponding elements whose info smoosh meet fails")

(check-equal?
  (s= (pw /iw /known 0) (pw /iw /known 0))
  (known /just /known /pw /iw /known 0)
  "Path-related info smoosh works on `known?` values whose elements are path-related info smooshable")

(w- obj (pw /iw /known 0)
  (check-eq?
    (known-value /just-value /known-value
      (s= obj (pw /iw /known 0)))
    obj
    "Path-related info smoosh preserves `eq?` when possible on `known?` values whose elements are path-related info smooshable"))

(check-equal?
  (s= (pw /iw /known 0) (pw /iw /known 0.0))
  (known /nothing)
  "Path-related info smoosh fails on `known?` values with at least one pair of corresponding elements whose path-related info smoosh fails")


(check-pred
  unknown?
  (s= (unknown) (known 0))
  "Smoosh is unknown on an `example-unknown?` value vs a `known?` value")

(check-pred
  unknown?
  (sj (unknown) (known 0))
  "Smoosh join is unknown on an `example-unknown?` value vs a `known?` value")

(check-pred
  unknown?
  (sm (unknown) (known 0))
  "Smoosh meet is unknown on an `example-unknown?` value vs a `known?` value")

(check-pred
  unknown?
  (s= (pw /unknown) (pw /known 0))
  "Path-related smoosh is unknown on an `example-unknown?` value vs a `known?` value")

(check-equal?
  (s= (iw /unknown) (iw /known 0))
  (known /nothing)
  "Info smoosh fails on an `example-unknown?` value vs a `known?` value")

(check-equal?
  (sj (iw /unknown) (iw /known 0))
  (known /just /known /iw /known 0)
  "Info smoosh join succeeds on an `example-unknown?` value vs a `known?` value")

(w- obj (iw /known 0)
  (check-eq?
    (known-value /just-value /known-value /sj (iw /unknown) obj)
    obj
    "Info smoosh join preserves `eq?` on an `example-unknown?` value vs a `known?` value"))

(check-pred
  unknown?
  (info-wrapper-value /known-value /just-value /known-value
    (sm (iw /unknown) (iw /known 0)))
  "Info smoosh meet succeeds on an `example-unknown?` value vs a `known?` value")

(w- obj (iw /unknown)
  (check-eq?
    (known-value /just-value /known-value /sm obj (iw /known 0))
    obj
    "Info smoosh meet succeeds on an `example-unknown?` value vs a `known?` value"))

(check-pred
  unknown?
  (info-wrapper-value /path-related-wrapper-value
    (known-value /just-value /known-value
      (s= (pw /iw /unknown) (pw /iw /known 0))))
  "Path-related info smoosh succeeds on an `example-unknown?` value vs a `known?` value")

(w- obj (pw /iw /unknown)
  (check-eq?
    (known-value /just-value /known-value /s= obj (pw /iw /known 0))
    obj
    "Path-related info smoosh succeeds on an `example-unknown?` value vs a `known?` value"))

(check-equal?
  (s= (iw /iw /unknown) (iw /iw /known 0))
  (known /nothing)
  "Info info smoosh fails on an `example-unknown?` value vs a `known?` value")

(check-equal?
  (sj (iw /iw /unknown) (iw /iw /known 0))
  (known /nothing)
  "Info info smoosh join fails on an `example-unknown?` value vs a `known?` value")

(check-equal?
  (sm (iw /iw /unknown) (iw /iw /known 0))
  (known /nothing)
  "Info info smoosh meet fails on an `example-unknown?` value vs a `known?` value")

(check-equal?
  (s= (pw /iw /iw /unknown) (pw /iw /iw /known 0))
  (known /nothing)
  "Path-related info info smoosh fails on an `example-unknown?` value vs a `known?` value")


(check-equal?
  (s= (gloss #f 0 #t 0.0) (gloss #f 0.0 #t 0))
  (known /just /known /gloss #f 0 #t 0.0)
  "Smoosh works on equal `gloss?` values")

(w- obj (gloss #f 0 #t 0.0)
  (check-eq?
    (known-value /just-value /known-value /s= obj (gloss #f 0.0 #t 0))
    obj
    "Smoosh preserves `eq?` when possible on equal `gloss?` values"))

(check-equal?
  (s= (gloss #f 0 #t 0) (gloss #f 1 #t 0))
  (known /nothing)
  "Smoosh fails on unequal `gloss?` values")

(check-equal?
  (sj (gloss #f 0 #t 0.0) (gloss #f 0.0 #t 0))
  (known /just /known /gloss #f 0 #t 0.0)
  "Smoosh join works on equal `gloss?` values")

(w- obj (gloss #f 0 #t 0.0)
  (check-eq?
    (known-value /just-value /known-value /sj obj (gloss #f 0.0 #t 0))
    obj
    "Smoosh join preserves `eq?` when possible on equal `gloss?` values"))

(check-equal?
  (sj (gloss #f 1 #t 0) (gloss #f 0.0 #t 1+0.0i))
  (known /just /known /gloss #f 1 #t 1+0.0i)
  "Smoosh join works on unequal, comparable `gloss?` values")

(check-pred
  unknown?
  (sj (gloss #f 0 #t 0+i) (gloss #f 0 #t 1+i))
  "Smoosh join is unknown on unequal, uncomparable `gloss?` values")

(check-equal?
  (sm (gloss #f 0 #t 0.0) (gloss #f 0.0 #t 0))
  (known /just /known /gloss #f 0 #t 0.0)
  "Smoosh meet works on equal `gloss?` values")

(w- obj (gloss #f 0 #t 0.0)
  (check-eq?
    (known-value /just-value /known-value /sm obj (gloss #f 0.0 #t 0))
    obj
    "Smoosh meet preserves `eq?` when possible on equal `gloss?` values"))

(check-equal?
  (sm (gloss #f 1 #t 0) (gloss #f 0.0 #t 1+0.0i))
  (known /just /known /gloss #f 0.0 #t 0)
  "Smoosh meet works on unequal, comparable `gloss?` values")

(check-pred
  unknown?
  (sm (gloss #f 0 #t 0+i) (gloss #f 0 #t 1+i))
  "Smoosh meet is unknown on unequal, uncomparable `gloss?` values")

(check-equal?
  (s= (pw /gloss #f 0 #t 0.0) (pw /gloss #f 0.0 #t 0))
  (known /just /known /pw /gloss #f 0 #t 0.0)
  "Path-related smoosh works on equal `gloss?` values")

(w- obj (pw /gloss #f 0 #t 0.0)
  (check-eq?
    (known-value /just-value /known-value
      (s= obj (pw /gloss #f 0.0 #t 0)))
    obj
    "Path-related smoosh preserves `eq?` when possible on equal `gloss?` values"))

(check-equal?
  (s= (pw /gloss #f 0 #t 0.0) (pw /gloss #f 1.0 #t 1+0.0i))
  (known /just /known /pw /gloss #f 0 #t 0.0)
  "Path-related smoosh works on `gloss?` values with path-related elements")

(w- obj (pw /gloss #f 0 #t 0.0)
  (check-eq?
    (known-value /just-value /known-value
      (s= obj (pw /gloss #f 1.0 #t 1+0.0i)))
    obj
    "Path-related smoosh preserves `eq?` when possible on `gloss?` values with path-related elements"))

(check-pred
  unknown?
  (s= (pw /gloss #f 0 #t 0+i) (pw /gloss #f 0 #t 1+i))
  "Path-related smoosh is unknown on `gloss?` values with at least one pair of corresponding elements whose path-relatedness is unknown and no pairs whose path-relatedness is known false")

(check-equal?
  (s=
    (pw /gloss #f path-failing-1 #t 0+i)
    (pw /gloss #f path-failing-2 #t 1+i))
  (known /nothing)
  "Path-related smoosh fails on `gloss?` values with at least one pair of corresponding elements whose path-related smoosh fails, even if another pair's path-related smoosh result is unknown")

(check-equal?
  (s= (iw /gloss #f 0 #t 0) (iw /gloss #f 0 #t 0))
  (known /just /known /iw /gloss #f 0 #t 0)
  "Info smoosh works on shallowly `chaperone=?` `gloss?` values whose elements are info smooshable")

(w- obj (iw /gloss #f 0 #t 0)
  (check-eq?
    (known-value /just-value /known-value
      (s= obj (iw /gloss #f 0 #t 0)))
    obj
    "Info smoosh preserves `eq?` when possible on shallowly `chaperone=?` `gloss?` values whose elements are info smooshable"))

(check-equal?
  (s= (iw /gloss #f 0 #t 0) (iw /gloss #f 0 #t 0.0))
  (known /nothing)
  "Info smoosh fails on shallowly `chaperone=?` `gloss?` values with a pair of corresponding elements whose info smoosh fails")

(check-equal?
  (sj (iw /gloss #f 0 #t 0) (iw /gloss #f 0 #t 0))
  (known /just /known /iw /gloss #f 0 #t 0)
  "Info smoosh join works on shallowly `chaperone=?` `gloss?` values whose elements are info smoosh joinable")

(w- obj (iw /gloss #f 0 #t 0)
  (check-eq?
    (known-value /just-value /known-value
      (sj obj (iw /gloss #f 0 #t 0)))
    obj
    "Info smoosh join preserves `eq?` when possible on shallowly `chaperone=?` `gloss?` values whose elements are info smoosh joinable"))

(check-equal?
  (sj (iw /gloss #f 0 #t 0) (iw /gloss #f 0 #t 0.0))
  (known /nothing)
  "Info smoosh join fails on `gloss?` values with at least one pair of corresponding elements whose info smoosh join fails")

(check-equal?
  (sm (iw /gloss #f 0 #t 0) (iw /gloss #f 0 #t 0))
  (known /just /known /iw /gloss #f 0 #t 0)
  "Info smoosh meet works on shallowly `chaperone=?` `gloss?` values whose elements are info smoosh meetable")

(w- obj (iw /gloss #f 0 #t 0)
  (check-eq?
    (known-value /just-value /known-value
      (sm obj (iw /gloss #f 0 #t 0)))
    obj
    "Info smoosh meet preserves `eq?` when possible on shallowly `chaperone=?` `gloss?` values whose elements are info smoosh meetable"))

(check-equal?
  (sm (iw /gloss #f 0 #t 0) (iw /gloss #f 0 #t 0.0))
  (known /nothing)
  "Info smoosh meet fails on `gloss?` values with at least one pair of corresponding elements whose info smoosh meet fails")

(check-equal?
  (s= (pw /iw /gloss #f 0 #t 0) (pw /iw /gloss #f 0 #t 0))
  (known /just /known /pw /iw /gloss #f 0 #t 0)
  "Path-related info smoosh works on `gloss?` values whose elements are path-related info smooshable")

(w- obj (pw /iw /gloss #f 0 #t 0)
  (check-eq?
    (known-value /just-value /known-value
      (s= obj (pw /iw /gloss #f 0 #t 0)))
    obj
    "Path-related info smoosh preserves `eq?` when possible on `gloss?` values whose elements are path-related info smooshable"))

(check-equal?
  (s= (pw /iw /gloss #f 0 #t 0) (pw /iw /gloss #f 0 #t 0.0))
  (known /nothing)
  "Path-related info smoosh fails on `gloss?` values with at least one pair of corresponding elements whose path-related info smoosh fails")


(check-equal?
  (s= eaw1 eaw2)
  (known /just /known eaw1)
  "Smoosh works on `equal-always?` `equal-always-wrapper?` values")

(check-equal?
  (s= eaw1 eaw-different)
  (known /nothing)
  "Smoosh fails on non-`equal-always?` `equal-always-wrapper?` values")

(check-equal?
  (sj eaw1 eaw2)
  (known /just /known eaw1)
  "Smoosh join works on `equal-always?` `equal-always-wrapper?` values")

(check-pred
  unknown?
  (sj eaw1 eaw-different)
  "Smoosh join is unknown on non-`equal-always?` `equal-always-wrapper?` values")

(check-equal?
  (sm eaw1 eaw2)
  (known /just /known eaw1)
  "Smoosh meet works on `equal-always?` `equal-always-wrapper?` values")

(check-pred
  unknown?
  (sm eaw1 eaw-different)
  "Smoosh meet is unknown on non-`equal-always?` `equal-always-wrapper?` values")

(check-equal?
  (s= (pw eaw1) (pw eaw2))
  (known /just /known /pw eaw1)
  "Path-related smoosh works on `equal-always?` `equal-always-wrapper?` values")

(check-pred
  unknown?
  (s= (pw eaw1) (pw eaw-different))
  "Path-related smoosh is unknown on non-`equal-always?` `equal-always-wrapper?` values")

(check-equal?
  (s= (iw eaw1) (iw eaw2))
  (known /just /known /iw eaw1)
  "Info smoosh works on `equal-always?` `equal-always-wrapper?` values")

(check-equal?
  (s= (iw eaw1) (iw eaw-different))
  (known /nothing)
  "Info smoosh fails on non-`equal-always?` `equal-always-wrapper?` values")

(check-equal?
  (sj (iw eaw1) (iw eaw2))
  (known /just /known /iw eaw1)
  "Info smoosh join works on `equal-always?` `equal-always-wrapper?` values")

(check-equal?
  (sj (iw eaw1) (iw eaw-different))
  (known /nothing)
  "Info smoosh join fails on non-`equal-always?` `equal-always-wrapper?` values")

(check-equal?
  (sm (iw eaw1) (iw eaw2))
  (known /just /known /iw eaw1)
  "Info smoosh meet works on `equal-always?` `equal-always-wrapper?` values")

(check-equal?
  (sm (iw eaw1) (iw eaw-different))
  (known /nothing)
  "Info smoosh meet fails on non-`equal-always?` `equal-always-wrapper?` values")

(check-equal?
  (s= (pw /iw eaw1) (pw /iw eaw2))
  (known /just /known /pw /iw eaw1)
  "Path-related info smoosh works on `equal-always?` `equal-always-wrapper?` values")

(check-equal?
  (s= (pw /iw eaw1) (pw /iw eaw-different))
  (known /nothing)
  "Path-related info smoosh fails on non-`equal-always?` `equal-always-wrapper?` values")


(check-equal?
  (s= (indistinct-wrapper eaw1) (indistinct-wrapper eaw2))
  (known /just /known /indistinct-wrapper eaw1)
  "Smoosh works on `indistinct-wrapper?` values if it works on their elements")

(check-pred
  unknown?
  (s= (indistinct-wrapper eaw1) (indistinct-wrapper eaw-different))
  "Smoosh is unknown on `indistinct-wrapper?` values if it fails on their elements")

(check-pred
  unknown?
  (s= (indistinct-wrapper +nan.0) (indistinct-wrapper +nan.0))
  "Smoosh is unknown on `indistinct-wrapper?` values if it's unknown on their elements")

(check-equal?
  (sj (indistinct-wrapper eaw1) (indistinct-wrapper eaw2))
  (known /just /known /indistinct-wrapper eaw1)
  "Smoosh join works on `indistinct-wrapper?` values if it works on their elements")

(check-pred
  unknown?
  (sj
    (indistinct-wrapper path-failing-1)
    (indistinct-wrapper path-failing-2))
  "Smoosh join is unknown on `indistinct-wrapper?` values if it fails on their elements")

(check-pred
  unknown?
  (sj (indistinct-wrapper eaw1) (indistinct-wrapper eaw-different))
  "Smoosh join is unknown on `indistinct-wrapper?` values if it's unknown on their elements")

(check-equal?
  (sm (indistinct-wrapper eaw1) (indistinct-wrapper eaw2))
  (known /just /known /indistinct-wrapper eaw1)
  "Smoosh meet works on `indistinct-wrapper?` values if it works on their elements")

(check-pred
  unknown?
  (sm
    (indistinct-wrapper path-failing-1)
    (indistinct-wrapper path-failing-2))
  "Smoosh meet is unknown on `indistinct-wrapper?` values if it fails on their elements")

(check-pred
  unknown?
  (sm (indistinct-wrapper eaw1) (indistinct-wrapper eaw-different))
  "Smoosh meet is unknown on `indistinct-wrapper?` values if it's unknown on their elements")

(check-equal?
  (s= (pw /indistinct-wrapper eaw1) (pw /indistinct-wrapper eaw2))
  (known /just /known /pw /indistinct-wrapper eaw1)
  "Path-related smoosh works on `indistinct-wrapper?` values if it works on their elements")

(check-pred
  unknown?
  (s=
    (pw /indistinct-wrapper path-failing-1)
    (pw /indistinct-wrapper path-failing-2))
  "Path-related smoosh is unknown on `indistinct-wrapper?` values if it fails on their elements")

(check-pred
  unknown?
  (s=
    (pw /indistinct-wrapper eaw1)
    (pw /indistinct-wrapper eaw-different))
  "Path-related smoosh is unknown on `indistinct-wrapper?` values if it's unknown on their elements")

(check-equal?
  (s= (iw /indistinct-wrapper eaw1) (iw /indistinct-wrapper eaw2))
  (known /just /known /iw /indistinct-wrapper eaw1)
  "Info smoosh works on `indistinct-wrapper?` values if it works on their elements")

(check-pred
  unknown?
  (s=
    (iw /indistinct-wrapper eaw1)
    (iw /indistinct-wrapper eaw-different))
  "Info smoosh is unknown on `indistinct-wrapper?` values if it fails on their elements")

(check-pred
  unknown?
  (s= (iw /indistinct-wrapper +nan.0) (iw /indistinct-wrapper +nan.0))
  "Info smoosh is unknown on `indistinct-wrapper?` values if it's unknown on their elements")

(check-equal?
  (sj (iw /indistinct-wrapper eaw1) (iw /indistinct-wrapper eaw2))
  (known /just /known /iw /indistinct-wrapper eaw1)
  "Info smoosh join works on `indistinct-wrapper?` values if it works on their elements")

(check-pred
  unknown?
  (sj
    (iw /indistinct-wrapper eaw1)
    (iw /indistinct-wrapper eaw-different))
  "Info smoosh join is unknown on `indistinct-wrapper?` values if it fails on their elements")

(check-pred
  unknown?
  (sj (iw /indistinct-wrapper +nan.0) (iw /indistinct-wrapper +nan.0))
  "Info smoosh join is unknown on `indistinct-wrapper?` values if it's unknown on their elements")

(check-equal?
  (sm (iw /indistinct-wrapper eaw1) (iw /indistinct-wrapper eaw2))
  (known /just /known /iw /indistinct-wrapper eaw1)
  "Info smoosh meet works on `indistinct-wrapper?` values if it works on their elements")

(check-pred
  unknown?
  (sm
    (iw /indistinct-wrapper eaw1)
    (iw /indistinct-wrapper eaw-different))
  "Info smoosh meet is unknown on `indistinct-wrapper?` values if it fails on their elements")

(check-pred
  unknown?
  (sm (iw /indistinct-wrapper +nan.0) (iw /indistinct-wrapper +nan.0))
  "Info smoosh meet is unknown on `indistinct-wrapper?` values if it's unknown on their elements")

(check-equal?
  (s=
    (pw /iw /indistinct-wrapper eaw1)
    (pw /iw /indistinct-wrapper eaw2))
  (known /just /known /pw /iw /indistinct-wrapper eaw1)
  "Path-related info smoosh works on `indistinct-wrapper?` values if it works on their elements")

(check-pred
  unknown?
  (s=
    (pw /iw /indistinct-wrapper eaw1)
    (pw /iw /indistinct-wrapper eaw-different))
  "Path-related info smoosh is unknown on `indistinct-wrapper?` values if it fails on their elements")

(check-pred
  unknown?
  (s=
    (pw /iw /indistinct-wrapper +nan.0)
    (pw /iw /indistinct-wrapper +nan.0))
  "Path-related info smoosh is unknown on `indistinct-wrapper?` values if it's unknown on their elements")


; TODO SMOOSH (currently basically done): Implement smooshing tests
; for values of the following types:
;
;   - (Done) Mutable strings, mutable byte strings, mutable boxes,
;     mutable vectors, prefab structs with mutable fields, and mutable
;     hash tables.
;
;   - (Done) Flvectors and fxvectors.
;
;   - (Done) Symbols and keywords.
;
;   - (Done) Empty lists.
;
;   - (Done) Cons cells.
;
;   - (Done) Booleans.
;
;   - (Done) Numbers with NaN parts.
;
;   - (Done) Numbers with no NaN parts.
;
;   - (Done) NaN extflonums.
;
;   - (Done) Non-NaN extflonums.
;
;   - (Done) Characters, immutable strings, and immutable byte
;     strings.
;
;   - (Done) Regular expressions (`regexp?`) and compiled code
;     expressions (`compiled-expression?`).
;
;   - (Done) Immutable boxes.
;
;   - (Done) Immutable vectors.
;
;   - (Done) Prefab structs with no mutable fields.
;
;   - (Done) Immutable hash tables. (TODO SMOOSH: But maybe we should
;     test more than one hash table comparison function and test what
;     happens with keys that are equal according to the comparison
;     function but not according to smooshing.)
;
;   - (Done) `maybe?` values.
;
;   - (Done) `trivial?` values.
;
;   - (Done) `example-unknown?` values and `known?` values.
;
;   - (Done) `gloss?` values.
;
;   - (Done) `equal-always-wrapper?` values.
;
;   - (Done) `indistinct-wrapper?` values.
