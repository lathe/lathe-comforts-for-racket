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
(require lathe-comforts/trivial)

; (We provide nothing from this module.)



(define (smoosh-report-exercise report)
  (w- kpmkp-exercise-and-force
    (fn kpmkp
      (knowable-map (force kpmkp) /fn kpm
        (maybe-map kpm /fn kp
          (delay/strict /force kp))))
  /w- path-related-kpmk
    (kpmkp-exercise-and-force
      (smoosh-report-path-related-knowable-promise-maybe-knowable-promise
        report))
  /w- join-kpmk
    (kpmkp-exercise-and-force
      (smoosh-report-join-knowable-promise-maybe-knowable-promise
        report))
  /w- meet-kpmk
    (kpmkp-exercise-and-force
      (smoosh-report-meet-knowable-promise-maybe-knowable-promise
        report))
  /w- ==-kpmk
    (kpmkp-exercise-and-force
      (smoosh-report-==-knowable-promise-maybe-knowable-promise
        report))
  /begin
    (expect ==-kpmk (known /just _) (void) /begin
      (mat path-related-kpmk (known /just _) (void)
        (error "expected == to imply path-related"))
      (mat join-kpmk (known /just _) (void)
        (error "expected == to imply join"))
      (mat meet-kpmk (known /just _) (void)
        (error "expected == to imply meet")))
    (expect join-kpmk (known /just _) (void) /begin
      (mat path-related-kpmk (known /just _) (void)
        (error "expected join to imply path-related")))
    (expect meet-kpmk (known /just _) (void) /begin
      (mat path-related-kpmk (known /just _) (void)
        (error "expected meet to imply path-related")))
    report))

(define (smoosh-and-comparison-of-two-report-exercise report)
  (w- smoosh-report
    (smoosh-report-exercise
      (smoosh-and-comparison-of-two-report-get-smoosh-report report))
  /w- <=?-k
    (force
      (smoosh-and-comparison-of-two-report-<=?-knowable-promise
        report))
  /w- >=?-k
    (force
      (smoosh-and-comparison-of-two-report->=?-knowable-promise
        report))
  /w- path-related-kpmk
    (force
      (smoosh-report-path-related-knowable-promise-maybe-knowable-promise
        smoosh-report))
  /w- join-kpmk
    (force
      (smoosh-report-join-knowable-promise-maybe-knowable-promise
        smoosh-report))
  /w- meet-kpmk
    (force
      (smoosh-report-meet-knowable-promise-maybe-knowable-promise
        smoosh-report))
  /w- ==-kpmk
    (force
      (smoosh-report-==-knowable-promise-maybe-knowable-promise
        smoosh-report))
  /begin
    (expect ==-kpmk (known /just _) (void) /begin
      (mat <=?-k (known #t) (void)
        (error "expected == to imply <="))
      (mat >=?-k (known #t) (void)
        (error "expected == to imply >=")))
    (expect <=?-k (known #t) (void) /begin
      (mat join-kpmk (known /just _) (void)
        (error "expected <= to imply join"))
      (mat meet-kpmk (known /just _) (void)
        (error "expected <= to imply meet")))
    (expect >=?-k (known #t) (void) /begin
      (mat join-kpmk (known /just _) (void)
        (error "expected >= to imply join"))
      (mat meet-kpmk (known /just _) (void)
        (error "expected >= to imply meet")))
    report))

(define (smoosh-and-comparison-of-two-reports-exercise reports)
  ; TODO: Also verify that the results for any info level beyond N are
  ; only known if the result of the == smoosh for info level N is a
  ; known success.
  (sequence-map
    (fn report /smoosh-and-comparison-of-two-report-exercise report)
    reports))


(define (kpmkp->kmk kpmkp)
  (knowable-map (force kpmkp) /fn kpm
    (maybe-map kpm /fn kp
      (force kp))))

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
  (kpmkp->kmk
    (smoosh-report-==-knowable-promise-maybe-knowable-promise
      (smooshable-exercise-first-smoosh-and-comparison-of-two-report
        a
        b))))

(define (smooshable-join-exercise-knowable-maybe-knowable a b)
  (kpmkp->kmk
    (smoosh-report-join-knowable-promise-maybe-knowable-promise
      (smooshable-exercise-first-smoosh-and-comparison-of-two-report
        a
        b))))

(define (smooshable-meet-exercise-knowable-maybe-knowable a b)
  (kpmkp->kmk
    (smoosh-report-meet-knowable-promise-maybe-knowable-promise
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

(check-equal?
  (s= mhash1 mhash2)
  (known /nothing)
  "Smoosh fails on non-`equal-always?` mutable hash tables")

(check-equal?
  (sj mhash1 mhash1)
  (known /just /known mhash1)
  "Smoosh join works on `equal-always?` mutable hash tables")

(check-pred
  unknown?
  (sj mhash1 mhash2)
  "Smoosh join is unknown on non-`equal-always?` mutable hash tables")

(check-equal?
  (sm mhash1 mhash1)
  (known /just /known mhash1)
  "Smoosh meet works on `equal-always?` mutable hash tables")

(check-pred
  unknown?
  (sm mhash1 mhash2)
  "Smoosh meet is unknown on non-`equal-always?` mutable hash tables")

(check-equal?
  (s= (pw mhash1) (pw mhash1))
  (known /just /known /pw mhash1)
  "Path-related smoosh works on `equal-always?` mutable hash tables")

(check-pred
  unknown?
  (s= (pw mhash1) (pw mhash2))
  "Path-related smoosh is unknown on non-`equal-always?` mutable hash tables")

(check-equal?
  (s= (iw mhash1) (iw mhash1))
  (known /just /known /iw mhash1)
  "Info smoosh works on `eq?` mutable hash tables")

(check-equal?
  (s= (iw mhash1) (iw mhash2))
  (known /nothing)
  "Info smoosh fails on non-`eq?` mutable hash tables")

(check-equal?
  (sj (iw mhash1) (iw mhash1))
  (known /just /known /iw mhash1)
  "Info smoosh join works on `eq?` mutable hash tables")

(check-equal?
  (sj (iw mhash1) (iw mhash2))
  (known /nothing)
  "Info smoosh join fails on non-`eq?` mutable hash tables")

(check-equal?
  (sm (iw mhash1) (iw mhash1))
  (known /just /known /iw mhash1)
  "Info smoosh meet works on `eq?` mutable hash tables")

(check-equal?
  (sm (iw mhash1) (iw mhash2))
  (known /nothing)
  "Info smoosh meet fails on non-`eq?` mutable hash tables")

(check-equal?
  (s= (pw /iw mhash1) (pw /iw mhash1))
  (known /just /known /pw /iw mhash1)
  "Path-related info smoosh works on `eq?` mutable hash tables")

(check-equal?
  (s= (pw /iw mhash1) (pw /iw mhash2))
  (known /nothing)
  "Path-related info smoosh fails on non-`eq?` mutable hash tables")


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

(w- c (cons 0 0.0)
  (check-eq?
    (known-value /just-value /known-value /s= c (cons 0.0 0))
    c
    "Smoosh preserves `eq?` when possible on equal cons cells"))

(check-equal?
  (s= (cons 0 0) (cons 1 0))
  (known /nothing)
  "Smoosh fails on unequal cons cells")

(check-equal?
  (sj (cons 0 0.0) (cons 0.0 0))
  (known /just /known /cons 0 0.0)
  "Smoosh join works on equal cons cells")

(w- c (cons 0 0.0)
  (check-eq?
    (known-value /just-value /known-value /sj c (cons 0.0 0))
    c
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

(w- c (cons 0 0.0)
  (check-eq?
    (known-value /just-value /known-value /sm c (cons 0.0 0))
    c
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

(w- c (pw /cons 0 0.0)
  (check-eq?
    (path-related-wrapper-value
      (known-value /just-value /known-value /s= c (pw /cons 0.0 0)))
    (path-related-wrapper-value c)
    "Path-related smoosh preserves `eq?` when possible on equal cons cells"))

(check-equal?
  (s= (pw /cons 0 0.0) (pw /cons 1.0 1+0.0i))
  (known /just /known /pw /cons 0 0.0)
  "Path-related smoosh works on cons cells with path-related elements")

(w- c (pw /cons 0 0.0)
  (check-eq?
    (known-value /just-value /known-value /s= c (pw /cons 1.0 1+0.0i))
    c
    "Path-related smoosh preserves `eq?` when possible on cons cells with path-related elements"))

(check-pred
  unknown?
  (s= (pw /cons 0 0+i) (pw /cons 0 1+i))
  "Path-related smoosh is unknown on cons cells with at least one pair of corresponding elements whose path-relatedness is unknown and no pairs whose path-relatedness is known false")

; TODO SMOOSH: If we ever have a pair of values whose path-related
; smoosh actually fails (rather than just having an unknown result),
; use it in the TODO slots here.
;
#;
(check-equal?
  (s= (pw /cons TODO 0+i) (pw /cons TODO 1+i))
  (known /nothing)
  "Path-related smoosh fails on cons cells with at least one pair of corresponding elements whose path-related smoosh fails, even if another pair's path-related smoosh result is unknown")

(check-equal?
  (s= (iw /cons 0 0) (iw /cons 0 0))
  (known /just /known /iw /cons 0 0)
  "Info smoosh works on cons cells whose elements are info smooshable")

(w- c (iw /cons 0 0)
  (check-eq?
    (known-value /just-value /known-value /s= c (iw /cons 0 0))
    c
    "Info smoosh preserves `eq?` when possible on cons cells whose elements are info smooshable"))

(check-equal?
  (s= (iw /cons 0 0) (iw /cons 0 0.0))
  (known /nothing)
  "Info smoosh fails on cons cells with a pair of corresponding elements whose info smoosh fails")

(check-equal?
  (sj (iw /cons 0 0) (iw /cons 0 0))
  (known /just /known /iw /cons 0 0)
  "Info smoosh join works on cons cells whose elements are info smoosh joinable")

(w- c (iw /cons 0 0)
  (check-eq?
    (known-value /just-value /known-value /sj c (iw /cons 0 0))
    c
    "Info smoosh join preserves `eq?` when possible on cons cells whose elements are info smoosh joinable"))

(check-equal?
  (sj (iw /cons 0 0) (iw /cons 0 0.0))
  (known /nothing)
  "Info smoosh join fails on cons cells with at least one pair of corresponding elements whose info smoosh join fails")

(check-equal?
  (sm (iw /cons 0 0) (iw /cons 0 0))
  (known /just /known /iw /cons 0 0)
  "Info smoosh meet works on `equal-always?` cons cells")

(w- c (iw /cons 0 0)
  (check-eq?
    (known-value /just-value /known-value /sm c (iw /cons 0 0))
    c
    "Info smoosh meet preserves `eq?` when possible on cons cells whose elements are info smoosh meetable"))

(check-equal?
  (sm (iw /cons 0 0) (iw /cons 0 0.0))
  (known /nothing)
  "Info smoosh meet fails on cons cells with at least one pair of corresponding elements whose info smoosh meet fails")

(check-equal?
  (s= (pw /iw /cons 0 0) (pw /iw /cons 0 0))
  (known /just /known /pw /iw /cons 0 0)
  "Path-related info smoosh works on cons cells whose elements are path-related info smooshable")

(w- c (pw /iw /cons 0 0)
  (check-eq?
    (known-value /just-value /known-value /s= c (pw /iw /cons 0 0))
    c
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


(check-equal?
  (s=
    (dynamic-type-var-for-any-dynamic-type)
    (dynamic-type-var-for-any-dynamic-type))
  (known /just /known /dynamic-type-var-for-any-dynamic-type)
  "Smoosh works on `dynamic-type-var-for-any-dynamic-type?` values")

(check-equal?
  (sj
    (dynamic-type-var-for-any-dynamic-type)
    (dynamic-type-var-for-any-dynamic-type))
  (known /just /known /dynamic-type-var-for-any-dynamic-type)
  "Smoosh join works on `dynamic-type-var-for-any-dynamic-type?` values")

(check-equal?
  (sm
    (dynamic-type-var-for-any-dynamic-type)
    (dynamic-type-var-for-any-dynamic-type))
  (known /just /known /dynamic-type-var-for-any-dynamic-type)
  "Smoosh meet works on `dynamic-type-var-for-any-dynamic-type?` values")

(check-equal?
  (s=
    (pw /dynamic-type-var-for-any-dynamic-type)
    (pw /dynamic-type-var-for-any-dynamic-type))
  (known /just /known /pw /dynamic-type-var-for-any-dynamic-type)
  "Path-related smoosh works on `dynamic-type-var-for-any-dynamic-type?` values")

(check-equal?
  (s=
    (iw /dynamic-type-var-for-any-dynamic-type)
    (iw /dynamic-type-var-for-any-dynamic-type))
  (known /just /known /iw /dynamic-type-var-for-any-dynamic-type)
  "Info smoosh works on `dynamic-type-var-for-any-dynamic-type?` values")

(check-equal?
  (sj
    (iw /dynamic-type-var-for-any-dynamic-type)
    (iw /dynamic-type-var-for-any-dynamic-type))
  (known /just /known /iw /dynamic-type-var-for-any-dynamic-type)
  "Info smoosh join works on `dynamic-type-var-for-any-dynamic-type?` values")

(check-equal?
  (sm
    (iw /dynamic-type-var-for-any-dynamic-type)
    (iw /dynamic-type-var-for-any-dynamic-type))
  (known /just /known /iw /dynamic-type-var-for-any-dynamic-type)
  "Info smoosh meet works on `dynamic-type-var-for-any-dynamic-type?` values")

(check-equal?
  (s=
    (pw /iw /dynamic-type-var-for-any-dynamic-type)
    (pw /iw /dynamic-type-var-for-any-dynamic-type))
  (known /just /known /pw /iw /dynamic-type-var-for-any-dynamic-type)
  "Path-related info smoosh works on `dynamic-type-var-for-any-dynamic-type?` values")


; TODO SMOOSH: Implement smooshing tests for values of the following
; types:
;
;   - (Done) Mutable strings, mutable byte strings, mutable boxes,
;     mutable vectors, prefab structs with mutable fields, and mutable
;     hash tables. (TODO SMOOSH: Write tests for smooshing the
;     chaperone wrappers of these.)
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
;   - Immutable boxes.
;
;   - Immutable vectors.
;
;   - Prefab structs with no mutable fields.
;
;   - Immutable hash tables.
;
;   - `maybe?` values.
;
;   - (Done) `trivial?` values.
;
;   - `known?` values and `example-unknown?` values.
;
;   - `gloss?` values.
;
;   - (Done) `dynamic-type-var-for-any-dynamic-type?` values.
;
;   - `equal-always-wrapper?` values.
;
;   - `indistinct-wrapper?` values.
