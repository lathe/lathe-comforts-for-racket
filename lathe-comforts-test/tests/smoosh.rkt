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
(require /only-in rackunit
  check-equal? check-pred define-check fail-check make-check-actual
  make-check-expected test-begin with-check-info*)
(require /only-in syntax/parse this-syntax)

(require lathe-comforts/private/shim)
(init-shim)

(require lathe-comforts)
(require lathe-comforts/knowable)
(require lathe-comforts/maybe)
(require lathe-comforts/private/smoosh)
(require /only-in lathe-comforts/private/smoosh
  [info-wrapper iw]
  [path-related-wrapper pw])
(require lathe-comforts/sequence)
(require lathe-comforts/trivial)
(require lathe-comforts/yknow)

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

(define (knowable= value=)
  (fn a b
    (mat (list a b) (list (known a) (known b)) (value= a b)
      #t)))

(define (equal-always-recur= value=)
  (fn a b
    (equal-always?/recur a b /fn a b /value= a b)))

(define (wrappers= value=)
  (fn a b
    (w-loop next a a b b
      (mat (list a b) (list (iw a) (iw b)) (next a b)
      /mat a (iw a) #f
      /mat b (iw b) #f
      /mat (list a b) (list (pw a) (pw b)) (next a b)
      /mat a (pw a) #f
      /mat b (pw b) #f
      /value= a b))))

(define-check (check-smoosh-underlying a e)
  (with-check-info*
    (list (make-check-actual a) (make-check-expected e))
  /fn
    (unless
      ((knowable= /equal-always-recur= /knowable= equal-always?) a e)
      (fail-check))))
(define-check (check-smoosh-eq-underlying a e)
  (with-check-info*
    (list (make-check-actual a) (make-check-expected e))
  /fn
    (unless
      ((knowable= /equal-always-recur= /knowable= /wrappers= eq?) a e)
      (fail-check))))

(define-syntax (check-smoosh stx)
  (syntax-parse stx
    [ (_ ({~literal s=} a:expr b:expr) e:expr message:string)
      #`
        (w- a-result a b-result b e-result e
          (test-begin
            #,
              (syntax/loc stx
                (check-smoosh-underlying
                  (s= a-result b-result)
                  e-result
                  message))
          /expect e-result (known e-result)
            #,
              (syntax/loc stx
                (check-pred
                  unknown?
                  (knowable-bind
                    (gloss-set-maybe-knowable (gloss) a-result
                      (just #f))
                    (fn g
                    /gloss-set-maybe-knowable g b-result (just #f)))
                  message))
          /expect e-result (just e-result)
            #,
              (syntax/loc stx
                (check-equal?
                  (gloss-count /gloss a-result #f b-result #f)
                  2
                  message))
            #,
              (syntax/loc stx
                (check-equal?
                  (gloss-count /gloss a-result #f b-result #f)
                  1
                  message))))]
    [ (_ (s:expr a:expr b:expr) e:expr message:string)
      #`
        (w- s-result s a-result a b-result b e-result e
          #,
            (syntax/loc stx
              (check-smoosh-underlying
                (s-result a-result b-result)
                e
                message)))]))

(define-syntax (check-smoosh-eq-left stx)
  (syntax-parse stx / (_ (s:expr a:expr b:expr) message:string)
    #`
      (w- s-result s a-result a b-result b
        #,
          (syntax/loc stx
            (check-smoosh-eq-underlying
              (s-result a-result b-result)
              (known /just /known a-result)
              message)))))

(define-syntax (check-smoosh-eq-right stx)
  (syntax-parse stx / (_ (s:expr a:expr b:expr) message:string)
    #`
      (w- s-result s a-result a b-result b
        #,
          (syntax/loc stx
            (check-smoosh-eq-underlying
              (s-result a-result b-result)
              (known /just /known b-result)
              message)))))

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


(check-smoosh
  (s= mstr1 mstr1)
  (known /just /known mstr1)
  "Smoosh works on `eq?` mutable strings")

(check-smoosh
  (s= mstr1 mstr2)
  (known /nothing)
  "Smoosh fails on non-`eq?` mutable strings")

(check-smoosh
  (sj mstr1 mstr1)
  (known /just /known mstr1)
  "Smoosh join works on `eq?` mutable strings")

(check-smoosh
  (sj mstr1 mstr2)
  (unknown)
  "Smoosh join is unknown on non-`eq?` mutable strings")

(check-smoosh
  (sm mstr1 mstr1)
  (known /just /known mstr1)
  "Smoosh meet works on `eq?` mutable strings")

(check-smoosh
  (sm mstr1 mstr2)
  (unknown)
  "Smoosh meet is unknown on non-`eq?` mutable strings")

(check-smoosh
  (s= (pw mstr1) (pw mstr1))
  (known /just /known /pw mstr1)
  "Path-related smoosh works on `eq?` mutable strings")

(check-smoosh
  (s= (pw mstr1) (pw mstr2))
  (unknown)
  "Path-related smoosh is unknown on non-`eq?` mutable strings")

(check-smoosh
  (s= (iw mstr1) (iw mstr1))
  (known /just /known /iw mstr1)
  "Info smoosh works on `eq?` mutable strings")

(check-smoosh
  (s= (iw mstr1) (iw mstr2))
  (known /nothing)
  "Info smoosh fails on non-`eq?` mutable strings")

(check-smoosh
  (sj (iw mstr1) (iw mstr1))
  (known /just /known /iw mstr1)
  "Info smoosh join works on `eq?` mutable strings")

(check-smoosh
  (sj (iw mstr1) (iw mstr2))
  (known /nothing)
  "Info smoosh join fails on non-`eq?` mutable strings")

(check-smoosh
  (sm (iw mstr1) (iw mstr1))
  (known /just /known /iw mstr1)
  "Info smoosh meet works on `eq?` mutable strings")

(check-smoosh
  (sm (iw mstr1) (iw mstr2))
  (known /nothing)
  "Info smoosh meet fails on non-`eq?` mutable strings")

(check-smoosh
  (s= (pw /iw mstr1) (pw /iw mstr1))
  (known /just /known /pw /iw mstr1)
  "Path-related info smoosh works on `eq?` mutable strings")

(check-smoosh
  (s= (pw /iw mstr1) (pw /iw mstr2))
  (known /nothing)
  "Path-related info smoosh fails on non-`eq?` mutable strings")


(check-smoosh
  (s= mbytes1 mbytes1)
  (known /just /known mbytes1)
  "Smoosh works on `eq?` mutable byte strings")

(check-smoosh
  (s= mbytes1 mbytes2)
  (known /nothing)
  "Smoosh fails on non-`eq?` mutable byte strings")

(check-smoosh
  (sj mbytes1 mbytes1)
  (known /just /known mbytes1)
  "Smoosh join works on `eq?` mutable byte strings")

(check-smoosh
  (sj mbytes1 mbytes2)
  (unknown)
  "Smoosh join is unknown on non-`eq?` mutable byte strings")

(check-smoosh
  (sm mbytes1 mbytes1)
  (known /just /known mbytes1)
  "Smoosh meet works on `eq?` mutable byte strings")

(check-smoosh
  (sm mbytes1 mbytes2)
  (unknown)
  "Smoosh meet is unknown on non-`eq?` mutable byte strings")

(check-smoosh
  (s= (pw mbytes1) (pw mbytes1))
  (known /just /known /pw mbytes1)
  "Path-related smoosh works on `eq?` mutable byte strings")

(check-smoosh
  (s= (pw mbytes1) (pw mbytes2))
  (unknown)
  "Path-related smoosh is unknown on non-`eq?` mutable byte strings")

(check-smoosh
  (s= (iw mbytes1) (iw mbytes1))
  (known /just /known /iw mbytes1)
  "Info smoosh works on `eq?` mutable byte strings")

(check-smoosh
  (s= (iw mbytes1) (iw mbytes2))
  (known /nothing)
  "Info smoosh fails on non-`eq?` mutable byte strings")

(check-smoosh
  (sj (iw mbytes1) (iw mbytes1))
  (known /just /known /iw mbytes1)
  "Info smoosh join works on `eq?` mutable byte strings")

(check-smoosh
  (sj (iw mbytes1) (iw mbytes2))
  (known /nothing)
  "Info smoosh join fails on non-`eq?` mutable byte strings")

(check-smoosh
  (sm (iw mbytes1) (iw mbytes1))
  (known /just /known /iw mbytes1)
  "Info smoosh meet works on `eq?` mutable byte strings")

(check-smoosh
  (sm (iw mbytes1) (iw mbytes2))
  (known /nothing)
  "Info smoosh meet fails on non-`eq?` mutable byte strings")

(check-smoosh
  (s= (pw /iw mbytes1) (pw /iw mbytes1))
  (known /just /known /pw /iw mbytes1)
  "Path-related info smoosh works on `eq?` mutable byte strings")

(check-smoosh
  (s= (pw /iw mbytes1) (pw /iw mbytes2))
  (known /nothing)
  "Path-related info smoosh fails on non-`eq?` mutable byte strings")


(check-smoosh-eq-left
  (s= mbox1 mbox1)
  "Smoosh works and preserves `eq?` on `equal-always?` mutable boxes")

(check-smoosh-eq-left
  (s= mbox1-chap mbox1)
  "Smoosh works and preserves `eq?` on `equal-always?` mutable boxes even when they're not `eq?`")

(check-smoosh
  (s= mbox1 mbox2)
  (known /nothing)
  "Smoosh fails on non-`equal-always?` mutable boxes")

(check-smoosh-eq-left
  (sj mbox1 mbox1)
  "Smoosh join works and preserves `eq?` on `equal-always?` mutable boxes")

(check-smoosh-eq-left
  (sj mbox1-chap mbox1)
  "Smoosh join works and preserves `eq?` on `equal-always?` mutable boxes even when they're not `eq?`")

(check-smoosh
  (sj mbox1 mbox2)
  (unknown)
  "Smoosh join is unknown on non-`equal-always?` mutable boxes")

(check-smoosh-eq-left
  (sm mbox1 mbox1)
  "Smoosh meet works and preserves `eq?` on `equal-always?` mutable boxes")

(check-smoosh-eq-left
  (sm mbox1-chap mbox1)
  "Smoosh meet works and preserves `eq?` on `equal-always?` mutable boxes even when they're not `eq?`")

(check-smoosh
  (sm mbox1 mbox2)
  (unknown)
  "Smoosh meet is unknown on non-`equal-always?` mutable boxes")

(check-smoosh-eq-left
  (s= (pw mbox1) (pw mbox1))
  "Path-related smoosh works and preserves `eq?` on `equal-always?` mutable boxes")

(check-smoosh-eq-left
  (s= (pw mbox1-chap) (pw mbox1))
  "Path-related smoosh works and preserves `eq?` on `equal-always?` mutable boxes even when they're not `eq?`")

(check-smoosh
  (s= (pw mbox1) (pw mbox2))
  (unknown)
  "Path-related smoosh is unknown on non-`equal-always?` mutable boxes")

(check-smoosh-eq-left
  (s= (iw mbox1) (iw mbox1))
  "Info smoosh works and preserves `eq?` on `eq?` mutable boxes")

(check-smoosh
  (s= (iw mbox1) (iw mbox2))
  (known /nothing)
  "Info smoosh fails on non-`equal-always?` mutable boxes")

(check-smoosh
  (s= (iw mbox1-chap-chap) (iw mbox1-chap))
  (unknown)
  "Info smoosh is unknown on non-`eq?` mutable boxes even when they're `chaperone-of?` in one direction")

(check-smoosh
  (s= (iw mbox1-chap) (iw mbox1-chap2))
  (unknown)
  "Info smoosh is unknown on non-`eq?` mutable boxes even when they're `equal-always?`")

(check-smoosh-eq-left
  (sj (iw mbox1) (iw mbox1))
  "Info smoosh join works and preserves `eq?` on `eq?` mutable boxes")

(check-smoosh-eq-left
  (sj (iw mbox1-chap-chap) (iw mbox1-chap))
  "Info smoosh join works and preserves `eq?` on `chaperone-of?` mutable boxes even when they're not `chaperone=?`")

(check-smoosh
  (sj (iw mbox1) (iw mbox2))
  (known /nothing)
  "Info smoosh join fails on non-`eq?` mutable boxes")

(check-smoosh
  (sj (iw mbox1-chap) (iw mbox1-chap2))
  (unknown)
  "Info smoosh join is unknown on non-`eq?` mutable boxes even when they're `equal-always?`")

(check-smoosh-eq-left
  (sm (iw mbox1) (iw mbox1))
  "Info smoosh meet works and preserves `eq?` on `eq?` mutable boxes")

(check-smoosh-eq-right
  (sm (iw mbox1-chap-chap) (iw mbox1-chap))
  "Info smoosh meet works and preserves `eq?` on `chaperone-of?` mutable boxes even when they're not `chaperone=?`")

(check-smoosh
  (sm (iw mbox1) (iw mbox2))
  (known /nothing)
  "Info smoosh meet fails on non-`eq?` mutable boxes")

(check-smoosh
  (sm (iw mbox1-chap) (iw mbox1-chap2))
  (unknown)
  "Info smoosh meet is unknown on non-`eq?` mutable boxes even when they're `equal-always?`")

(check-smoosh-eq-left
  (s= (pw /iw mbox1) (pw /iw mbox1))
  "Path-related info smoosh works and preserves `eq?` on `equal-always?` mutable boxes")

(check-smoosh-eq-left
  (s= (pw /iw mbox1-chap-chap) (pw /iw mbox1-chap))
  "Path-related info smoosh works and preserves `eq?` on `equal-always?` mutable boxes even when they're not `eq?`")

(check-smoosh-eq-left
  (s= (pw /iw mbox1-chap) (pw /iw mbox1-chap2))
  "Path-related info smoosh works and preserves `eq?` on `equal-always?` mutable boxes even when they're not `chaperone=?`")

(check-smoosh
  (s= (pw /iw mbox1) (pw /iw mbox2))
  (known /nothing)
  "Path-related info smoosh fails on non-`equal-always?` mutable boxes")


(check-smoosh-eq-left
  (s= mv1 mv1)
  "Smoosh works and preserves `eq?` on `equal-always?` mutable vectors")

(check-smoosh-eq-left
  (s= mv1-chap-with-prop mv1-with-prop)
  "Smoosh works and preserves `eq?` on `equal-always?` mutable vectors even when they're not `eq?`")

(check-smoosh
  (s= mv1 mv2)
  (known /nothing)
  "Smoosh fails on non-`equal-always?` mutable vectors")

(check-smoosh-eq-left
  (sj mv1 mv1)
  "Smoosh join works and preserves `eq?` on `equal-always?` mutable vectors")

(check-smoosh-eq-left
  (sj mv1-chap-with-prop mv1-with-prop)
  "Smoosh join works and preserves `eq?` on `equal-always?` mutable vectors even when they're not `eq?`")

(check-smoosh
  (sj mv1 mv2)
  (unknown)
  "Smoosh join is unknown on non-`equal-always?` mutable vectors")

(check-smoosh-eq-left
  (sm mv1 mv1)
  "Smoosh meet works and preserves `eq?` on `equal-always?` mutable vectors")

(check-smoosh-eq-left
  (sm mv1-chap-with-prop mv1-with-prop)
  "Smoosh meet works and preserves `eq?` on `equal-always?` mutable vectors even when they're not `eq?`")

(check-smoosh
  (sm mv1 mv2)
  (unknown)
  "Smoosh meet is unknown on non-`equal-always?` mutable vectors")

(check-smoosh-eq-left
  (s= (pw mv1) (pw mv1))
  "Path-related smoosh works and preserves `eq?` on `equal-always?` mutable vectors")

(check-smoosh-eq-left
  (s= (pw mv1-chap-with-prop) (pw mv1-with-prop))
  "Path-related smoosh works and preserves `eq?` on `equal-always?` mutable vectors even when they're not `eq?`")

(check-smoosh
  (s= (pw mv1) (pw mv2))
  (unknown)
  "Path-related smoosh is unknown on non-`equal-always?` mutable vectors")

(check-smoosh-eq-left
  (s= (iw mv1) (iw mv1))
  "Info smoosh works and preserves `eq?` on `chaperone=?` mutable vectors")

(check-smoosh
  (s= (iw mv1) (iw mv2))
  (known /nothing)
  "Info smoosh fails on non-`equal-always?` mutable vectors")

(check-smoosh
  (s= (iw mv1-chap-chap) (iw mv1-chap-with-prop))
  (unknown)
  "Info smoosh is unknown on non-`chaperone=?` mutable vectors even when they're `chaperone-of?` in one direction")

(check-smoosh
  (s= (iw mv1-chap) (iw mv1-chap2))
  (unknown)
  "Info smoosh is unknown on non-`chaperone=?` mutable vectors even when they're `equal-always?`")

(check-smoosh-eq-left
  (sj (iw mv1) (iw mv1))
  "Info smoosh join works and preserves `eq?` on `chaperone=?` mutable vectors")

(check-smoosh-eq-left
  (sj (iw mv1-chap-chap) (iw mv1-chap-with-prop))
  "Info smoosh join works and preserves `eq?` on `chaperone-of?` mutable vectors even when they're not `chaperone=?`")

(check-smoosh
  (sj (iw mv1) (iw mv2))
  (known /nothing)
  "Info smoosh join fails on non-`equal-always?` mutable vectors")

(check-smoosh
  (sj (iw mv1-chap) (iw mv1-chap2))
  (unknown)
  "Info smoosh join is unknown on non-`chaperone-of?` mutable vectors even when they're `equal-always?`")

(check-smoosh-eq-left
  (sm (iw mv1) (iw mv1))
  "Info smoosh meet works and preserves `eq?` on `chaperone=?` mutable vectors")

(check-smoosh-eq-right
  (sm (iw mv1-chap-chap) (iw mv1-chap-with-prop))
  "Info smoosh meet works and preserves `eq?` on `chaperone-of?` mutable vectors even when they're not `chaperone=?`")

(check-smoosh
  (sm (iw mv1) (iw mv2))
  (known /nothing)
  "Info smoosh meet fails on non-`equal-always?` mutable vectors")

(check-smoosh
  (sm (iw mv1-chap) (iw mv1-chap2))
  (unknown)
  "Info smoosh meet is unknown on non-`chaperone-of?` mutable vectors even when they're `equal-always?`")

(check-smoosh-eq-left
  (s= (pw /iw mv1) (pw /iw mv1))
  "Path-related info smoosh works and preserves `eq?` on `equal-always?` mutable vectors")

(check-smoosh-eq-left
  (s= (pw /iw mv1-chap-chap) (pw /iw mv1-chap-with-prop))
  "Path-related info smoosh works and preserves `eq?` on `equal-always?` mutable vectors even when they're not `eq?`")

(check-smoosh-eq-left
  (s= (pw /iw mv1-chap) (pw /iw mv1-chap2))
  "Path-related info smoosh works and preserves `eq?` on `equal-always?` mutable vectors even when they're not `chaperone=?`")

(check-smoosh
  (s= (pw /iw mv1) (pw /iw mv2))
  (known /nothing)
  "Path-related info smoosh fails on non-`equal-always?` mutable vectors")


(check-smoosh-eq-left
  (s= mprefab1 mprefab1)
  "Smoosh works and preserves `eq?` on `equal-always?` mutable prefab structs")

(check-smoosh-eq-left
  (s= mprefab1-chap-with-prop mprefab1-with-prop)
  "Smoosh works and preserves `eq?` on `equal-always?` mutable prefab structs even when they're not `eq?`")

(check-smoosh
  (s= mprefab1 mprefab2)
  (known /nothing)
  "Smoosh fails on non-`equal-always?` mutable prefab structs")

(check-smoosh-eq-left
  (sj mprefab1 mprefab1)
  "Smoosh join works and preserves `eq?` on `equal-always?` mutable prefab structs")

(check-smoosh-eq-left
  (sj mprefab1-chap-with-prop mprefab1-with-prop)
  "Smoosh join works on `equal-always?` mutable prefab structs even when they're not `eq?`")

(check-smoosh
  (sj mprefab1 mprefab2)
  (unknown)
  "Smoosh join is unknown on non-`equal-always?` mutable prefab structs")

(check-smoosh-eq-left
  (sm mprefab1 mprefab1)
  "Smoosh meet works and preserves `eq?` on `equal-always?` mutable prefab structs")

(check-smoosh-eq-left
  (sm mprefab1-chap-with-prop mprefab1-with-prop)
  "Smoosh meet works and preserves `eq?` on `equal-always?` mutable prefab structs even when they're not `eq?`")

(check-smoosh
  (sm mprefab1 mprefab2)
  (unknown)
  "Smoosh meet is unknown on non-`equal-always?` mutable prefab structs")

(check-smoosh-eq-left
  (s= (pw mprefab1) (pw mprefab1))
  "Path-related smoosh works and preserves `eq?` on `equal-always?` mutable prefab structs")

(check-smoosh-eq-left
  (s= (pw mprefab1-chap-with-prop) (pw mprefab1-with-prop))
  "Path-related smoosh works and preserves `eq?` on `equal-always?` mutable prefab structs even when they're not `eq?`")

(check-smoosh
  (s= (pw mprefab1) (pw mprefab2))
  (unknown)
  "Path-related smoosh is unknown on non-`equal-always?` mutable prefab structs")

(check-smoosh-eq-left
  (s= (iw mprefab1) (iw mprefab1))
  "Info smoosh works and preserves `eq?` on `chaperone=?` mutable prefab structs")

(check-smoosh
  (s= (iw mprefab1) (iw mprefab2))
  (known /nothing)
  "Info smoosh fails on non-`equal-always?` mutable prefab structs")

(check-smoosh
  (s= (iw mprefab1-chap-chap) (iw mprefab1-chap-with-prop))
  (unknown)
  "Info smoosh is unknown on non-`chaperone=?` mutable prefab structs even when they're `chaperone-of?` in one direction")

(check-smoosh
  (s= (iw mprefab1-chap) (iw mprefab1-chap2))
  (unknown)
  "Info smoosh is unknown on non-`chaperone=?` mutable prefab structs even when they're `equal-always?`")

(check-smoosh-eq-left
  (sj (iw mprefab1) (iw mprefab1))
  "Info smoosh join works and preserves `eq?` on `chaperone=?` mutable prefab structs")

(check-smoosh-eq-left
  (sj (iw mprefab1-chap-chap) (iw mprefab1-chap-with-prop))
  "Info smoosh join works and preserves `eq?` on `chaperone-of?` mutable prefab structs even when they're not `chaperone=?`")

(check-smoosh
  (sj (iw mprefab1) (iw mprefab2))
  (known /nothing)
  "Info smoosh join fails on non-`equal-always?` mutable prefab structs")

(check-smoosh
  (sj (iw mprefab1-chap) (iw mprefab1-chap2))
  (unknown)
  "Info smoosh join is unknown on non-`chaperone-of?` mutable prefab structs even when they're `equal-always?`")

(check-smoosh-eq-left
  (sm (iw mprefab1) (iw mprefab1))
  "Info smoosh meet works and preserves `eq?` on `chaperone=?` mutable prefab structs")

(check-smoosh-eq-right
  (sm (iw mprefab1-chap-chap) (iw mprefab1-chap-with-prop))
  "Info smoosh meet works and preserves `eq?` on `chaperone-of?` mutable prefab structs even when they're not `chaperone=?`")

(check-smoosh
  (sm (iw mprefab1) (iw mprefab2))
  (known /nothing)
  "Info smoosh meet fails on non-`equal-always?` mutable prefab structs")

(check-smoosh
  (sm (iw mprefab1-chap) (iw mprefab1-chap2))
  (unknown)
  "Info smoosh meet is unknown on non-`chaperone-of?` mutable prefab structs even when they're `equal-always?`")

(check-smoosh-eq-left
  (s= (pw /iw mprefab1) (pw /iw mprefab1))
  "Path-related info smoosh works and preserves `eq?` on `equal-always?` mutable prefab structs")

(check-smoosh-eq-left
  (s= (pw /iw mprefab1-chap-chap) (pw /iw mprefab1-chap-with-prop))
  "Path-related info smoosh works and preserves `eq?` on `equal-always?` mutable prefab structs even when they're not `eq?`")

(check-smoosh-eq-left
  (s= (pw /iw mprefab1-chap) (pw /iw mprefab1-chap2))
  "Path-related info smoosh works and preserves `eq?` on `equal-always?` mutable prefab structs even when they're not `chaperone=?`")

(check-smoosh
  (s= (pw /iw mprefab1) (pw /iw mprefab2))
  (known /nothing)
  "Path-related info smoosh fails on non-`equal-always?` mutable prefab structs")


(check-smoosh-eq-left
  (s= mhash1 mhash1)
  "Smoosh works and preserves `eq?` on `equal-always?` mutable hash tables")

(check-smoosh-eq-left
  (s= mhash1-chap mhash1)
  "Smoosh works and preserves `eq?` on `equal-always?` mutable hash tables even when they're not `eq?`")

(check-smoosh
  (s= mhash1 mhash2)
  (known /nothing)
  "Smoosh fails on non-`equal-always?` mutable hash tables")

(check-smoosh-eq-left
  (sj mhash1 mhash1)
  "Smoosh join works and preserves `eq?` on `equal-always?` mutable hash tables")

(check-smoosh-eq-left
  (sj mhash1-chap mhash1)
  "Smoosh join works and preserves `eq?` on `equal-always?` mutable hash tables even when they're not `eq?`")

(check-smoosh
  (sj mhash1 mhash2)
  (unknown)
  "Smoosh join is unknown on non-`equal-always?` mutable hash tables")

(check-smoosh-eq-left
  (sm mhash1 mhash1)
  "Smoosh meet works and preserves `eq?` on `equal-always?` mutable hash tables")

(check-smoosh-eq-left
  (sm mhash1-chap mhash1)
  "Smoosh meet works and preserves `eq?` on `equal-always?` mutable hash tables even when they're not `eq?`")

(check-smoosh
  (sm mhash1 mhash2)
  (unknown)
  "Smoosh meet is unknown on non-`equal-always?` mutable hash tables")

(check-smoosh-eq-left
  (s= (pw mhash1) (pw mhash1))
  "Path-related smoosh works and preserves `eq?` on `equal-always?` mutable hash tables")

(check-smoosh-eq-left
  (s= (pw mhash1-chap) (pw mhash1))
  "Path-related smoosh works on `equal-always?` mutable hash tables even when they're not `eq?`")

(check-smoosh
  (s= (pw mhash1) (pw mhash2))
  (unknown)
  "Path-related smoosh is unknown on non-`equal-always?` mutable hash tables")

(check-smoosh-eq-left
  (s= (iw mhash1) (iw mhash1))
  "Info smoosh works and preserves `eq?` on `eq?` mutable hash tables")

(check-smoosh
  (s= (iw mhash1) (iw mhash2))
  (known /nothing)
  "Info smoosh fails on non-`equal-always?` mutable hash tables")

(check-smoosh
  (s= (iw mhash1-chap-chap) (iw mhash1-chap))
  (unknown)
  "Info smoosh is unknown on non-`eq?` mutable hash tables even when they're `chaperone-of?` in one direction")

(check-smoosh
  (s= (iw mhash1-chap) (iw mhash1-chap2))
  (unknown)
  "Info smoosh is unknown on non-`eq?` mutable hash tables even when they're `equal-always?`")

(check-smoosh-eq-left
  (sj (iw mhash1) (iw mhash1))
  "Info smoosh join works and preserves `eq?` on `eq?` mutable hash tables")

(check-smoosh-eq-left
  (sj (iw mhash1-chap-chap) (iw mhash1-chap))
  "Info smoosh join works and preserves `eq?` on `chaperone-of?` mutable hash tables even when they're not `chaperone=?`")

(check-smoosh
  (sj (iw mhash1) (iw mhash2))
  (known /nothing)
  "Info smoosh join fails on non-`eq?` mutable hash tables")

(check-smoosh
  (sj (iw mhash1-chap) (iw mhash1-chap2))
  (unknown)
  "Info smoosh join is unknown on non-`eq?` mutable hash tables even when they're `equal-always?`")

(check-smoosh-eq-left
  (sm (iw mhash1) (iw mhash1))
  "Info smoosh meet works and preserves `eq?` on `eq?` mutable hash tables")

(check-smoosh-eq-right
  (sm (iw mhash1-chap-chap) (iw mhash1-chap))
  "Info smoosh meet works on `chaperone-of?` mutable hash tables even when they're not `chaperone=?`")

(check-smoosh
  (sm (iw mhash1) (iw mhash2))
  (known /nothing)
  "Info smoosh meet fails on non-`eq?` mutable hash tables")

(check-smoosh
  (sm (iw mhash1-chap) (iw mhash1-chap2))
  (unknown)
  "Info smoosh meet is unknown on non-`eq?` mutable hash tables even when they're `equal-always?`")

(check-smoosh-eq-left
  (s= (pw /iw mhash1) (pw /iw mhash1))
  "Path-related info smoosh works and preserves `eq?` on `equal-always?` mutable hash tables")

(check-smoosh-eq-left
  (s= (pw /iw mhash1-chap-chap) (pw /iw mhash1-chap))
  "Path-related info smoosh works and preserves `eq?` on `equal-always?` mutable hash tables even when they're not `eq?`")

(check-smoosh-eq-left
  (s= (pw /iw mhash1-chap) (pw /iw mhash1-chap2))
  "Path-related info smoosh works and preserves `eq?` on `equal-always?` mutable hash tables even when they're not `chaperone=?`")

(check-smoosh
  (s= (pw /iw mhash1) (pw /iw mhash2))
  (known /nothing)
  "Path-related info smoosh fails on non-`equal-always?` mutable hash tables")


(check-smoosh
  (s= flv1 flv1)
  (known /just /known flv1)
  "Smoosh works on `eq?` flvectors")

(check-smoosh
  (s= flv1 flv2)
  (known /nothing)
  "Smoosh fails on non-`eq?` flvectors")

(check-smoosh
  (sj flv1 flv1)
  (known /just /known flv1)
  "Smoosh join works on `eq?` flvectors")

(check-smoosh
  (sj flv1 flv2)
  (unknown)
  "Smoosh join is unknown on non-`eq?` flvectors")

(check-smoosh
  (sm flv1 flv1)
  (known /just /known flv1)
  "Smoosh meet works on `eq?` flvectors")

(check-smoosh
  (sm flv1 flv2)
  (unknown)
  "Smoosh meet is unknown on non-`eq?` flvectors")

(check-smoosh
  (s= (pw flv1) (pw flv1))
  (known /just /known /pw flv1)
  "Path-related smoosh works on `eq?` flvectors")

(check-smoosh
  (s= (pw flv1) (pw flv2))
  (unknown)
  "Path-related smoosh is unknown on non-`eq?` flvectors")

(check-smoosh
  (s= (iw flv1) (iw flv1))
  (known /just /known /iw flv1)
  "Info smoosh works on `eq?` flvectors")

(check-smoosh
  (s= (iw flv1) (iw flv2))
  (known /nothing)
  "Info smoosh fails on non-`eq?` flvectors")

(check-smoosh
  (sj (iw flv1) (iw flv1))
  (known /just /known /iw flv1)
  "Info smoosh join works on `eq?` flvectors")

(check-smoosh
  (sj (iw flv1) (iw flv2))
  (known /nothing)
  "Info smoosh join fails on non-`eq?` flvectors")

(check-smoosh
  (sm (iw flv1) (iw flv1))
  (known /just /known /iw flv1)
  "Info smoosh meet works on `eq?` flvectors")

(check-smoosh
  (sm (iw flv1) (iw flv2))
  (known /nothing)
  "Info smoosh meet fails on non-`eq?` flvectors")

(check-smoosh
  (s= (pw /iw flv1) (pw /iw flv1))
  (known /just /known /pw /iw flv1)
  "Path-related info smoosh works on `eq?` flvectors")

(check-smoosh
  (s= (pw /iw flv1) (pw /iw flv2))
  (known /nothing)
  "Path-related info smoosh fails on non-`eq?` flvectors")


(check-smoosh
  (s= fxv1 fxv1)
  (known /just /known fxv1)
  "Smoosh works on `eq?` fxvectors")

(check-smoosh
  (s= fxv1 fxv2)
  (known /nothing)
  "Smoosh fails on non-`eq?` fxvectors")

(check-smoosh
  (sj fxv1 fxv1)
  (known /just /known fxv1)
  "Smoosh join works on `eq?` fxvectors")

(check-smoosh
  (sj fxv1 fxv2)
  (unknown)
  "Smoosh join is unknown on non-`eq?` fxvectors")

(check-smoosh
  (sm fxv1 fxv1)
  (known /just /known fxv1)
  "Smoosh meet works on `eq?` fxvectors")

(check-smoosh
  (sm fxv1 fxv2)
  (unknown)
  "Smoosh meet is unknown on non-`eq?` fxvectors")

(check-smoosh
  (s= (pw fxv1) (pw fxv1))
  (known /just /known /pw fxv1)
  "Path-related smoosh works on `eq?` fxvectors")

(check-smoosh
  (s= (pw fxv1) (pw fxv2))
  (unknown)
  "Path-related smoosh is unknown on non-`eq?` fxvectors")

(check-smoosh
  (s= (iw fxv1) (iw fxv1))
  (known /just /known /iw fxv1)
  "Info smoosh works on `eq?` fxvectors")

(check-smoosh
  (s= (iw fxv1) (iw fxv2))
  (known /nothing)
  "Info smoosh fails on non-`eq?` fxvectors")

(check-smoosh
  (sj (iw fxv1) (iw fxv1))
  (known /just /known /iw fxv1)
  "Info smoosh join works on `eq?` fxvectors")

(check-smoosh
  (sj (iw fxv1) (iw fxv2))
  (known /nothing)
  "Info smoosh join fails on non-`eq?` fxvectors")

(check-smoosh
  (sm (iw fxv1) (iw fxv1))
  (known /just /known /iw fxv1)
  "Info smoosh meet works on `eq?` fxvectors")

(check-smoosh
  (sm (iw fxv1) (iw fxv2))
  (known /nothing)
  "Info smoosh meet fails on non-`eq?` fxvectors")

(check-smoosh
  (s= (pw /iw fxv1) (pw /iw fxv1))
  (known /just /known /pw /iw fxv1)
  "Path-related info smoosh works on `eq?` fxvectors")

(check-smoosh
  (s= (pw /iw fxv1) (pw /iw fxv2))
  (known /nothing)
  "Path-related info smoosh fails on non-`eq?` fxvectors")


(check-smoosh
  (s= 'a 'a)
  (known /just /known 'a)
  "Smoosh works on equal symbols")

(check-smoosh
  (s= 'a 'b)
  (known /nothing)
  "Smoosh fails on unequal symbols")

(check-smoosh
  (sj 'a 'a)
  (known /just /known 'a)
  "Smoosh join works on equal symbols")

(check-smoosh
  (sj 'a 'b)
  (unknown)
  "Smoosh join is unknown on unequal symbols")

(check-smoosh
  (sm 'a 'a)
  (known /just /known 'a)
  "Smoosh meet works on equal symbols")

(check-smoosh
  (sm 'a 'b)
  (unknown)
  "Smoosh meet is unknown on unequal symbols")

(check-smoosh
  (s= (pw 'a) (pw 'a))
  (known /just /known /pw 'a)
  "Path-related smoosh works on equal symbols")

(check-smoosh
  (s= (pw 'a) (pw 'b))
  (unknown)
  "Path-related smoosh is unknown on unequal symbols")

(check-smoosh
  (s= (iw 'a) (iw 'a))
  (known /just /known /iw 'a)
  "Info smoosh works on equal symbols")

(check-smoosh
  (s= (iw 'a) (iw 'b))
  (known /nothing)
  "Info smoosh fails on unequal symbols")

(check-smoosh
  (sj (iw 'a) (iw 'a))
  (known /just /known /iw 'a)
  "Info smoosh join works on equal symbols")

(check-smoosh
  (sj (iw 'a) (iw 'b))
  (known /nothing)
  "Info smoosh join fails on unequal symbols")

(check-smoosh
  (sm (iw 'a) (iw 'a))
  (known /just /known /iw 'a)
  "Info smoosh meet works on equal symbols")

(check-smoosh
  (sm (iw 'a) (iw 'b))
  (known /nothing)
  "Info smoosh meet fails on unequal symbols")

(check-smoosh
  (s= (pw /iw 'a) (pw /iw 'a))
  (known /just /known /pw /iw 'a)
  "Path-related info smoosh works on equal symbols")

(check-smoosh
  (s= (pw /iw 'a) (pw /iw 'b))
  (known /nothing)
  "Path-related info smoosh fails on unequal symbols")


(check-smoosh
  (s= '#:a '#:a)
  (known /just /known '#:a)
  "Smoosh works on equal keywords")

(check-smoosh
  (s= '#:a '#:b)
  (known /nothing)
  "Smoosh fails on unequal keywords")

(check-smoosh
  (sj '#:a '#:a)
  (known /just /known '#:a)
  "Smoosh join works on equal keywords")

(check-smoosh
  (sj '#:a '#:b)
  (unknown)
  "Smoosh join is unknown on unequal keywords")

(check-smoosh
  (sm '#:a '#:a)
  (known /just /known '#:a)
  "Smoosh meet works on equal keywords")

(check-smoosh
  (sm '#:a '#:b)
  (unknown)
  "Smoosh meet is unknown on unequal keywords")

(check-smoosh
  (s= (pw '#:a) (pw '#:a))
  (known /just /known /pw '#:a)
  "Path-related smoosh works on equal keywords")

(check-smoosh
  (s= (pw '#:a) (pw '#:b))
  (unknown)
  "Path-related smoosh is unknown on unequal keywords")

(check-smoosh
  (s= (iw '#:a) (iw '#:a))
  (known /just /known /iw '#:a)
  "Info smoosh works on equal keywords")

(check-smoosh
  (s= (iw '#:a) (iw '#:b))
  (known /nothing)
  "Info smoosh fails on unequal keywords")

(check-smoosh
  (sj (iw '#:a) (iw '#:a))
  (known /just /known /iw '#:a)
  "Info smoosh join works on equal keywords")

(check-smoosh
  (sj (iw '#:a) (iw '#:b))
  (known /nothing)
  "Info smoosh join fails on unequal keywords")

(check-smoosh
  (sm (iw '#:a) (iw '#:a))
  (known /just /known /iw '#:a)
  "Info smoosh meet works on equal keywords")

(check-smoosh
  (sm (iw '#:a) (iw '#:b))
  (known /nothing)
  "Info smoosh meet fails on unequal keywords")

(check-smoosh
  (s= (pw /iw '#:a) (pw /iw '#:a))
  (known /just /known /pw /iw '#:a)
  "Path-related info smoosh works on equal keywords")

(check-smoosh
  (s= (pw /iw '#:a) (pw /iw '#:b))
  (known /nothing)
  "Path-related info smoosh fails on unequal keywords")


(check-smoosh
  (s= (list) (list))
  (known /just /known /list)
  "Smoosh works on empty lists")

(check-smoosh
  (sj (list) (list))
  (known /just /known /list)
  "Smoosh join works on empty lists")

(check-smoosh
  (sm (list) (list))
  (known /just /known /list)
  "Smoosh meet works on empty lists")

(check-smoosh
  (s= (pw /list) (pw /list))
  (known /just /known /pw /list)
  "Path-related smoosh works on empty lists")

(check-smoosh
  (s= (iw /list) (iw /list))
  (known /just /known /iw /list)
  "Info smoosh works on empty lists")

(check-smoosh
  (sj (iw /list) (iw /list))
  (known /just /known /iw /list)
  "Info smoosh join works on empty lists")

(check-smoosh
  (sm (iw /list) (iw /list))
  (known /just /known /iw /list)
  "Info smoosh meet works on empty lists")

(check-smoosh
  (s= (pw /iw /list) (pw /iw /list))
  (known /just /known /pw /iw /list)
  "Path-related info smoosh works on empty lists")


(check-smoosh-eq-left
  (s= (cons 0 0.0) (cons 0.0 0))
  "Smoosh works and preserves `eq?` when possible on equal cons cells")

(check-smoosh
  (s= (cons 0 0) (cons 1 0))
  (known /nothing)
  "Smoosh fails on unequal cons cells")

(check-smoosh-eq-left
  (sj (cons 0 0.0) (cons 0.0 0))
  "Smoosh join works and preserves `eq?` when possible on equal cons cells")

(check-smoosh
  (sj (cons 1 0) (cons 0.0 1+0.0i))
  (known /just /known /cons 1 1+0.0i)
  "Smoosh join works on unequal, comparable cons cells")

(check-smoosh
  (sj (cons 0 0+i) (cons 0 1+i))
  (unknown)
  "Smoosh join is unknown on unequal, uncomparable cons cells")

(check-smoosh-eq-left
  (sm (cons 0 0.0) (cons 0.0 0))
  "Smoosh meet works and preserves `eq?` when possible on equal cons cells")

(check-smoosh
  (sm (cons 1 0) (cons 0.0 1+0.0i))
  (known /just /known /cons 0.0 0)
  "Smoosh meet works on unequal, comparable cons cells")

(check-smoosh
  (sm (cons 0 0+i) (cons 0 1+i))
  (unknown)
  "Smoosh meet is unknown on unequal, uncomparable cons cells")

(check-smoosh-eq-left
  (s= (pw /cons 0 0.0) (pw /cons 0.0 0))
  "Path-related smoosh works and preserves `eq?` when possible on equal cons cells")

(check-smoosh-eq-left
  (s= (pw /cons 0 0.0) (pw /cons 1.0 1+0.0i))
  "Path-related smoosh works and preserves `eq?` when possible on cons cells with path-related elements")

(check-smoosh
  (s= (pw /cons 0 0+i) (pw /cons 0 1+i))
  (unknown)
  "Path-related smoosh is unknown on cons cells with at least one pair of corresponding elements whose path-relatedness is unknown and no pairs whose path-relatedness is known false")

(check-smoosh
  (s= (pw /cons path-failing-1 0+i) (pw /cons path-failing-2 1+i))
  (known /nothing)
  "Path-related smoosh fails on cons cells with at least one pair of corresponding elements whose path-related smoosh fails, even if another pair's path-related smoosh result is unknown")

(check-smoosh-eq-left
  (s= (iw /cons 0 0) (iw /cons 0 0))
  "Info smoosh works and preserves `eq?` when possible on cons cells whose elements are info smooshable")

(check-smoosh
  (s= (iw /cons 0 0) (iw /cons 0 0.0))
  (known /nothing)
  "Info smoosh fails on cons cells with a pair of corresponding elements whose info smoosh fails")

(check-smoosh-eq-left
  (sj (iw /cons 0 0) (iw /cons 0 0))
  "Info smoosh join works and preserves `eq?` when possible on cons cells whose elements are info smoosh joinable")

(check-smoosh
  (sj (iw /cons 0 0) (iw /cons 0 0.0))
  (known /nothing)
  "Info smoosh join fails on cons cells with at least one pair of corresponding elements whose info smoosh join fails")

(check-smoosh-eq-left
  (sm (iw /cons 0 0) (iw /cons 0 0))
  "Info smoosh meet works and preserves `eq?` when possible on cons cells whose elements are info smoosh meetable")

(check-smoosh
  (sm (iw /cons 0 0) (iw /cons 0 0.0))
  (known /nothing)
  "Info smoosh meet fails on cons cells with at least one pair of corresponding elements whose info smoosh meet fails")

(check-smoosh-eq-left
  (s= (pw /iw /cons 0 0) (pw /iw /cons 0 0))
  "Path-related info smoosh works and preserves `eq?` when possible on cons cells whose elements are path-related info smooshable")

(check-smoosh
  (s= (pw /iw /cons 0 0) (pw /iw /cons 0 0.0))
  (known /nothing)
  "Path-related info smoosh fails on cons cells with at least one pair of corresponding elements whose path-related info smoosh fails")


(check-smoosh
  (s= 'a '#:b)
  (known /nothing)
  "Smoosh fails on interactions between different types of base syntactic atom")

(check-smoosh
  (sj 'a '#:b)
  (unknown)
  "Smoosh join is unknown on interactions between different types of base syntactic atom")

(check-smoosh
  (sm 'a '#:b)
  (unknown)
  "Smoosh meet is unknown on interactions between different types of base syntactic atom")

(check-smoosh
  (s= (pw 'a) (pw '#:b))
  (unknown)
  "Path-related smoosh is unknown on interactions between different types of base syntactic atom")

(check-smoosh
  (s= (iw 'a) (iw '#:b))
  (known /nothing)
  "Info smoosh fails on interactions between different types of base syntactic atom")

(check-smoosh
  (sj (iw 'a) (iw '#:b))
  (known /nothing)
  "Info smoosh join fails on interactions between different types of base syntactic atom")

(check-smoosh
  (sm (iw 'a) (iw '#:b))
  (known /nothing)
  "Info smoosh meet fails on interactions between different types of base syntactic atom")

(check-smoosh
  (s= (pw /iw 'a) (pw /iw '#:b))
  (known /nothing)
  "Path-related info smoosh fails on interactions between different types of base syntactic atom")


(check-smoosh
  (s= #f #f)
  (known /just /known #f)
  "Smoosh works on equal booleans")

(check-smoosh
  (s= #f #t)
  (known /nothing)
  "Smoosh fails on unequal booleans")

(check-smoosh
  (sj #f #f)
  (known /just /known #f)
  "Smoosh join works on equal booleans")

(check-smoosh
  (sj #f #t)
  (unknown)
  "Smoosh join is unknown on unequal booleans")

(check-smoosh
  (sm #f #f)
  (known /just /known #f)
  "Smoosh meet works on equal booleans")

(check-smoosh
  (sm #f #t)
  (unknown)
  "Smoosh meet is unknown on unequal booleans")

(check-smoosh
  (s= (pw #f) (pw #f))
  (known /just /known /pw #f)
  "Path-related smoosh works on equal booleans")

(check-smoosh
  (s= (pw #f) (pw #t))
  (unknown)
  "Path-related smoosh is unknown on unequal booleans")

(check-smoosh
  (s= (iw #f) (iw #f))
  (known /just /known /iw #f)
  "Info smoosh works on equal booleans")

(check-smoosh
  (s= (iw #f) (iw #t))
  (known /nothing)
  "Info smoosh fails on unequal booleans")

(check-smoosh
  (sj (iw #f) (iw #f))
  (known /just /known /iw #f)
  "Info smoosh join works on equal booleans")

(check-smoosh
  (sj (iw #f) (iw #t))
  (known /nothing)
  "Info smoosh join fails on unequal booleans")

(check-smoosh
  (sm (iw #f) (iw #f))
  (known /just /known /iw #f)
  "Info smoosh meet works on equal booleans")

(check-smoosh
  (sm (iw #f) (iw #t))
  (known /nothing)
  "Info smoosh meet fails on unequal booleans")

(check-smoosh
  (s= (pw /iw #f) (pw /iw #f))
  (known /just /known /pw /iw #f)
  "Path-related info smoosh works on equal booleans")

(check-smoosh
  (s= (pw /iw #f) (pw /iw #t))
  (known /nothing)
  "Path-related info smoosh fails on unequal booleans")


(check-smoosh
  (s= +nan.0 +nan.0)
  (unknown)
  "Smoosh is unknown on real NaNs")

(check-smoosh
  (s= 0+nan.0i 0+nan.0i)
  (unknown)
  "Smoosh is unknown on imaginary NaNs")

(check-smoosh
  (s= +nan.0 0+nan.0i)
  (unknown)
  "Smoosh is unknown on distinct NaNs")

(check-smoosh
  (sj +nan.0 +nan.0)
  (unknown)
  "Smoosh join is unknown on real NaNs")

(check-smoosh
  (sj 0+nan.0i 0+nan.0i)
  (unknown)
  "Smoosh join is unknown on imaginary NaNs")

(check-smoosh
  (sj +nan.0 0+nan.0i)
  (unknown)
  "Smoosh join is unknown on distinct NaNs")

(check-smoosh
  (sm +nan.0 +nan.0)
  (unknown)
  "Smoosh meet is unknown on real NaNs")

(check-smoosh
  (sm 0+nan.0i 0+nan.0i)
  (unknown)
  "Smoosh meet is unknown on imaginary NaNs")

(check-smoosh
  (sm +nan.0 0+nan.0i)
  (unknown)
  "Smoosh meet is unknown on distinct NaNs")

(check-smoosh
  (s= (pw +nan.0) (pw +nan.0))
  (unknown)
  "Path-related smoosh is unknown on real NaNs")

(check-smoosh
  (s= (pw 0+nan.0i) (pw 0+nan.0i))
  (unknown)
  "Path-related smoosh is unknown on imaginary NaNs")

(check-smoosh
  (s= (pw +nan.0) (pw 0+nan.0i))
  (unknown)
  "Path-related smoosh is unknown on distinct NaNs")

(check-smoosh
  (s= (iw +nan.0) (iw +nan.0))
  (unknown)
  "Info smoosh is unknown on real NaNs")

(check-smoosh
  (s= (iw 0+nan.0i) (iw 0+nan.0i))
  (unknown)
  "Info smoosh is unknown on imaginary NaNs")

(check-smoosh
  (s= (iw +nan.0) (iw 0+nan.0i))
  (unknown)
  "Info smoosh is unknown on distinct NaNs")

(check-smoosh
  (sj (iw +nan.0) (iw +nan.0))
  (unknown)
  "Info smoosh join is unknown on real NaNs")

(check-smoosh
  (sj (iw 0+nan.0i) (iw 0+nan.0i))
  (unknown)
  "Info smoosh join is unknown on imaginary NaNs")

(check-smoosh
  (sj (iw +nan.0) (iw 0+nan.0i))
  (unknown)
  "Info smoosh join is unknown on distinct NaNs")

(check-smoosh
  (sm (iw +nan.0) (iw +nan.0))
  (unknown)
  "Info smoosh meet is unknown on real NaNs")

(check-smoosh
  (sm (iw 0+nan.0i) (iw 0+nan.0i))
  (unknown)
  "Info smoosh meet is unknown on imaginary NaNs")

(check-smoosh
  (sm (iw +nan.0) (iw 0+nan.0i))
  (unknown)
  "Info smoosh meet is unknown on distinct NaNs")

(check-smoosh
  (s= (pw /iw +nan.0) (pw /iw +nan.0))
  (unknown)
  "Path-related info smoosh is unknown on real NaNs")

(check-smoosh
  (s= (pw /iw 0+nan.0i) (pw /iw 0+nan.0i))
  (unknown)
  "Path-related info smoosh is unknown on imaginary NaNs")

(check-smoosh
  (s= (pw /iw +nan.0) (pw /iw 0+nan.0i))
  (unknown)
  "Path-related info smoosh is unknown on distinct NaNs")


(check-smoosh
  (s= 0 0.0)
  (known /just /known 0)
  "Smoosh works on equal numbers")

(check-smoosh
  (s= 0 1)
  (known /nothing)
  "Smoosh fails on unequal numbers")

(check-smoosh
  (sj 0 0.0)
  (known /just /known 0)
  "Smoosh join works on equal numbers")

(check-smoosh
  (sj 0 1+0.0i)
  (known /just /known 1+0.0i)
  "Smoosh join works on unequal, comparable numbers")

(check-smoosh
  (sj 0+i 1+i)
  (unknown)
  "Smoosh join is unknown on unequal, uncomparable numbers")

(check-smoosh
  (sm 0 0.0)
  (known /just /known 0)
  "Smoosh meet works on equal numbers")

(check-smoosh
  (sm 0 1+0.0i)
  (known /just /known 0)
  "Smoosh meet works on unequal, comparable numbers")

(check-smoosh
  (sm 0+i 1+i)
  (unknown)
  "Smoosh meet is unknown on unequal, uncomparable numbers")

(check-smoosh
  (s= (pw 0) (pw 0.0))
  (known /just /known /pw 0)
  "Path-related smoosh works on equal, zero-imaginary-part numbers")

(check-smoosh
  (s= (pw 0) (pw 1+0.0i))
  (known /just /known /pw 0)
  "Path-related smoosh works on unequal, zero-imaginary-part numbers")

(check-smoosh
  (s= (pw 0+i) (pw 0.0+1.0i))
  (known /just /known /pw 0+i)
  "Path-related smoosh works on equal, nonzero-imaginary-part numbers")

(check-smoosh
  (s= (pw 0+i) (pw 1+i))
  (unknown)
  "Path-related smoosh is unknown on unequal, nonzero-imaginary-part numbers")

(check-smoosh
  (s= (iw 0) (iw 0))
  (known /just /known /iw 0)
  "Info smoosh works on `equal-always?` numbers")

(check-smoosh
  (s= (iw 0) (iw 0.0))
  (known /nothing)
  "Info smoosh fails on non-`equal-always?` numbers")

(check-smoosh
  (sj (iw 0) (iw 0))
  (known /just /known /iw 0)
  "Info smoosh join works on `equal-always?` numbers")

(check-smoosh
  (sj (iw 0) (iw 0.0))
  (known /nothing)
  "Info smoosh join fails on non-`equal-always?` numbers")

(check-smoosh
  (sm (iw 0) (iw 0))
  (known /just /known /iw 0)
  "Info smoosh meet works on `equal-always?` numbers")

(check-smoosh
  (sm (iw 0) (iw 0.0))
  (known /nothing)
  "Info smoosh meet fails on non-`equal-always?` numbers")

(check-smoosh
  (s= (pw /iw 0) (pw /iw 0))
  (known /just /known /pw /iw 0)
  "Path-related info smoosh works on `equal-always?` numbers")

(check-smoosh
  (s= (pw /iw 0) (pw /iw 0.0))
  (known /nothing)
  "Path-related info smoosh fails on non-`equal-always?` numbers")


(when (extflonum-available?)
  
  (check-smoosh
    (s= +nan.t +nan.t)
    (unknown)
    "Smoosh is unknown on extflonum NaNs")
  
  (check-smoosh
    (sj +nan.t +nan.t)
    (unknown)
    "Smoosh join is unknown on extflonum NaNs")
  
  (check-smoosh
    (sm +nan.t +nan.t)
    (unknown)
    "Smoosh meet is unknown on extflonum NaNs")
  
  (check-smoosh
    (s= (pw +nan.t) (pw +nan.t))
    (unknown)
    "Path-related smoosh is unknown on extflonum NaNs")
  
  (check-smoosh
    (s= (iw +nan.t) (iw +nan.t))
    (unknown)
    "Info smoosh is unknown on extflonum NaNs")
  
  (check-smoosh
    (sj (iw +nan.t) (iw +nan.t))
    (unknown)
    "Info smoosh join is unknown on extflonum NaNs")
  
  (check-smoosh
    (sm (iw +nan.t) (iw +nan.t))
    (unknown)
    "Info smoosh meet is unknown on extflonum NaNs")
  
  (check-smoosh
    (s= (pw /iw +nan.t) (pw /iw +nan.t))
    (unknown)
    "Path-related info smoosh is unknown on extflonum NaNs")
  
  
  (check-smoosh
    (s= -0t0 0t0)
    (known /just /known -0t0)
    "Smoosh works on equal extflonums")
  
  (check-smoosh
    (s= 0t0 1t0)
    (known /nothing)
    "Smoosh fails on unequal extflonums")
  
  (check-smoosh
    (sj -0t0 0t0)
    (known /just /known -0t0)
    "Smoosh join works on equal extflonums")
  
  (check-smoosh
    (sj 0t0 1t0)
    (known /just /known 1t0)
    "Smoosh join works on unequal extflonums")
  
  (check-smoosh
    (sm -0t0 0t0)
    (known /just /known -0t0)
    "Smoosh meet works on equal extflonums")
  
  (check-smoosh
    (sm 0t0 1t0)
    (known /just /known 0t0)
    "Smoosh meet works on unequal extflonums")
  
  (check-smoosh
    (s= (pw -0t0) (pw 0t0))
    (known /just /known /pw -0t0)
    "Path-related smoosh works on equal extflonums")
  
  (check-smoosh
    (s= (pw 0t0) (pw 1t0))
    (known /just /known /pw 0t0)
    "Path-related smoosh works on unequal extflonums")
  
  (check-smoosh
    (s= (iw 0t0) (iw 0t0))
    (known /just /known /iw 0t0)
    "Info smoosh works on `equal-always?` extflonums")
  
  (check-smoosh
    (s= (iw -0t0) (iw 0t0))
    (known /nothing)
    "Info smoosh fails on non-`equal-always?` extflonums")
  
  (check-smoosh
    (sj (iw 0t0) (iw 0t0))
    (known /just /known /iw 0t0)
    "Info smoosh join works on `equal-always?` extflonums")
  
  (check-smoosh
    (sj (iw -0t0) (iw 0t0))
    (known /nothing)
    "Info smoosh join fails on non-`equal-always?` extflonums")
  
  (check-smoosh
    (sm (iw 0t0) (iw 0t0))
    (known /just /known /iw 0t0)
    "Info smoosh meet works on `equal-always?` extflonums")
  
  (check-smoosh
    (sm (iw -0t0) (iw 0t0))
    (known /nothing)
    "Info smoosh meet fails on non-`equal-always?` extflonums")
  
  (check-smoosh
    (s= (pw /iw 0t0) (pw /iw 0t0))
    (known /just /known /pw /iw 0t0)
    "Path-related info smoosh works on `equal-always?` extflonums")
  
  (check-smoosh
    (s= (pw /iw -0t0) (pw /iw 0t0))
    (known /nothing)
    "Path-related info smoosh fails on non-`equal-always?` extflonums")
  
  )


(check-smoosh
  (s= #\a #\a)
  (known /just /known #\a)
  "Smoosh works on equal characters")

(check-smoosh
  (s= #\a #\b)
  (unknown)
  "Smoosh is unknown on unequal characters")

(check-smoosh
  (sj #\a #\a)
  (known /just /known #\a)
  "Smoosh join works on equal characters")

(check-smoosh
  (sj #\a #\b)
  (unknown)
  "Smoosh join is unknown on unequal characters")

(check-smoosh
  (sm #\a #\a)
  (known /just /known #\a)
  "Smoosh meet works on equal characters")

(check-smoosh
  (sm #\a #\b)
  (unknown)
  "Smoosh meet is unknown on unequal characters")

(check-smoosh
  (s= (pw #\a) (pw #\a))
  (known /just /known /pw #\a)
  "Path-related smoosh works on equal characters")

(check-smoosh
  (s= (pw #\a) (pw #\b))
  (unknown)
  "Path-related smoosh is unknown on unequal characters")

(check-smoosh
  (s= (iw #\a) (iw #\a))
  (known /just /known /iw #\a)
  "Info smoosh works on equal characters")

(check-smoosh
  (s= (iw #\a) (iw #\b))
  (unknown)
  "Info smoosh is unknown on unequal characters")

(check-smoosh
  (sj (iw #\a) (iw #\a))
  (known /just /known /iw #\a)
  "Info smoosh join works on equal characters")

(check-smoosh
  (sj (iw #\a) (iw #\b))
  (unknown)
  "Info smoosh join is unknown on unequal characters")

(check-smoosh
  (sm (iw #\a) (iw #\a))
  (known /just /known /iw #\a)
  "Info smoosh meet works on equal characters")

(check-smoosh
  (sm (iw #\a) (iw #\b))
  (unknown)
  "Info smoosh meet is unknown on unequal characters")

(check-smoosh
  (s= (pw /iw #\a) (pw /iw #\a))
  (known /just /known /pw /iw #\a)
  "Path-related info smoosh works on equal characters")

(check-smoosh
  (s= (pw /iw #\a) (pw /iw #\b))
  (unknown)
  "Path-related info smoosh is unknown on unequal characters")


(check-smoosh
  (s= str1 str1)
  (known /just /known str1)
  "Smoosh works on equal immutable strings")

(check-smoosh
  (s= str1 str2)
  (unknown)
  "Smoosh is unknown on unequal immutable strings")

(check-smoosh
  (sj str1 str1)
  (known /just /known str1)
  "Smoosh join works on equal immutable strings")

(check-smoosh
  (sj str1 str2)
  (unknown)
  "Smoosh join is unknown on unequal immutable strings")

(check-smoosh
  (sm str1 str1)
  (known /just /known str1)
  "Smoosh meet works on equal immutable strings")

(check-smoosh
  (sm str1 str2)
  (unknown)
  "Smoosh meet is unknown on unequal immutable strings")

(check-smoosh
  (s= (pw str1) (pw str1))
  (known /just /known /pw str1)
  "Path-related smoosh works on equal immutable strings")

(check-smoosh
  (s= (pw str1) (pw str2))
  (unknown)
  "Path-related smoosh is unknown on unequal immutable strings")

(check-smoosh
  (s= (iw str1) (iw str1))
  (known /just /known /iw str1)
  "Info smoosh works on equal immutable strings")

(check-smoosh
  (s= (iw str1) (iw str2))
  (unknown)
  "Info smoosh is unknown on unequal immutable strings")

(check-smoosh
  (sj (iw str1) (iw str1))
  (known /just /known /iw str1)
  "Info smoosh join works on equal immutable strings")

(check-smoosh
  (sj (iw str1) (iw str2))
  (unknown)
  "Info smoosh join is unknown on unequal immutable strings")

(check-smoosh
  (sm (iw str1) (iw str1))
  (known /just /known /iw str1)
  "Info smoosh meet works on equal immutable strings")

(check-smoosh
  (sm (iw str1) (iw str2))
  (unknown)
  "Info smoosh meet is unknown on unequal immutable strings")

(check-smoosh
  (s= (pw /iw str1) (pw /iw str1))
  (known /just /known /pw /iw str1)
  "Path-related info smoosh works on equal immutable strings")

(check-smoosh
  (s= (pw /iw str1) (pw /iw str2))
  (unknown)
  "Path-related info smoosh is unknown on unequal immutable strings")


(check-smoosh
  (s= bts1 bts1)
  (known /just /known bts1)
  "Smoosh works on equal immutable byte strings")

(check-smoosh
  (s= bts1 bts2)
  (unknown)
  "Smoosh is unknown on unequal immutable byte strings")

(check-smoosh
  (sj bts1 bts1)
  (known /just /known bts1)
  "Smoosh join works on equal immutable byte strings")

(check-smoosh
  (sj bts1 bts2)
  (unknown)
  "Smoosh join is unknown on unequal immutable byte strings")

(check-smoosh
  (sm bts1 bts1)
  (known /just /known bts1)
  "Smoosh meet works on equal immutable byte strings")

(check-smoosh
  (sm bts1 bts2)
  (unknown)
  "Smoosh meet is unknown on unequal immutable byte strings")

(check-smoosh
  (s= (pw bts1) (pw bts1))
  (known /just /known /pw bts1)
  "Path-related smoosh works on equal immutable byte strings")

(check-smoosh
  (s= (pw bts1) (pw bts2))
  (unknown)
  "Path-related smoosh is unknown on unequal immutable byte strings")

(check-smoosh
  (s= (iw bts1) (iw bts1))
  (known /just /known /iw bts1)
  "Info smoosh works on equal immutable byte strings")

(check-smoosh
  (s= (iw bts1) (iw bts2))
  (unknown)
  "Info smoosh is unknown on unequal immutable byte strings")

(check-smoosh
  (sj (iw bts1) (iw bts1))
  (known /just /known /iw bts1)
  "Info smoosh join works on equal immutable byte strings")

(check-smoosh
  (sj (iw bts1) (iw bts2))
  (unknown)
  "Info smoosh join is unknown on unequal immutable byte strings")

(check-smoosh
  (sm (iw bts1) (iw bts1))
  (known /just /known /iw bts1)
  "Info smoosh meet works on equal immutable byte strings")

(check-smoosh
  (sm (iw bts1) (iw bts2))
  (unknown)
  "Info smoosh meet is unknown on unequal immutable byte strings")

(check-smoosh
  (s= (pw /iw bts1) (pw /iw bts1))
  (known /just /known /pw /iw bts1)
  "Path-related info smoosh works on equal immutable byte strings")

(check-smoosh
  (s= (pw /iw bts1) (pw /iw bts2))
  (unknown)
  "Path-related info smoosh is unknown on unequal immutable byte strings")


(check-smoosh
  (s= regexp1 regexp1)
  (unknown)
  "Smoosh is unknown on regular expressions")

(check-smoosh
  (sj regexp1 regexp1)
  (unknown)
  "Smoosh join is unknown on regular expressions")

(check-smoosh
  (sm regexp1 regexp1)
  (unknown)
  "Smoosh meet is unknown on regular expressions")

(check-smoosh
  (s= (pw regexp1) (pw regexp1))
  (unknown)
  "Path-related smoosh is unknown on regular expressions")

(check-smoosh
  (s= (iw regexp1) (iw regexp1))
  (unknown)
  "Info smoosh is unknown on regular expressions")

(check-smoosh
  (sj (iw regexp1) (iw regexp1))
  (unknown)
  "Info smoosh join is unknown on regular expressions")

(check-smoosh
  (sm (iw regexp1) (iw regexp1))
  (unknown)
  "Info smoosh meet is unknown on regular expressions")

(check-smoosh
  (s= (pw /iw regexp1) (pw /iw regexp1))
  (unknown)
  "Path-related info smoosh is unknown on regular expressions")


(check-smoosh
  (s= expr1 expr1)
  (unknown)
  "Smoosh is unknown on compiled code expressions")

(check-smoosh
  (sj expr1 expr1)
  (unknown)
  "Smoosh join is unknown on compiled code expressions")

(check-smoosh
  (sm expr1 expr1)
  (unknown)
  "Smoosh meet is unknown on compiled code expressions")

(check-smoosh
  (s= (pw expr1) (pw expr1))
  (unknown)
  "Path-related smoosh is unknown on compiled code expressions")

(check-smoosh
  (s= (iw expr1) (iw expr1))
  (unknown)
  "Info smoosh is unknown on compiled code expressions")

(check-smoosh
  (sj (iw expr1) (iw expr1))
  (unknown)
  "Info smoosh join is unknown on compiled code expressions")

(check-smoosh
  (sm (iw expr1) (iw expr1))
  (unknown)
  "Info smoosh meet is unknown on compiled code expressions")

(check-smoosh
  (s= (pw /iw expr1) (pw /iw expr1))
  (unknown)
  "Path-related info smoosh is unknown on compiled code expressions")


(check-smoosh-eq-left
  (s= (box-immutable 0) (box-immutable 0.0))
  "Smoosh works and preserves `eq?` when possible on equal immutable boxes")

(check-smoosh
  (s= (box-immutable 0) (box-immutable 1))
  (known /nothing)
  "Smoosh fails on unequal immutable boxes")

(check-smoosh-eq-left
  (sj (box-immutable 0) (box-immutable 0.0))
  "Smoosh join works and preserves `eq?` when possible on equal immutable boxes")

(check-smoosh
  (sj (box-immutable 1) (box-immutable 0.0))
  (known /just /known /box-immutable 1)
  "Smoosh join works on unequal, comparable immutable boxes")

(check-smoosh
  (sj (box-immutable 0+i) (box-immutable 1+i))
  (unknown)
  "Smoosh join is unknown on unequal, uncomparable immutable boxes")

(check-smoosh-eq-left
  (sm (box-immutable 0) (box-immutable 0.0))
  "Smoosh meet works and preserves `eq?` when possible on equal immutable boxes")

(check-smoosh
  (sm (box-immutable 1) (box-immutable 0.0))
  (known /just /known /box-immutable 0.0)
  "Smoosh meet works on unequal, comparable immutable boxes")

(check-smoosh
  (sm (box-immutable 0+i) (box-immutable 1+i))
  (unknown)
  "Smoosh meet is unknown on unequal, uncomparable immutable boxes")

(check-smoosh-eq-left
  (s= (pw /box-immutable 0) (pw /box-immutable 0.0))
  "Path-related smoosh works and preserves `eq?` when possible on equal immutable boxes")

(check-smoosh-eq-left
  (s= (pw /box-immutable 0) (pw /box-immutable 1.0))
  "Path-related smoosh works and preserves `eq?` when possible on immutable boxes with path-related elements")

(check-smoosh
  (s= (pw /box-immutable 0+i) (pw /box-immutable 1+i))
  (unknown)
  "Path-related smoosh is unknown on immutable boxes with at least one pair of corresponding elements whose path-relatedness is unknown and no pairs whose path-relatedness is known false")

(check-smoosh-eq-left
  (s= (iw /box-immutable 0) (iw /box-immutable 0))
  "Info smoosh works and preserves `eq?` when possible on shallowly `chaperone=?` immutable boxes whose elements are info smooshable")

(check-smoosh
  (s= (iw /box-immutable 0) (iw /box-immutable 0.0))
  (known /nothing)
  "Info smoosh fails on shallowly `chaperone=?` immutable boxes with a pair of corresponding elements whose info smoosh fails")

(check-smoosh
  (s= (iw ibox1-chap-chap) (iw ibox1-chap))
  (unknown)
  "Info smoosh is unknown on non-shallowly-`chaperone=?` immutable boxes even when they're shallowly `chaperone-of?` in one direction and have elements which are info smooshable")

(check-smoosh
  (s= (iw ibox1-chap) (iw ibox1-chap2))
  (unknown)
  "Info smoosh is unknown on non-shallowly-`chaperone=?` immutable boxes even when they're shallowly `equal-always?` and have elements which are info smooshable")

(check-smoosh-eq-left
  (sj (iw /box-immutable 0) (iw /box-immutable 0))
  "Info smoosh join works and preserves `eq?` when possible on shallowly `chaperone=?` immutable boxes whose elements are info smoosh joinable")

(check-smoosh-eq-left
  (sj (iw ibox1-chap-chap) (iw ibox1-chap))
  "Info smoosh join works and preserves `eq?` on shallowly `chaperone-of?` immutable boxes even when they're not shallowly `chaperone=?`")

(check-smoosh
  (sj (iw /box-immutable 0) (iw /box-immutable 0.0))
  (known /nothing)
  "Info smoosh join fails on immutable boxes with at least one pair of corresponding elements whose info smoosh join fails")

(check-smoosh
  (sj (iw ibox1-chap) (iw ibox1-chap2))
  (unknown)
  "Info smoosh join is unknown on non-shallowly-`chaperone-of?` immutable boxes even when they're `equal-always?`")

(check-smoosh-eq-left
  (sm (iw /box-immutable 0) (iw /box-immutable 0))
  "Info smoosh meet works and preserves `eq?` when possible on shallowly `chaperone=?` immutable boxes whose elements are info smoosh meetable")

(check-smoosh-eq-right
  (sm (iw ibox1-chap-chap) (iw ibox1-chap))
  "Info smoosh meet works and preserves `eq?` on shallowly `chaperone-of?` immutable boxes even when they're not shallowly `chaperone=?`")

(check-smoosh
  (sm (iw /box-immutable 0) (iw /box-immutable 0.0))
  (known /nothing)
  "Info smoosh meet fails on immutable boxes with at least one pair of corresponding elements whose info smoosh meet fails")

(check-smoosh
  (sm (iw ibox1-chap) (iw ibox1-chap2))
  (unknown)
  "Info smoosh meet is unknown on non-shallowly-`chaperone-of?` immutable boxes even when they're `equal-always?`")

(check-smoosh-eq-left
  (s= (pw /iw /box-immutable 0) (pw /iw /box-immutable 0))
  "Path-related info smoosh works and preserves `eq?` when possible on immutable boxes whose elements are path-related info smooshable")

(check-smoosh-eq-left
  (s= (pw /iw ibox1-chap-chap) (pw /iw ibox1-chap))
  "Path-related info smoosh works and preserves `eq?` on `equal-always?` immutable boxes even when they're only shallowly `chaperone-of?` in one direction")

(check-smoosh-eq-left
  (s= (pw /iw ibox1-chap) (pw /iw ibox1-chap2))
  "Path-related info smoosh works and preserves `eq?` when possible on `equal-always?` immutable boxes even when they're not shallowly `chaperone=?` in either direction")

(check-smoosh
  (s= (pw /iw /box-immutable 0) (pw /iw /box-immutable 0.0))
  (known /nothing)
  "Path-related info smoosh fails on immutable boxes with at least one pair of corresponding elements whose path-related info smoosh fails")


(check-smoosh-eq-left
  (s= (vector-immutable 0 0.0) (vector-immutable 0.0 0))
  "Smoosh works and preserves `eq?` when possible on equal immutable vectors")

(check-smoosh
  (s= (vector-immutable 0 0) (vector-immutable 1 0))
  (known /nothing)
  "Smoosh fails on unequal immutable vectors")

(check-smoosh-eq-left
  (sj (vector-immutable 0 0.0) (vector-immutable 0.0 0))
  "Smoosh join works and preserves `eq?` when possible on equal immutable vectors")

(check-smoosh
  (sj (vector-immutable 1 0) (vector-immutable 0.0 1+0.0i))
  (known /just /known /vector-immutable 1 1+0.0i)
  "Smoosh join works on unequal, comparable immutable vectors")

(check-smoosh
  (sj (vector-immutable 0 0+i) (vector-immutable 0 1+i))
  (unknown)
  "Smoosh join is unknown on unequal, uncomparable immutable vectors")

(check-smoosh-eq-left
  (sm (vector-immutable 0 0.0) (vector-immutable 0.0 0))
  "Smoosh meet works and preserves `eq?` when possible on equal immutable vectors")

(check-smoosh
  (sm (vector-immutable 1 0) (vector-immutable 0.0 1+0.0i))
  (known /just /known /vector-immutable 0.0 0)
  "Smoosh meet works on unequal, comparable immutable vectors")

(check-smoosh
  (sm (vector-immutable 0 0+i) (vector-immutable 0 1+i))
  (unknown)
  "Smoosh meet is unknown on unequal, uncomparable immutable vectors")

(check-smoosh-eq-left
  (s= (pw /vector-immutable 0 0.0) (pw /vector-immutable 0.0 0))
  "Path-related smoosh works and preserves `eq?` when possible on equal immutable vectors")

(check-smoosh-eq-left
  (s= (pw /vector-immutable 0 0.0) (pw /vector-immutable 1.0 1+0.0i))
  "Path-related smoosh works and preserves `eq?` when possible on immutable vectors with path-related elements")

(check-smoosh
  (s= (pw /vector-immutable 0 0+i) (pw /vector-immutable 0 1+i))
  (unknown)
  "Path-related smoosh is unknown on immutable vectors with at least one pair of corresponding elements whose path-relatedness is unknown and no pairs whose path-relatedness is known false")

(check-smoosh
  (s=
    (pw /vector-immutable path-failing-1 0+i)
    (pw /vector-immutable path-failing-2 1+i))
  (known /nothing)
  "Path-related smoosh fails on immutable vectors with at least one pair of corresponding elements whose path-related smoosh fails, even if another pair's path-related smoosh result is unknown")

(check-smoosh-eq-left
  (s= (iw /vector-immutable 0 0) (iw /vector-immutable 0 0))
  "Info smoosh works and preserves `eq?` when possible on shallowly `chaperone=?` immutable vectors whose elements are info smooshable")

(check-smoosh
  (s= (iw /vector-immutable 0 0) (iw /vector-immutable 0 0.0))
  (known /nothing)
  "Info smoosh fails on shallowly `chaperone=?` immutable vectors with a pair of corresponding elements whose info smoosh fails")

(check-smoosh
  (s= (iw iv1-chap-chap) (iw iv1-chap-with-prop))
  (unknown)
  "Info smoosh is unknown on non-shallowly-`chaperone=?` immutable vectors even when they're shallowly `chaperone-of?` in one direction and have elements which are info smooshable")

(check-smoosh
  (s= (iw iv1-chap) (iw iv1-chap2))
  (unknown)
  "Info smoosh is unknown on non-shallowly-`chaperone=?` immutable vectors even when they're shallowly `equal-always?` and have elements which are info smooshable")

(check-smoosh-eq-left
  (sj (iw /vector-immutable 0 0) (iw /vector-immutable 0 0))
  "Info smoosh join works and preserves `eq?` when possible on shallowly `chaperone=?` immutable vectors whose elements are info smoosh joinable")

(check-smoosh-eq-left
  (sj (iw iv1-chap-chap) (iw iv1-chap-with-prop))
  "Info smoosh join works and preserves `eq?` on shallowly `chaperone-of?` immutable vectors even when they're not shallowly `chaperone=?`")

(check-smoosh
  (sj (iw /vector-immutable 0 0) (iw /vector-immutable 0 0.0))
  (known /nothing)
  "Info smoosh join fails on immutable vectors with at least one pair of corresponding elements whose info smoosh join fails")

(check-smoosh
  (sj (iw iv1-chap) (iw iv1-chap2))
  (unknown)
  "Info smoosh join is unknown on non-shallowly-`chaperone-of?` immutable vectors even when they're `equal-always?`")

(check-smoosh-eq-left
  (sm (iw /vector-immutable 0 0) (iw /vector-immutable 0 0))
  "Info smoosh meet works and preserves `eq?` when possible on shallowly `chaperone=?` immutable vectors whose elements are info smoosh meetable")

(check-smoosh-eq-right
  (sm (iw iv1-chap-chap) (iw iv1-chap-with-prop))
  "Info smoosh meet works and preserves `eq?` on shallowly `chaperone-of?` immutable vectors even when they're not shallowly `chaperone=?`")

(check-smoosh
  (sm (iw /vector-immutable 0 0) (iw /vector-immutable 0 0.0))
  (known /nothing)
  "Info smoosh meet fails on immutable vectors with at least one pair of corresponding elements whose info smoosh meet fails")

(check-smoosh
  (sm (iw iv1-chap) (iw iv1-chap2))
  (unknown)
  "Info smoosh meet is unknown on non-shallowly-`chaperone-of?` immutable vectors even when they're `equal-always?`")

(check-smoosh-eq-left
  (s= (pw /iw /vector-immutable 0 0) (pw /iw /vector-immutable 0 0))
  "Path-related info smoosh works and preserves `eq?` when possible on immutable vectors whose elements are path-related info smooshable")

(check-smoosh-eq-left
  (s= (pw /iw iv1-chap-chap) (pw /iw iv1-chap-with-prop))
  "Path-related info smoosh works and preserves `eq?` on `equal-always?` immutable vectors even when they're only shallowly `chaperone-of?` in one direction")

(check-smoosh-eq-left
  (s= (pw /iw iv1-chap) (pw /iw iv1-chap2))
  "Path-related info smoosh works and preserves `eq?` on `equal-always?` immutable vectors even when they're not shallowly `chaperone=?` in either direction")

(check-smoosh
  (s= (pw /iw /vector-immutable 0 0) (pw /iw /vector-immutable 0 0.0))
  (known /nothing)
  "Path-related info smoosh fails on immutable vectors with at least one pair of corresponding elements whose path-related info smoosh fails")


(check-smoosh-eq-left
  (s= (iprefab 0 0.0) (iprefab 0.0 0))
  "Smoosh works and preserves `eq?` when possible on equal immutable prefab structs")

(check-smoosh
  (s= (iprefab 0 0) (iprefab 1 0))
  (known /nothing)
  "Smoosh fails on unequal immutable prefab structs")

(check-smoosh-eq-left
  (sj (iprefab 0 0.0) (iprefab 0.0 0))
  "Smoosh join works and preserves `eq?` when possible on equal immutable prefab structs")

(check-smoosh
  (sj (iprefab 1 0) (iprefab 0.0 1+0.0i))
  (known /just /known /iprefab 1 1+0.0i)
  "Smoosh join works on unequal, comparable immutable prefab structs")

(check-smoosh
  (sj (iprefab 0 0+i) (iprefab 0 1+i))
  (unknown)
  "Smoosh join is unknown on unequal, uncomparable immutable prefab structs")

(check-smoosh-eq-left
  (sm (iprefab 0 0.0) (iprefab 0.0 0))
  "Smoosh meet works and preserves `eq?` when possible on equal immutable prefab structs")

(check-smoosh
  (sm (iprefab 1 0) (iprefab 0.0 1+0.0i))
  (known /just /known /iprefab 0.0 0)
  "Smoosh meet works on unequal, comparable immutable prefab structs")

(check-smoosh
  (sm (iprefab 0 0+i) (iprefab 0 1+i))
  (unknown)
  "Smoosh meet is unknown on unequal, uncomparable immutable prefab structs")

(check-smoosh-eq-left
  (s= (pw /iprefab 0 0.0) (pw /iprefab 0.0 0))
  "Path-related smoosh works and preserves `eq?` when possible on equal immutable prefab structs")

(check-smoosh-eq-left
  (s= (pw /iprefab 0 0.0) (pw /iprefab 1.0 1+0.0i))
  "Path-related smoosh works and preserves `eq?` when possible on immutable prefab structs with path-related elements")

(check-smoosh
  (s= (pw /iprefab 0 0+i) (pw /iprefab 0 1+i))
  (unknown)
  "Path-related smoosh is unknown on immutable prefab structs with at least one pair of corresponding elements whose path-relatedness is unknown and no pairs whose path-relatedness is known false")

(check-smoosh
  (s=
    (pw /iprefab path-failing-1 0+i)
    (pw /iprefab path-failing-2 1+i))
  (known /nothing)
  "Path-related smoosh fails on immutable prefab structs with at least one pair of corresponding elements whose path-related smoosh fails, even if another pair's path-related smoosh result is unknown")

(check-smoosh-eq-left
  (s= (iw /iprefab 0 0) (iw /iprefab 0 0))
  "Info smoosh works and preserves `eq?` when possible on shallowly `chaperone=?` immutable prefab structs whose elements are info smooshable")

(check-smoosh
  (s= (iw /iprefab 0 0) (iw /iprefab 0 0.0))
  (known /nothing)
  "Info smoosh fails on shallowly `chaperone=?` immutable prefab structs with a pair of corresponding elements whose info smoosh fails")

(check-smoosh
  (s= (iw iprefab1-chap-chap) (iw iprefab1-chap-with-prop))
  (unknown)
  "Info smoosh is unknown on non-shallowly-`chaperone=?` immutable prefab structs even when they're shallowly `chaperone-of?` in one direction and have elements which are info smooshable")

(check-smoosh
  (s= (iw iprefab1-chap) (iw iprefab1-chap2))
  (unknown)
  "Info smoosh is unknown on non-shallowly-`chaperone=?` immutable prefab structs even when they're shallowly `equal-always?` and have elements which are info smooshable")

(check-smoosh-eq-left
  (sj (iw /iprefab 0 0) (iw /iprefab 0 0))
  "Info smoosh join works and preserves `eq?` when possible on shallowly `chaperone=?` immutable prefab structs whose elements are info smoosh joinable")

(check-smoosh-eq-left
  (sj (iw iprefab1-chap-chap) (iw iprefab1-chap-with-prop))
  "Info smoosh join works and preserves `eq?` on shallowly `chaperone-of?` immutable prefab structs even when they're not shallowly `chaperone=?`")

(check-smoosh
  (sj (iw /iprefab 0 0) (iw /iprefab 0 0.0))
  (known /nothing)
  "Info smoosh join fails on immutable prefab structs with at least one pair of corresponding elements whose info smoosh join fails")

(check-smoosh
  (sj (iw iprefab1-chap) (iw iprefab1-chap2))
  (unknown)
  "Info smoosh join is unknown on non-shallowly-`chaperone-of?` immutable prefab structs even when they're `equal-always?`")

(check-smoosh-eq-left
  (sm (iw /iprefab 0 0) (iw /iprefab 0 0))
  "Info smoosh meet works and preserves `eq?` when possible on shallowly `chaperone=?` immutable prefab structs whose elements are info smoosh meetable")

(check-smoosh-eq-right
  (sm (iw iprefab1-chap-chap) (iw iprefab1-chap-with-prop))
  "Info smoosh meet works and preserves `eq?` on shallowly `chaperone-of?` immutable prefab structs even when they're not shallowly `chaperone=?`")

(check-smoosh
  (sm (iw /iprefab 0 0) (iw /iprefab 0 0.0))
  (known /nothing)
  "Info smoosh meet fails on immutable prefab structs with at least one pair of corresponding elements whose info smoosh meet fails")

(check-smoosh
  (sm (iw iprefab1-chap) (iw iprefab1-chap2))
  (unknown)
  "Info smoosh meet is unknown on non-shallowly-`chaperone-of?` immutable prefab structs even when they're `equal-always?`")

(check-smoosh-eq-left
  (s= (pw /iw /iprefab 0 0) (pw /iw /iprefab 0 0))
  "Path-related info smoosh works and preserves `eq?` when possible on immutable prefab structs whose elements are path-related info smooshable")

(check-smoosh-eq-left
  (s= (pw /iw iprefab1-chap-chap) (pw /iw iprefab1-chap-with-prop))
  "Path-related info smoosh works and preserves `eq?` on `equal-always?` immutable prefab structs even when they're only shallowly `chaperone-of?` in one direction")

(check-smoosh-eq-left
  (s= (pw /iw iprefab1-chap) (pw /iw iprefab1-chap2))
  "Path-related info smoosh works and preserves `eq?` on `equal-always?` immutable prefab structs even when they're not shallowly `chaperone=?` in either direction")

(check-smoosh
  (s= (pw /iw /iprefab 0 0) (pw /iw /iprefab 0 0.0))
  (known /nothing)
  "Path-related info smoosh fails on immutable prefab structs with at least one pair of corresponding elements whose path-related info smoosh fails")


(check-smoosh-eq-left
  (s= (hash #f 0 #t 0.0) (hash #f 0.0 #t 0))
  "Smoosh works and preserves `eq?` when possible on equal immutable hash tables")

(check-smoosh
  (s= (hash #f 0 #t 0) (hash #f 1 #t 0))
  (known /nothing)
  "Smoosh fails on unequal immutable hash tables")

(check-smoosh-eq-left
  (sj (hash #f 0 #t 0.0) (hash #f 0.0 #t 0))
  "Smoosh join works and preserves `eq?` when possible on equal immutable hash tables")

(check-smoosh
  (sj (hash #f 1 #t 0) (hash #f 0.0 #t 1+0.0i))
  (known /just /known /hash #f 1 #t 1+0.0i)
  "Smoosh join works on unequal, comparable immutable hash tables")

(check-smoosh
  (sj (hash #f 0 #t 0+i) (hash #f 0 #t 1+i))
  (unknown)
  "Smoosh join is unknown on unequal, uncomparable immutable hash tables")

(check-smoosh-eq-left
  (sm (hash #f 0 #t 0.0) (hash #f 0.0 #t 0))
  "Smoosh meet works and preserves `eq?` when possible on equal immutable hash tables")

(check-smoosh
  (sm (hash #f 1 #t 0) (hash #f 0.0 #t 1+0.0i))
  (known /just /known /hash #f 0.0 #t 0)
  "Smoosh meet works on unequal, comparable immutable hash tables")

(check-smoosh
  (sm (hash #f 0 #t 0+i) (hash #f 0 #t 1+i))
  (unknown)
  "Smoosh meet is unknown on unequal, uncomparable immutable hash tables")

(check-smoosh-eq-left
  (s= (pw /hash #f 0 #t 0.0) (pw /hash #f 0.0 #t 0))
  "Path-related smoosh works and preserves `eq?` when possible on equal immutable hash tables")

(check-smoosh-eq-left
  (s= (pw /hash #f 0 #t 0.0) (pw /hash #f 1.0 #t 1+0.0i))
  "Path-related smoosh works and preserves `eq?` when possible on immutable hash tables with path-related elements")

(check-smoosh
  (s= (pw /hash #f 0 #t 0+i) (pw /hash #f 0 #t 1+i))
  (unknown)
  "Path-related smoosh is unknown on immutable hash tables with at least one pair of corresponding elements whose path-relatedness is unknown and no pairs whose path-relatedness is known false")

(check-smoosh
  (s=
    (pw /hash #f path-failing-1 #t 0+i)
    (pw /hash #f path-failing-2 #t 1+i))
  (unknown)
  "Path-related smoosh is unknown on immutable hash tables with at least one pair of corresponding elements whose path-related smoosh result is unknown, even if another pair's path-relatedness is known false")

(check-smoosh-eq-left
  (s= (iw /hash #f 0 #t 0) (iw /hash #f 0 #t 0))
  "Info smoosh works and preserves `eq?` when possible on shallowly `chaperone=?` immutable hash tables whose elements are info smooshable")

(check-smoosh
  (s= (iw /hash #f 0 #t 0) (iw /hash #f 0 #t 0.0))
  (known /nothing)
  "Info smoosh fails on shallowly `chaperone=?` immutable hash tables with a pair of corresponding elements whose info smoosh fails")

(check-smoosh
  (s= (iw ihash1-chap-chap) (iw ihash1-chap))
  (unknown)
  "Info smoosh is unknown on non-shallowly-`chaperone=?` immutable hash tables even when they're shallowly `chaperone-of?` in one direction and have elements which are info smooshable")

(check-smoosh
  (s= (iw ihash1-chap) (iw ihash1-chap2))
  (unknown)
  "Info smoosh is unknown on non-shallowly-`chaperone=?` immutable hash tables even when they're shallowly `equal-always?` and have elements which are info smooshable")

(check-smoosh-eq-left
  (sj (iw /hash #f 0 #t 0) (iw /hash #f 0 #t 0))
  "Info smoosh join works and preserves `eq?` when possible on shallowly `chaperone=?` immutable hash tables whose elements are info smoosh joinable")

(check-smoosh-eq-left
  (sj (iw ihash1-chap-chap) (iw ihash1-chap))
  "Info smoosh join works and preserves `eq?` on shallowly `chaperone-of?` immutable hash tables even when they're not shallowly `chaperone=?`")

(check-smoosh
  (sj (iw /hash #f 0 #t 0) (iw /hash #f 0 #t 0.0))
  (known /nothing)
  "Info smoosh join fails on immutable hash tables with at least one pair of corresponding elements whose info smoosh join fails")

(check-smoosh
  (sj (iw ihash1-chap) (iw ihash1-chap2))
  (unknown)
  "Info smoosh join is unknown on non-shallowly-`chaperone-of?` immutable hash tables even when they're `equal-always?`")

(check-smoosh-eq-left
  (sm (iw /hash #f 0 #t 0) (iw /hash #f 0 #t 0))
  "Info smoosh meet works and preserves `eq?` when possible on shallowly `chaperone=?` immutable hash tables whose elements are info smoosh meetable")

(check-smoosh-eq-right
  (sm (iw ihash1-chap-chap) (iw ihash1-chap))
  "Info smoosh meet works and preserves `eq?` on shallowly `chaperone-of?` immutable hash tables even when they're not shallowly `chaperone=?`")

(check-smoosh
  (sm (iw /hash #f 0 #t 0) (iw /hash #f 0 #t 0.0))
  (known /nothing)
  "Info smoosh meet fails on immutable hash tables with at least one pair of corresponding elements whose info smoosh meet fails")

(check-smoosh
  (sm (iw ihash1-chap) (iw ihash1-chap2))
  (unknown)
  "Info smoosh meet is unknown on non-shallowly-`chaperone-of?` immutable hash tables even when they're `equal-always?`")

(check-smoosh-eq-left
  (s= (pw /iw /hash #f 0 #t 0) (pw /iw /hash #f 0 #t 0))
  "Path-related info smoosh works and preserves `eq?` when possible on immutable hash tables whose elements are path-related info smooshable")

(check-smoosh-eq-left
  (s= (pw /iw ihash1-chap-chap) (pw /iw ihash1-chap))
  "Path-related info smoosh works and preserves `eq?` on `equal-always?` immutable hash tables even when they're only shallowly `chaperone-of?` in one direction")

(check-smoosh-eq-left
  (s= (pw /iw ihash1-chap) (pw /iw ihash1-chap2))
  "Path-related info smoosh works and preserves `eq?` on `equal-always?` immutable hash tables even when they're not shallowly `chaperone=?` in either direction")

(check-smoosh
  (s= (pw /iw /hash #f 0 #t 0) (pw /iw /hash #f 0 #t 0.0))
  (known /nothing)
  "Path-related info smoosh fails on immutable hash tables with at least one pair of corresponding elements whose path-related info smoosh fails")


(check-smoosh
  (s= (nothing) (nothing))
  (known /just /known /nothing)
  "Smoosh works on `nothing?` values")

(check-smoosh
  (sj (nothing) (nothing))
  (known /just /known /nothing)
  "Smoosh join works on `nothing?` values")

(check-smoosh
  (sm (nothing) (nothing))
  (known /just /known /nothing)
  "Smoosh meet works on `nothing?` values")

(check-smoosh
  (s= (pw /nothing) (pw /nothing))
  (known /just /known /pw /nothing)
  "Path-related smoosh works on `nothing?` values")

(check-smoosh
  (s= (iw /nothing) (iw /nothing))
  (known /just /known /iw /nothing)
  "Info smoosh works on `nothing?` values")

(check-smoosh
  (sj (iw /nothing) (iw /nothing))
  (known /just /known /iw /nothing)
  "Info smoosh join works on `nothing?` values")

(check-smoosh
  (sm (iw /nothing) (iw /nothing))
  (known /just /known /iw /nothing)
  "Info smoosh meet works on `nothing?` values")

(check-smoosh
  (s= (pw /iw /nothing) (pw /iw /nothing))
  (known /just /known /pw /iw /nothing)
  "Path-related info smoosh works on `nothing?` values")


(check-smoosh-eq-left
  (s= (just 0) (just 0.0))
  "Smoosh works and preserves `eq?` when possible on equal `just?` values")

(check-smoosh
  (s= (just 0) (just 1))
  (known /nothing)
  "Smoosh fails on unequal `just?` values")

(check-smoosh-eq-left
  (sj (just 0) (just 0.0))
  "Smoosh join works and preserves `eq?` when possible on equal `just?` values")

(check-smoosh
  (sj (just 1) (just 0.0))
  (known /just /known /just 1)
  "Smoosh join works on unequal, comparable `just?` values")

(check-smoosh
  (sj (just 0+i) (just 1+i))
  (unknown)
  "Smoosh join is unknown on unequal, uncomparable `just?` values")

(check-smoosh-eq-left
  (sm (just 0) (just 0.0))
  "Smoosh meet works and preserves `eq?` when possible on equal `just?` values")

(check-smoosh
  (sm (just 1) (just 0.0))
  (known /just /known /just 0.0)
  "Smoosh meet works on unequal, comparable `just?` values")

(check-smoosh
  (sm (just 0+i) (just 1+i))
  (unknown)
  "Smoosh meet is unknown on unequal, uncomparable `just?` values")

(check-smoosh-eq-left
  (s= (pw /just 0) (pw /just 0.0))
  "Path-related smoosh works and preserves `eq?` when possible on equal `just?` values")

(check-smoosh-eq-left
  (s= (pw /just 0) (pw /just 1.0))
  "Path-related smoosh works and preserves `eq?` when possible on `just?` values with path-related elements")

(check-smoosh
  (s= (pw /just 0+i) (pw /just 1+i))
  (unknown)
  "Path-related smoosh is unknown on `just?` values with at least one pair of corresponding elements whose path-relatedness is unknown and no pairs whose path-relatedness is known false")

(check-smoosh-eq-left
  (s= (iw /just 0) (iw /just 0))
  "Info smoosh works and preserves `eq?` when possible on `just?` values whose elements are info smooshable")

(check-smoosh
  (s= (iw /just 0) (iw /just 0.0))
  (known /nothing)
  "Info smoosh fails on `just?` values with a pair of corresponding elements whose info smoosh fails")

(check-smoosh-eq-left
  (sj (iw /just 0) (iw /just 0))
  "Info smoosh join works and preserves `eq?` when possible on `just?` values whose elements are info smoosh joinable")

(check-smoosh
  (sj (iw /just 0) (iw /just 0.0))
  (known /nothing)
  "Info smoosh join fails on `just?` values with at least one pair of corresponding elements whose info smoosh join fails")

(check-smoosh-eq-left
  (sm (iw /just 0) (iw /just 0))
  "Info smoosh meet works and preserves `eq?` when possible on shallowly `chaperone=?` `just?` values whose elements are info smoosh meetable")

(check-smoosh
  (sm (iw /just 0) (iw /just 0.0))
  (known /nothing)
  "Info smoosh meet fails on `just?` values with at least one pair of corresponding elements whose info smoosh meet fails")

(check-smoosh-eq-left
  (s= (pw /iw /just 0) (pw /iw /just 0))
  "Path-related info smoosh works and preserves `eq?` when possible on `just?` values whose elements are path-related info smooshable")

(check-smoosh
  (s= (pw /iw /just 0) (pw /iw /just 0.0))
  (known /nothing)
  "Path-related info smoosh fails on `just?` values with at least one pair of corresponding elements whose path-related info smoosh fails")


(check-smoosh
  (s= (nothing) (just 0))
  (known /nothing)
  "Smoosh fails on distinctive `maybe?` values")

(check-smoosh
  (sj (nothing) (just 0))
  (unknown)
  "Smoosh join is unknown on distinctive `maybe?` values")

(check-smoosh
  (sm (nothing) (just 0))
  (unknown)
  "Smoosh meet is unknown on distinctive `maybe?` values")

(check-smoosh
  (s= (pw /nothing) (pw /just 0))
  (unknown)
  "Path-related smoosh is unknown on distinctive `maybe?` values")

(check-smoosh
  (s= (iw /nothing) (iw /just 0))
  (known /nothing)
  "Info smoosh fails on distinctive `maybe?` values")

(check-smoosh
  (sj (iw /nothing) (iw /just 0))
  (known /nothing)
  "Info smoosh join fails on distinctive `maybe?` values")

(check-smoosh
  (sm (iw /nothing) (iw /just 0))
  (known /nothing)
  "Info smoosh meet fails on distinctive `maybe?` values")

(check-smoosh
  (s= (pw /iw /nothing) (pw /iw /just 0))
  (known /nothing)
  "Path-related info smoosh fails on distinctive `maybe?` values")


(check-smoosh
  (s= (trivial) (trivial))
  (known /just /known /trivial)
  "Smoosh works on `trivial?` values")

(check-smoosh
  (sj (trivial) (trivial))
  (known /just /known /trivial)
  "Smoosh join works on `trivial?` values")

(check-smoosh
  (sm (trivial) (trivial))
  (known /just /known /trivial)
  "Smoosh meet works on `trivial?` values")

(check-smoosh
  (s= (pw /trivial) (pw /trivial))
  (known /just /known /pw /trivial)
  "Path-related smoosh works on `trivial?` values")

(check-smoosh
  (s= (iw /trivial) (iw /trivial))
  (known /just /known /iw /trivial)
  "Info smoosh works on `trivial?` values")

(check-smoosh
  (sj (iw /trivial) (iw /trivial))
  (known /just /known /iw /trivial)
  "Info smoosh join works on `trivial?` values")

(check-smoosh
  (sm (iw /trivial) (iw /trivial))
  (known /just /known /iw /trivial)
  "Info smoosh meet works on `trivial?` values")

(check-smoosh
  (s= (pw /iw /trivial) (pw /iw /trivial))
  (known /just /known /pw /iw /trivial)
  "Path-related info smoosh works on `trivial?` values")


(check-smoosh-eq-left
  (s= (unknown) (unknown))
  "Smoosh works and preserves `eq?` on `example-unknown?` values")

(check-smoosh-eq-left
  (sj (unknown) (unknown))
  "Smoosh join works and preserves `eq?` on `example-unknown?` values")

(check-smoosh-eq-left
  (sm (unknown) (unknown))
  "Smoosh meet works and preserves `eq?` on `example-unknown?` values")

(check-smoosh-eq-left
  (s= (pw /unknown) (pw /unknown))
  "Path-related smoosh works and preserves `eq?` on `example-unknown?` values")

(check-smoosh-eq-left
  (s= (iw /unknown) (iw /unknown))
  "Info smoosh works and preserves `eq?` on `example-unknown?` values")

(check-smoosh-eq-left
  (sj (iw /unknown) (iw /unknown))
  "Info smoosh join works and preserves `eq?` on `example-unknown?` values")

(check-smoosh-eq-left
  (sm (iw /unknown) (iw /unknown))
  "Info smoosh meet works and preserves `eq?` on `example-unknown?` values")

(check-smoosh-eq-left
  (s= (pw /iw /unknown) (pw /iw /unknown))
  "Path-related info smoosh works and preserves `eq?` on `example-unknown?` values")


(check-smoosh-eq-left
  (s= (known 0) (known 0.0))
  "Smoosh works and preserves `eq?` when possible on equal `known?` values")

(check-smoosh
  (s= (known 0) (known 1))
  (known /nothing)
  "Smoosh fails on unequal `known?` values")

(check-smoosh-eq-left
  (sj (known 0) (known 0.0))
  "Smoosh join works and preserves `eq?` when possible on equal `known?` values")

(check-smoosh
  (sj (known 1) (known 0.0))
  (known /just /known /known 1)
  "Smoosh join works on unequal, comparable `known?` values")

(check-smoosh
  (sj (known 0+i) (known 1+i))
  (unknown)
  "Smoosh join is unknown on unequal, uncomparable `known?` values")

(check-smoosh-eq-left
  (sm (known 0) (known 0.0))
  "Smoosh meet works and preserves `eq?` when possible on equal `known?` values")

(check-smoosh
  (sm (known 1) (known 0.0))
  (known /just /known /known 0.0)
  "Smoosh meet works on unequal, comparable `known?` values")

(check-smoosh
  (sm (known 0+i) (known 1+i))
  (unknown)
  "Smoosh meet is unknown on unequal, uncomparable `known?` values")

(check-smoosh-eq-left
  (s= (pw /known 0) (pw /known 0.0))
  "Path-related smoosh works and preserves `eq?` when possible on equal `known?` values")

(check-smoosh-eq-left
  (s= (pw /known 0) (pw /known 1.0))
  "Path-related smoosh works and preserves `eq?` when possible on `known?` values with path-related elements")

(check-smoosh
  (s= (pw /known 0+i) (pw /known 1+i))
  (unknown)
  "Path-related smoosh is unknown on `known?` values with at least one pair of corresponding elements whose path-relatedness is unknown and no pairs whose path-relatedness is known false")

(check-smoosh-eq-left
  (s= (iw /known 0) (iw /known 0))
  "Info smoosh works and preserves `eq?` when possible on `known?` values whose elements are info smooshable")

(check-smoosh
  (s= (iw /known 0) (iw /known 0.0))
  (known /nothing)
  "Info smoosh fails on `known?` values with a pair of corresponding elements whose info smoosh fails")

(check-smoosh-eq-left
  (sj (iw /known 0) (iw /known 0))
  "Info smoosh join works and preserves `eq?` when possible on `known?` values whose elements are info smoosh joinable")

(check-smoosh
  (sj (iw /known 0) (iw /known 0.0))
  (known /nothing)
  "Info smoosh join fails on `known?` values with at least one pair of corresponding elements whose info smoosh join fails")

(check-smoosh-eq-left
  (sm (iw /known 0) (iw /known 0))
  "Info smoosh meet works and preserves `eq?` when possible on `known?` values whose elements are info smoosh meetable")

(check-smoosh
  (sm (iw /known 0) (iw /known 0.0))
  (unknown)
  "Info smoosh meet is unknown on `known?` values with at least one pair of corresponding elements whose info smoosh meet fails")

(check-smoosh-eq-left
  (s= (pw /iw /known 0) (pw /iw /known 0))
  "Path-related info smoosh works and preserves `eq?` when possible on `known?` values whose elements are path-related info smooshable")

(check-smoosh-eq-left
  (s= (pw /iw /known 0) (pw /iw /known 0.0))
  "Path-related info smoosh works and preserves `eq?` when possible on `known?` values with at least one pair of corresponding elements whose path-related info smoosh fails")


(check-smoosh
  (s= (unknown) (known 0))
  (unknown)
  "Smoosh is unknown on an `example-unknown?` value vs a `known?` value")

(check-smoosh
  (sj (unknown) (known 0))
  (unknown)
  "Smoosh join is unknown on an `example-unknown?` value vs a `known?` value")

(check-smoosh
  (sm (unknown) (known 0))
  (unknown)
  "Smoosh meet is unknown on an `example-unknown?` value vs a `known?` value")

(check-smoosh
  (s= (pw /unknown) (pw /known 0))
  (unknown)
  "Path-related smoosh is unknown on an `example-unknown?` value vs a `known?` value")

(check-smoosh
  (s= (iw /unknown) (iw /known 0))
  (known /nothing)
  "Info smoosh fails on an `example-unknown?` value vs a `known?` value")

(check-smoosh-eq-right
  (sj (iw /unknown) (iw /known 0))
  "Info smoosh join works and preserves `eq?` on an `example-unknown?` value vs a `known?` value")

(check-smoosh-eq-left
  (sm (iw /unknown) (iw /known 0))
  "Info smoosh meet works and preserves `eq?` on an `example-unknown?` value vs a `known?` value")

(check-smoosh-eq-left
  (s= (pw /iw /unknown) (pw /iw /known 0))
  "Path-related info smoosh works and preserves `eq?` on an `example-unknown?` value vs a `known?` value")

(check-smoosh
  (s= (iw /iw /unknown) (iw /iw /known 0))
  (known /nothing)
  "Info info smoosh fails on an `example-unknown?` value vs a `known?` value")

(check-smoosh
  (sj (iw /iw /unknown) (iw /iw /known 0))
  (known /nothing)
  "Info info smoosh join fails on an `example-unknown?` value vs a `known?` value")

(check-smoosh
  (sm (iw /iw /unknown) (iw /iw /known 0))
  (known /nothing)
  "Info info smoosh meet fails on an `example-unknown?` value vs a `known?` value")

(check-smoosh
  (s= (pw /iw /iw /unknown) (pw /iw /iw /known 0))
  (known /nothing)
  "Path-related info info smoosh fails on an `example-unknown?` value vs a `known?` value")


(check-equal?
  (gloss-count (gloss #t 0.0))
  1
  "A `gloss?` value can be made that has precisely one key")

(check-equal?
  (gloss-count (gloss #t 0.0 #f 0))
  2
  "A `gloss?` value can be made that has precisely two keys")


(check-smoosh-eq-left
  (s= (gloss #f 0 #t 0.0) (gloss #f 0.0 #t 0))
  "Smoosh works and preserves `eq?` when possible on equal `gloss?` values")

(check-smoosh
  (s= (gloss #f 0 #t 0) (gloss #f 1 #t 0))
  (known /nothing)
  "Smoosh fails on unequal `gloss?` values")

(check-smoosh-eq-left
  (sj (gloss #f 0 #t 0.0) (gloss #f 0.0 #t 0))
  "Smoosh join works and preserves `eq?` when possible on equal `gloss?` values")

(check-smoosh
  (sj (gloss #f 1 #t 0) (gloss #f 0.0 #t 1+0.0i))
  (known /just /known /gloss #f 1 #t 1+0.0i)
  "Smoosh join works on unequal, comparable `gloss?` values")

(check-smoosh
  (sj (gloss #f 0 #t 0+i) (gloss #f 0 #t 1+i))
  (unknown)
  "Smoosh join is unknown on unequal, uncomparable `gloss?` values")

(check-smoosh-eq-left
  (sm (gloss #f 0 #t 0.0) (gloss #f 0.0 #t 0))
  "Smoosh meet works and preserves `eq?` when possible on equal `gloss?` values")

(check-smoosh
  (sm (gloss #f 1 #t 0) (gloss #f 0.0 #t 1+0.0i))
  (known /just /known /gloss #f 0.0 #t 0)
  "Smoosh meet works on unequal, comparable `gloss?` values")

(check-smoosh
  (sm (gloss #f 0 #t 0+i) (gloss #f 0 #t 1+i))
  (unknown)
  "Smoosh meet is unknown on unequal, uncomparable `gloss?` values")

(check-smoosh-eq-left
  (s= (pw /gloss #f 0 #t 0.0) (pw /gloss #f 0.0 #t 0))
  "Path-related smoosh works and preserves `eq?` when possible on equal `gloss?` values")

(check-smoosh-eq-left
  (s= (pw /gloss #f 0 #t 0.0) (pw /gloss #f 1.0 #t 1+0.0i))
  "Path-related smoosh works and preserves `eq?` when possible on `gloss?` values with path-related elements")

(check-smoosh
  (s= (pw /gloss #f 0 #t 0+i) (pw /gloss #f 0 #t 1+i))
  (unknown)
  "Path-related smoosh is unknown on `gloss?` values with at least one pair of corresponding elements whose path-relatedness is unknown and no pairs whose path-relatedness is known false")

(check-smoosh
  (s=
    (pw /gloss #f path-failing-1 #t 0+i)
    (pw /gloss #f path-failing-2 #t 1+i))
  (unknown)
  "Path-related smoosh is unknown on `gloss?` values with at least one pair of corresponding elements whose path-related smoosh result is unknown, even if another pair's path-relatedness is known false")

(check-smoosh-eq-left
  (s= (iw /gloss #f 0 #t 0) (iw /gloss #f 0 #t 0))
  "Info smoosh works and preserves `eq?` when possible on shallowly `chaperone=?` `gloss?` values whose elements are info smooshable")

(check-smoosh
  (s= (iw /gloss #f 0 #t 0) (iw /gloss #f 0 #t 0.0))
  (known /nothing)
  "Info smoosh fails on shallowly `chaperone=?` `gloss?` values with a pair of corresponding elements whose info smoosh fails")

(check-smoosh-eq-left
  (sj (iw /gloss #f 0 #t 0) (iw /gloss #f 0 #t 0))
  "Info smoosh join works and preserves `eq?` when possible on shallowly `chaperone=?` `gloss?` values whose elements are info smoosh joinable")

(check-smoosh
  (sj (iw /gloss #f 0 #t 0) (iw /gloss #f 0 #t 0.0))
  (known /nothing)
  "Info smoosh join fails on `gloss?` values with at least one pair of corresponding elements whose info smoosh join fails")

(check-smoosh-eq-left
  (sm (iw /gloss #f 0 #t 0) (iw /gloss #f 0 #t 0))
  "Info smoosh meet works and preserves `eq?` when possible on shallowly `chaperone=?` `gloss?` values whose elements are info smoosh meetable")

(check-smoosh
  (sm (iw /gloss #f 0 #t 0) (iw /gloss #f 0 #t 0.0))
  (known /nothing)
  "Info smoosh meet fails on `gloss?` values with at least one pair of corresponding elements whose info smoosh meet fails")

(check-smoosh-eq-left
  (s= (pw /iw /gloss #f 0 #t 0) (pw /iw /gloss #f 0 #t 0))
  "Path-related info smoosh works and preserves `eq?` when possible on `gloss?` values whose elements are path-related info smooshable")

(check-smoosh
  (s= (pw /iw /gloss #f 0 #t 0) (pw /iw /gloss #f 0 #t 0.0))
  (known /nothing)
  "Path-related info smoosh fails on `gloss?` values with at least one pair of corresponding elements whose path-related info smoosh fails")


(check-smoosh
  (s= eaw1 eaw2)
  (known /just /known eaw1)
  "Smoosh works on `equal-always?` `equal-always-wrapper?` values")

(check-smoosh
  (s= eaw1 eaw-different)
  (known /nothing)
  "Smoosh fails on non-`equal-always?` `equal-always-wrapper?` values")

(check-smoosh
  (sj eaw1 eaw2)
  (known /just /known eaw1)
  "Smoosh join works on `equal-always?` `equal-always-wrapper?` values")

(check-smoosh
  (sj eaw1 eaw-different)
  (unknown)
  "Smoosh join is unknown on non-`equal-always?` `equal-always-wrapper?` values")

(check-smoosh
  (sm eaw1 eaw2)
  (known /just /known eaw1)
  "Smoosh meet works on `equal-always?` `equal-always-wrapper?` values")

(check-smoosh
  (sm eaw1 eaw-different)
  (unknown)
  "Smoosh meet is unknown on non-`equal-always?` `equal-always-wrapper?` values")

(check-smoosh
  (s= (pw eaw1) (pw eaw2))
  (known /just /known /pw eaw1)
  "Path-related smoosh works on `equal-always?` `equal-always-wrapper?` values")

(check-smoosh
  (s= (pw eaw1) (pw eaw-different))
  (unknown)
  "Path-related smoosh is unknown on non-`equal-always?` `equal-always-wrapper?` values")

(check-smoosh
  (s= (iw eaw1) (iw eaw2))
  (known /just /known /iw eaw1)
  "Info smoosh works on `equal-always?` `equal-always-wrapper?` values")

(check-smoosh
  (s= (iw eaw1) (iw eaw-different))
  (known /nothing)
  "Info smoosh fails on non-`equal-always?` `equal-always-wrapper?` values")

(check-smoosh
  (sj (iw eaw1) (iw eaw2))
  (known /just /known /iw eaw1)
  "Info smoosh join works on `equal-always?` `equal-always-wrapper?` values")

(check-smoosh
  (sj (iw eaw1) (iw eaw-different))
  (known /nothing)
  "Info smoosh join fails on non-`equal-always?` `equal-always-wrapper?` values")

(check-smoosh
  (sm (iw eaw1) (iw eaw2))
  (known /just /known /iw eaw1)
  "Info smoosh meet works on `equal-always?` `equal-always-wrapper?` values")

(check-smoosh
  (sm (iw eaw1) (iw eaw-different))
  (known /nothing)
  "Info smoosh meet fails on non-`equal-always?` `equal-always-wrapper?` values")

(check-smoosh
  (s= (pw /iw eaw1) (pw /iw eaw2))
  (known /just /known /pw /iw eaw1)
  "Path-related info smoosh works on `equal-always?` `equal-always-wrapper?` values")

(check-smoosh
  (s= (pw /iw eaw1) (pw /iw eaw-different))
  (known /nothing)
  "Path-related info smoosh fails on non-`equal-always?` `equal-always-wrapper?` values")


(check-smoosh
  (s= (indistinct-wrapper eaw1) (indistinct-wrapper eaw2))
  (known /just /known /indistinct-wrapper eaw1)
  "Smoosh works on `indistinct-wrapper?` values if it works on their elements")

(check-smoosh
  (s= (indistinct-wrapper eaw1) (indistinct-wrapper eaw-different))
  (unknown)
  "Smoosh is unknown on `indistinct-wrapper?` values if it fails on their elements")

(check-smoosh
  (s= (indistinct-wrapper +nan.0) (indistinct-wrapper +nan.0))
  (unknown)
  "Smoosh is unknown on `indistinct-wrapper?` values if it's unknown on their elements")

(check-smoosh
  (sj (indistinct-wrapper eaw1) (indistinct-wrapper eaw2))
  (known /just /known /indistinct-wrapper eaw1)
  "Smoosh join works on `indistinct-wrapper?` values if it works on their elements")

(check-smoosh
  (sj
    (indistinct-wrapper path-failing-1)
    (indistinct-wrapper path-failing-2))
  (unknown)
  "Smoosh join is unknown on `indistinct-wrapper?` values if it fails on their elements")

(check-smoosh
  (sj (indistinct-wrapper eaw1) (indistinct-wrapper eaw-different))
  (unknown)
  "Smoosh join is unknown on `indistinct-wrapper?` values if it's unknown on their elements")

(check-smoosh
  (sm (indistinct-wrapper eaw1) (indistinct-wrapper eaw2))
  (known /just /known /indistinct-wrapper eaw1)
  "Smoosh meet works on `indistinct-wrapper?` values if it works on their elements")

(check-smoosh
  (sm
    (indistinct-wrapper path-failing-1)
    (indistinct-wrapper path-failing-2))
  (unknown)
  "Smoosh meet is unknown on `indistinct-wrapper?` values if it fails on their elements")

(check-smoosh
  (sm (indistinct-wrapper eaw1) (indistinct-wrapper eaw-different))
  (unknown)
  "Smoosh meet is unknown on `indistinct-wrapper?` values if it's unknown on their elements")

(check-smoosh
  (s= (pw /indistinct-wrapper eaw1) (pw /indistinct-wrapper eaw2))
  (known /just /known /pw /indistinct-wrapper eaw1)
  "Path-related smoosh works on `indistinct-wrapper?` values if it works on their elements")

(check-smoosh
  (s=
    (pw /indistinct-wrapper path-failing-1)
    (pw /indistinct-wrapper path-failing-2))
  (unknown)
  "Path-related smoosh is unknown on `indistinct-wrapper?` values if it fails on their elements")

(check-smoosh
  (s=
    (pw /indistinct-wrapper eaw1)
    (pw /indistinct-wrapper eaw-different))
  (unknown)
  "Path-related smoosh is unknown on `indistinct-wrapper?` values if it's unknown on their elements")

(check-smoosh
  (s= (iw /indistinct-wrapper eaw1) (iw /indistinct-wrapper eaw2))
  (known /just /known /iw /indistinct-wrapper eaw1)
  "Info smoosh works on `indistinct-wrapper?` values if it works on their elements")

(check-smoosh
  (s=
    (iw /indistinct-wrapper eaw1)
    (iw /indistinct-wrapper eaw-different))
  (unknown)
  "Info smoosh is unknown on `indistinct-wrapper?` values if it fails on their elements")

(check-smoosh
  (s= (iw /indistinct-wrapper +nan.0) (iw /indistinct-wrapper +nan.0))
  (unknown)
  "Info smoosh is unknown on `indistinct-wrapper?` values if it's unknown on their elements")

(check-smoosh
  (sj (iw /indistinct-wrapper eaw1) (iw /indistinct-wrapper eaw2))
  (known /just /known /iw /indistinct-wrapper eaw1)
  "Info smoosh join works on `indistinct-wrapper?` values if it works on their elements")

(check-smoosh
  (sj
    (iw /indistinct-wrapper eaw1)
    (iw /indistinct-wrapper eaw-different))
  (unknown)
  "Info smoosh join is unknown on `indistinct-wrapper?` values if it fails on their elements")

(check-smoosh
  (sj (iw /indistinct-wrapper +nan.0) (iw /indistinct-wrapper +nan.0))
  (unknown)
  "Info smoosh join is unknown on `indistinct-wrapper?` values if it's unknown on their elements")

(check-smoosh
  (sm (iw /indistinct-wrapper eaw1) (iw /indistinct-wrapper eaw2))
  (known /just /known /iw /indistinct-wrapper eaw1)
  "Info smoosh meet works on `indistinct-wrapper?` values if it works on their elements")

(check-smoosh
  (sm
    (iw /indistinct-wrapper eaw1)
    (iw /indistinct-wrapper eaw-different))
  (unknown)
  "Info smoosh meet is unknown on `indistinct-wrapper?` values if it fails on their elements")

(check-smoosh
  (sm (iw /indistinct-wrapper +nan.0) (iw /indistinct-wrapper +nan.0))
  (unknown)
  "Info smoosh meet is unknown on `indistinct-wrapper?` values if it's unknown on their elements")

(check-smoosh
  (s=
    (pw /iw /indistinct-wrapper eaw1)
    (pw /iw /indistinct-wrapper eaw2))
  (known /just /known /pw /iw /indistinct-wrapper eaw1)
  "Path-related info smoosh works on `indistinct-wrapper?` values if it works on their elements")

(check-smoosh
  (s=
    (pw /iw /indistinct-wrapper eaw1)
    (pw /iw /indistinct-wrapper eaw-different))
  (unknown)
  "Path-related info smoosh is unknown on `indistinct-wrapper?` values if it fails on their elements")

(check-smoosh
  (s=
    (pw /iw /indistinct-wrapper +nan.0)
    (pw /iw /indistinct-wrapper +nan.0))
  (unknown)
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
