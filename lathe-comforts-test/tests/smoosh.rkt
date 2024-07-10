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
(require /only-in rackunit check-equal? check-pred)

(require lathe-comforts/private/shim)
(init-shim)

(require lathe-comforts)
(require lathe-comforts/maybe)
(require lathe-comforts/private/smoosh)
(require /only-in lathe-comforts/private/smoosh
  [info-wrapper iw]
  [path-related-wrapper pw])

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


(define flv1 (flvector 0.0))
(define flv2 (flvector 0.0))
(define fxv1 (fxvector 0))
(define fxv2 (fxvector 0))
(define str1 "a")
(define str2 "b")
(define bts1 #"a")
(define bts2 #"b")


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


; TODO: Actually run the extflonum tests on a platform for which
; `(extflonum-available?)` is `#t`.
;
; TODO: When an extflonum appears in a test's expected result, compare
; it using a pickier check than `check-equal?`, since `equal?` is
; consistent with `extfl=` and considers `-0t0` to be equal to `0t0`.


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
  (and (extflonum-available?) (s= (iw -0t0) (iw 0t0)))
  (and (extflonum-available?) (known /just /known /iw -0t0))
  "Info smoosh works on equal extflonums")

(check-equal?
  (if (extflonum-available?)
    (s= (iw 0t0) (iw 1t0))
    (known /nothing))
  (known /nothing)
  "Info smoosh fails on unequal extflonums")

(check-equal?
  (and (extflonum-available?) (sj (iw -0t0) (iw 0t0)))
  (and (extflonum-available?) (known /just /known /iw -0t0))
  "Info smoosh join works on equal extflonums")

(check-equal?
  (if (extflonum-available?)
    (sj (iw 0t0) (iw 1t0))
    (known /nothing))
  (known /nothing)
  "Info smoosh join fails on unequal extflonums")

(check-equal?
  (and (extflonum-available?) (sm (iw -0t0) (iw 0t0)))
  (and (extflonum-available?) (known /just /known /iw -0t0))
  "Info smoosh meet works on equal extflonums")

(check-equal?
  (if (extflonum-available?)
    (sm (iw 0t0) (iw 1t0))
    (known /nothing))
  (known /nothing)
  "Info smoosh meet fails on unequal extflonums")

(check-equal?
  (and (extflonum-available?) (s= (pw /iw -0t0) (pw /iw 0t0)))
  (and (extflonum-available?) (known /just /known /pw /iw -0t0))
  "Path-related info smoosh works on equal extflonums")

(check-equal?
  (if (extflonum-available?)
    (s= (pw /iw 0t0) (pw /iw 1t0))
    (known /nothing))
  (known /nothing)
  "Path-related info smoosh fails on unequal extflonums")


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


; TODO SMOOSH: Implement smooshing tests for values of the following
; types:
;
;   - Mutable strings, mutable byte strings, mutable boxes, mutable
;     vectors, prefab structs with mutable fields, and mutable hash
;     tables.
;
;   - (Done) Flvectors and fxvectors.
;
;   - (Done) Symbols and keywords.
;
;   - (Done) Empty lists.
;
;   - Cons cells.
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
;   - Regular expressions (`regexp?`) and compiled code expressions
;     (`compiled-expression?`).
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
;   - `trivial?` values.
;
;   - `known?` values and `example-unknown?` values.
;
;   - `gloss?` values.
;
;   - `dynamic-type-var-for-any-dynamic-type?` values.
;
;   - `equal-always-wrapper?` values.
;
;   - `indistinct-wrapper?` values.
