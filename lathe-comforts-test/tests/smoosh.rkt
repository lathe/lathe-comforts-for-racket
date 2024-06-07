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


(require lathe-comforts/private/shim)
(init-shim)

(require lathe-comforts)
(require lathe-comforts/maybe)
(require lathe-comforts/private/smoosh)

(require /only-in rackunit check-equal?)

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


(define (smooshable==-exercise-knowable-maybe-knowable a b)
  (knowable-map
    (force
      (smoosh-report-==-knowable-promise-maybe-knowable-promise
        (smoosh-and-comparison-of-two-report-get-smoosh-report
          (smoosh-and-comparison-of-two-report-exercise
            (sequence-first
              (dynamic-type-get-smoosh-and-comparison-of-two-reports
                (any-dynamic-type)
                a
                b))))))
    (fn kpm
      (maybe-map kpm /fn kp
        (force kp)))))


(check-equal?
  (smooshable==-exercise-knowable-maybe-knowable 'a 'a)
  (known /just /known 'a)
  "Smoosh works on equal symbols")

(check-equal?
  (smooshable==-exercise-knowable-maybe-knowable 'a 'b)
  (known /nothing)
  "Smoosh fails on unequal symbols")
