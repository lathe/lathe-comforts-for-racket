#lang parendown/slash racket/base

; lathe-comforts/yknow
;
; Yknow objects, a representation for pending computations that can
; turn out to allow user-specified results.

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
(require lathe-comforts/knowable)
(require lathe-comforts/list)
(require lathe-comforts/match)
(require lathe-comforts/maybe)
(require lathe-comforts/promise)
(require lathe-comforts/struct)


(provide /own-contract-out
  yknow?
  yknow-value-promise-maybe-knowable-promise
  yknow/c
  make-yknow-from-value-promise-maybe-knowable-promise)
(provide
  ; TODO: Stop exporting this.
  yknow-augment)
(provide /own-contract-out
  make-yknow-from-value-knowable-promise
  make-yknow-from-value
  uninformative-yknow
  yknow-value-promise-knowable
  yknow-value-knowable
  yknow-known-specified?
  yknow-value
  yknow-value-promise-maybe-knowable-promise-map
  yknow-value-promise-maybe-knowable-map
  yknow-map
  yknow-map/knowable
  yknow-joininfo*
  yknow-maybe-yknow-joininfo*)


; NOTE: When we want to debug where a `yknow?` value came from, we can
; configure this.
(define-for-syntax debugging-yknow #f)
(ifc debugging-yknow
  (define-imitation-simple-struct
    (yknow?
      yknow-debug-info
      yknow-value-promise-maybe-knowable-promise)
    yknow 'yknow (current-inspector) (auto-write))
  (define-imitation-simple-struct
    (yknow? yknow-value-promise-maybe-knowable-promise)
    yknow 'yknow (current-inspector) (auto-write)))
(ascribe-own-contract yknow? (-> any/c boolean?))
(ascribe-own-contract yknow-value-promise-maybe-knowable-promise
  (-> yknow? (promise/c (knowable/c (maybe/c (promise/c any/c))))))

; TODO: Give the resulting contract a better name, check that it has
; good `contract-stronger?` behavior, etc.
(define/own-contract (yknow/c c)
  (-> contract? contract?)
  (w- c (coerce-contract 'yknow/c c)
  /rename-contract
    (ifc debugging-yknow
      (match/c yknow any/c
        (promise/c /knowable/c /maybe/c /promise/c c))
      (match/c yknow /promise/c /knowable/c /maybe/c /promise/c c))
    `(yknow/c ,(contract-name c))))

(define/own-contract
  (make-yknow-from-value-promise-maybe-knowable-promise value-pmkp)
  (-> (promise/c (knowable/c (maybe/c (promise/c any/c)))) yknow?)
  (ifc debugging-yknow
    (yknow
      `(
         (make-yknow-from-value-promise-maybe-knowable-promise
           ,value-pmkp))
      value-pmkp)
    (yknow value-pmkp)))

(ifc debugging-yknow
  (define (yknow-augment debug-info y)
    (dissect y (yknow d pmkp)
    /yknow (cons debug-info d) pmkp))
  (define-syntax-parse-rule (yknow-augment debug-info:expr y:expr)
    (begin
      (fn debug-info)
      y)))

(define/own-contract (make-yknow-from-value-knowable-promise value-kp)
  (-> (promise/c (knowable/c any/c)) yknow?)
  (yknow-augment `(make-yknow-from-value-knowable-promise ,value-kp)
  /make-yknow-from-value-promise-maybe-knowable-promise
    (promise-map value-kp /fn value-k
      (knowable-map value-k /fn value /just /delay/strict value))))

(define/own-contract (make-yknow-from-value value)
  (-> any/c yknow?)
  (yknow-augment `(make-yknow-from-value ,value)
  /make-yknow-from-value-knowable-promise /delay/strict /known value))

(define/own-contract (uninformative-yknow)
  (-> (yknow/c none/c))
  (yknow-augment `(uninformative-yknow)
  /make-yknow-from-value-knowable-promise /delay/strict /unknown))

(define/own-contract (yknow-value-promise-knowable y)
  (-> yknow? (knowable/c promise?))
  (w- pmkp (yknow-value-promise-maybe-knowable-promise y)
  /knowable-bind (force pmkp) /fn pm
  /expect pm (just p) (unknown)
  /known p))

(define/own-contract (yknow-value-knowable y)
  (-> yknow? knowable?)
  (knowable-map (yknow-value-promise-knowable y) /fn p /force p))

(define/own-contract (yknow-known-specified? y)
  (-> any/c boolean?)
  (and (yknow? y) (known? /yknow-value-promise-knowable y)))

(define/own-contract (yknow-value y)
  (-> yknow-known-specified? any/c)
  (known-value /yknow-value-knowable y))

(define/own-contract
  (yknow-value-promise-maybe-knowable-promise-map y on-value-pmkp)
  (->
    yknow?
    (-> (promise/c (knowable/c (maybe/c (promise/c any/c))))
      (promise/c (knowable/c (maybe/c (promise/c any/c)))))
    yknow?)
  (yknow-augment `(yknow-value-promise-maybe-knowable-promise-map ,y ,on-value-pmkp)
  /make-yknow-from-value-promise-maybe-knowable-promise
    (on-value-pmkp /yknow-value-promise-maybe-knowable-promise y)))

(define/own-contract
  (yknow-value-promise-maybe-knowable-map y on-value-pmk)
  (->
    yknow?
    (-> (knowable/c (maybe/c (promise/c any/c)))
      (knowable/c (maybe/c (promise/c any/c))))
    yknow?)
  (yknow-augment `(yknow-value-promise-maybe-knowable-map ,y ,on-value-pmk)
  /yknow-value-promise-maybe-knowable-promise-map y /fn pmkp
    (promise-map pmkp /fn pmk
      (on-value-pmk pmk))))

(define/own-contract (yknow-map y on-value)
  (-> yknow? (-> any/c any/c) yknow?)
  (yknow-augment `(yknow-map ,y ,on-value)
  /yknow-value-promise-maybe-knowable-map y /fn pmk
    (knowable-map pmk /fn pm
      (maybe-map pm /fn p
        (promise-map p /fn value
          (on-value value))))))

(define/own-contract (yknow-map/knowable y on-value-knowable)
  (-> yknow? (-> any/c knowable?) yknow?)
  (yknow-augment `(yknow-map/knowable ,y ,on-value-knowable)
  /yknow-value-promise-maybe-knowable-map y /fn pmk
    (knowable-bind pmk /fn pm
      (expect pm (just p) (unknown)
      /knowable-map (on-value-knowable /force p) /fn value
        (just /delay/strict value)))))

(define/own-contract (yknow-joininfo*-on-promise y-list on-promise)
  (-> (listof yknow?) (-> (listof yknow?) promise? promise?) yknow?)
  (yknow-augment `(yknow-joininfo*-on-promise ,y-list ,on-promise)
  /make-yknow-from-value-promise-maybe-knowable-promise /delay
    (w-loop next y-list y-list
      (expect y-list (cons y y-list) (known /nothing)
      /w- pmkp (yknow-value-promise-maybe-knowable-promise y)
      /knowable-bind (force pmkp) /fn pm
      /expect pm (just p) (next y-list)
      /known /just /on-promise y-list p))))

(define/own-contract (yknow-joininfo* y-list)
  (-> (listof yknow?) yknow?)
  (yknow-augment `(yknow-joininfo* ,y-list)
  /yknow-joininfo*-on-promise y-list /fn y-list p p))

(define/own-contract (yknow-maybe-yknow-joininfo* ymy-list)
  (-> (listof (yknow/c (maybe/c (yknow/c any/c))))
    (yknow/c (maybe/c (yknow/c any/c))))
  (yknow-augment `(yknow-maybe-yknow-joininfo* ,ymy-list)
  /yknow-joininfo*-on-promise ymy-list /fn ymy-list ymp
    (promise-map ymp /fn ym
      (maybe-map ym /fn y
        (yknow-augment `(yknow-maybe-yknow-joininfo*/inner ,ymy-list ,y)
        /yknow-joininfo*
          (cons y
            (list-map ymy-list /fn ymy
              (yknow-value-promise-maybe-knowable-map ymy /fn ympmk
                (expect ympmk (known ympm)
                  ; TODO: Add details to this error message.
                  (raise-arguments-error 'yknow-maybe-yknow-joininfo*
                    "encountered a not-known-if-specified outer yknow after trying a known-specified one, which means the resulting outer yknow should have been not-known-if-specified all along, but we can't back up now"
                    "ympmk" ympmk)
                /expect ympm (just ymp) (known /nothing)
                /expect (force ymp) (just y)
                  ; TODO: Add details to this error message.
                  (raise-arguments-error 'yknow-maybe-yknow-joininfo*
                    "encountered a known-specified yknow with a `nothing?` value after trying a known-specified yknow with a `just?` value, meaning we have contradictory information"
                    "ym" (force ymp)
                    "ymy" ymy)
                /force /yknow-value-promise-maybe-knowable-promise
                  y)))))))))
