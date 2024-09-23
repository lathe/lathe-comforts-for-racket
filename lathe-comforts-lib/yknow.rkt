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
  make-yknow-from-value-promise-maybe-knowable-promise
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
  yknow-zip*-map
  yknow-map/knowable
  yknow-joininfo*-resumably
  yknow-joininfo*
  maybe-min-yknow-zip*-map
  yknow-maybe-yknow-joininfo*)


(define-imitation-simple-struct
  (yknow? yknow-value-promise-maybe-knowable-promise)
  yknow 'yknow (current-inspector) (auto-write))
(ascribe-own-contract yknow? (-> any/c boolean?))
(ascribe-own-contract yknow-value-promise-maybe-knowable-promise
  (-> yknow? (promise/c (knowable/c (maybe/c (promise/c any/c))))))

; TODO: Give the resulting contract a better name, check that it has
; good `contract-stronger?` behavior, etc.
(define/own-contract (yknow/c c)
  (-> contract? contract?)
  (w- c (coerce-contract 'yknow/c c)
  /rename-contract
    (match/c yknow /promise/c /knowable/c /maybe/c /promise/c c)
    `(yknow/c ,(contract-name c))))

(define/own-contract
  (make-yknow-from-value-promise-maybe-knowable-promise value-pmkp)
  (-> (promise/c (knowable/c (maybe/c (promise/c any/c)))) yknow?)
  (yknow value-pmkp))

(define/own-contract (make-yknow-from-value-knowable-promise value-kp)
  (-> (promise/c (knowable/c any/c)) yknow?)
  (make-yknow-from-value-promise-maybe-knowable-promise
    (promise-map value-kp /fn value-k
      (knowable-map value-k /fn value /just /delay/strict value))))

(define/own-contract (make-yknow-from-value value)
  (-> any/c yknow?)
  (make-yknow-from-value-knowable-promise /delay/strict /known value))

(define/own-contract (uninformative-yknow)
  (-> (yknow/c none/c))
  (make-yknow-from-value-knowable-promise /delay/strict /unknown))

(define/own-contract (yknow-value-promise-knowable y)
  (-> yknow? (knowable/c (promise/c any/c)))
  (w- pmkp (yknow-value-promise-maybe-knowable-promise y)
  /knowable-bind (force pmkp) /fn pm
  /expect pm (just p) (unknown)
  /known p))

(define/own-contract (yknow-value-knowable y)
  (-> yknow? knowable?)
  (knowable-map (yknow-value-promise-knowable y) /fn p /force p))

(define/own-contract (yknow-known-specified? v)
  (-> any/c boolean?)
  (and (yknow? v) (known? /yknow-value-promise-knowable v)))

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
  (make-yknow-from-value-promise-maybe-knowable-promise
    (on-value-pmkp /yknow-value-promise-maybe-knowable-promise y)))

(define/own-contract
  (yknow-value-promise-maybe-knowable-map y on-value-pmk)
  (->
    yknow?
    (-> (knowable/c (maybe/c (promise/c any/c)))
      (knowable/c (maybe/c (promise/c any/c))))
    yknow?)
  (yknow-value-promise-maybe-knowable-promise-map y /fn pmkp
    (promise-map pmkp /fn pmk
      (on-value-pmk pmk))))

(define/own-contract (yknow-map y on-value)
  (-> yknow? (-> any/c any/c) yknow?)
  (yknow-value-promise-maybe-knowable-map y /fn pmk
    (knowable-map pmk /fn pm
      (maybe-map pm /fn p
        (promise-map p /fn value
          (on-value value))))))

(define/own-contract (yknow-zip*-map y-list on-value)
  (-> (listof yknow?) (-> list? any/c) yknow?)
  (make-yknow-from-value-knowable-promise /delay
    (knowable-if (list-all y-list /fn y /yknow-known-specified? y) /fn
      (on-value /list-map y-list /fn y /yknow-value y))))

(define/own-contract (yknow-map/knowable y on-value-knowable)
  (-> yknow? (-> any/c knowable?) yknow?)
  (yknow-value-promise-maybe-knowable-map y /fn pmk
    (knowable-bind pmk /fn pm
      (expect pm (just p) (unknown)
      /knowable-map (on-value-knowable /force p) /fn value
        (just /delay/strict value)))))

(define/own-contract
  (yknow-joininfo*-resumably y-list on-known-specified)
  (->
    (listof yknow?)
    (-> (promise/c any/c) (listof yknow?) (promise/c any/c))
    yknow?)
  (make-yknow-from-value-promise-maybe-knowable-promise /delay
    (w-loop next y-list y-list
      (expect y-list (cons y y-list) (known /nothing)
      /w- pmkp (yknow-value-promise-maybe-knowable-promise y)
      /knowable-bind (force pmkp) /fn pm
      /expect pm (just p) (next y-list)
      /known /just /on-known-specified p y-list))))

(define/own-contract (yknow-joininfo* y-list)
  (-> (listof yknow?) yknow?)
  (yknow-joininfo*-resumably y-list /fn p y-list p))

(define/own-contract (maybe-min-yknow-zip*-map my-list on-value)
  (-> (listof (yknow/c maybe?)) (-> list? any/c) (yknow/c maybe?))
  (make-yknow-from-value-promise-maybe-knowable-promise /delay
    (if
      (list-any my-list /fn my
        (mat (yknow-value-knowable my) (known /nothing) #t #f))
      (known /just /delay/strict /nothing)
    /force /yknow-value-promise-maybe-knowable-promise
      (yknow-zip*-map my-list /fn m-list
        (just /on-value /list-map m-list /fn m /just-value m)))))

(define/own-contract (yknow-maybe-yknow-joininfo* ymy-list)
  (-> (listof (yknow/c (maybe/c (yknow/c any/c))))
    (yknow/c (maybe/c (yknow/c any/c))))
  (yknow-joininfo*-resumably ymy-list /fn ymp ymy-list
    (promise-map ymp /fn ym
      (maybe-map ym /fn y
        (yknow-joininfo*
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
