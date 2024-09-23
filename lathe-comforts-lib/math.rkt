#lang parendown/slash racket/base

; lathe-comforts/math
;
; Utilities for Racket's numeric types.

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


(provide /own-contract-out
  nan-number?
  non-nan-number?
  normalize-non-nan-number
  nan-extflonum?
  non-nan-extflonum?
  normalize-non-nan-extflonum)


(define/own-contract (nan-number? v)
  (-> any/c boolean?)
  (and
    (number? v)
    (or
      (nan? /real-part v)
      (nan? /imag-part v))))

(define/own-contract (non-nan-number? v)
  (-> any/c boolean?)
  (and (number? v) (not /nan-number? v)))

(define/own-contract (normalize-non-nan-number n)
  (-> non-nan-number? non-nan-number?)
  (define (normalize-non-nan-real n)
    (if (rational? n) (inexact->exact n)
    ; If the real number to normalize is infinity or negative
    ; infinity, we return its double-precision version.
    /if (positive? n) +inf.0 -inf.0))
  (make-rectangular
    (normalize-non-nan-real /real-part n)
    (normalize-non-nan-real /imag-part n)))

(define/own-contract (non-nan-extflonum? v)
  (-> any/c boolean?)
  (and (extflonum? v) (extfl= v v)))

(define/own-contract (nan-extflonum? v)
  (-> any/c boolean?)
  (and (extflonum? v) (not /non-nan-extflonum? v)))

(define/own-contract (normalize-non-nan-extflonum n)
  (-> non-nan-extflonum? non-nan-extflonum?)
  (if (equal-always? n -0t0)
    0t0
    n))
