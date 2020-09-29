#lang parendown racket/base

; lathe-comforts/tests/implicit
;
; Unit tests for implicit variables.

;   Copyright 2020 The Lathe Authors
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


(require rackunit)

(require lathe-comforts)
(require lathe-comforts/implicit)

; (We provide nothing from this module.)


; TODO: Write more unit tests.

; TODO NOW: Figure out some more tests to do on implicit variables. We
; We should write at least one test where we use
; `let-implicits-with-scopes` to bind at least two implicit variables
; at the same time with different scope sets.

;(define-empty-aux-env)

#|
(check-equal? "correct"
  (w- x 1
    (let-implicit 4 "correct"
      (w- x 2
        (quote-implicit 4))))
  "Test `let-implicit` and `quote-implicit` in a simple arrangement")

(check-equal? "correct"
  (let-implicit 4 "cor"
    (let-syntax ([four (syntax-id-rules () [_ (quote-implicit 4)])])
      (let-implicit 4 "rect"
        (string-append four (quote-implicit 4)))))
  "Test that a quoted instance of `quote-implicit` in a local macro definition still finds the correct lexically bound implicit variable")

(define-simple-macro
  (using-implicit-4-as-an-implementation-detail body:expr)
  (let-implicit 4 "rect"
    (string-append body (quote-implicit 4))))

(check-equal? "correct"
  (let-implicit 4 "cor"
    (using-implicit-4-as-an-implementation-detail
      (quote-implicit 4)))
  "Test that a quoted instance of `let-implicit` in a macro definition doesn't interfere with the caller's implicit variables")
|#
