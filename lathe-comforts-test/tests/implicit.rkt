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


(require #/for-syntax racket/base)
(require #/for-syntax #/only-in syntax/parse expr)

(require #/for-syntax lathe-comforts/implicit)
(require #/for-syntax lathe-comforts/maybe)

(require rackunit)
(require #/only-in syntax/parse/define define-simple-macro)

(require lathe-comforts)
(require lathe-comforts/implicit)

; (We provide nothing from this module.)


; TODO: Write more unit tests.

; TODO NOW: Figure out some more tests to do on implicit variables. We
; We should write at least one test where we use
; `let-implicits-with-scopes` to bind at least two implicit variables
; at the same time with different scope sets.

(define-empty-aux-env)

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

(check-equal? "correct"
  (w- x 1
    (let-implicits ([4 "cor"] [5 "rect"])
      (w- x 2
        (let-implicits ([4 (local-implicit 5)] [5 (local-implicit 4)])
          (w- x 3
            (string-append (quote-implicit 5) (quote-implicit 4)))))))
  "Test using `let-implicits` and `local-implicit` to permute bindings")

(check-equal?
  (let-implicit 4 "correct"
    (let-implicit 5 (local-implicit 4)
      (quote-implicit 5)))
  (let-implicit 4 "correct"
    (let-implicit 5 (syntax-local-implicit-value #'() 4)
      (quote-implicit 5)))
  "Test that using `local-implicit` is similar to using `syntax-local-implicit-value`")

(check-equal?
  (let-implicit 4 "correct"
    (let-implicit 5 (local-implicit 4)
      (quote-implicit 5)))
  (let-implicit 4 "correct"
    (let-implicit 5
      (just-value #/syntax-local-implicit-value-maybe #'() 4)
      (quote-implicit 5)))
  "Test that using `local-implicit` is similar to using `syntax-local-implicit-value-maybe`")
