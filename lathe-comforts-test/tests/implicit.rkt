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
(require #/for-syntax #/only-in syntax/parse expr id)

(require #/for-syntax lathe-comforts/implicit)
(require #/for-syntax lathe-comforts/maybe)

(require rackunit)
(require #/only-in syntax/parse/define define-simple-macro)

(require lathe-comforts)
(require lathe-comforts/implicit)

; (We provide nothing from this module.)


; TODO: Write more unit tests.

(define-empty-aux-env)

(check-equal?
  (w- x 1
    (let-implicit-equals-transformer-binding 4 "correct"
      (w- x 2
        (quote-implicit-equals-transformer-part 4))))
  "correct"
  "Test `let-implicit-equals-transformer-binding` and `quote-implicit-equals-transformer-part` in a simple arrangement")

(check-equal?
  (let-implicit-equals-transformer-binding 4 "cor"
    (let-syntax
      (
        [
          four
          (syntax-id-rules ()
            [_ (quote-implicit-equals-transformer-part 4)])])
      (let-implicit-equals-transformer-binding 4 "rect"
        (string-append
          four
          (quote-implicit-equals-transformer-part 4)))))
  "correct"
  "Test that a quoted instance of `quote-implicit-equals-transformer-part` in a local macro definition still finds the correct lexically bound implicit variable")

(define-simple-macro
  (using-implicit-4-as-an-implementation-detail body:expr)
  (let-implicit-equals-transformer-binding 4 "rect"
    (string-append body (quote-implicit-equals-transformer-part 4))))

(check-equal?
  (let-implicit-equals-transformer-binding 4 "cor"
    (using-implicit-4-as-an-implementation-detail
      (quote-implicit-equals-transformer-part 4)))
  "correct"
  "Test that a quoted instance of `let-implicit-equals-transformer-binding` in a macro definition doesn't interfere with the caller's implicit variables")

(check-equal?
  (w- x 1
    (let-implicit-equals-transformer-bindings ([4 "cor"] [5 "rect"])
      (w- x 2
        (let-implicit-equals-transformer-bindings
          (
            [4 (local-implicit-equals-transformer-part 5)]
            [5 (local-implicit-equals-transformer-part 4)])
          (w- x 3
            (string-append
              (quote-implicit-equals-transformer-part 5)
              (quote-implicit-equals-transformer-part 4)))))))
  "correct"
  "Test using `let-implicit-equals-transformer-bindings` and `local-implicit-equals-transformer-part` to permute bindings")

(check-equal?
  (let-implicit-equals-transformer-binding 4 "correct"
    (let-implicit-equals-transformer-binding 5
      (local-implicit-equals-transformer-part 4)
      (quote-implicit-equals-transformer-part 5)))
  (let-implicit-equals-transformer-binding 4 "correct"
    (let-implicit-equals-transformer-binding 5
      (syntax-local-implicit-equals-transformer-part #'() 4)
      (quote-implicit-equals-transformer-part 5)))
  "Test that using `local-implicit-equals-transformer-part` is similar to using `syntax-local-implicit-equals-transformer-part`")

(check-equal?
  (let-implicit-equals-transformer-binding 4 "correct"
    (let-implicit-equals-transformer-binding 5
      (local-implicit-equals-transformer-part 4)
      (quote-implicit-equals-transformer-part 5)))
  (let-implicit-equals-transformer-binding 4 "correct"
    (let-implicit-equals-transformer-binding 5
      (just-value
        (syntax-local-implicit-equals-transformer-part-maybe #'() 4))
      (quote-implicit-equals-transformer-part 5)))
  "Test that using `local-implicit-equals-transformer-part` is similar to using `syntax-local-implicit-equals-transformer-part-maybe`")

(define-simple-macro
  (test-multiple-scopes four:expr five:id body:expr)
  (let-implicit-equals-transformer-bindings ([four "cor"] [5 "rect"])
    (let-syntax
      (
        [
          five
          (syntax-id-rules ()
            [_ (quote-implicit-equals-transformer-part 5)])])
      body)))

(check-equal?
  (test-multiple-scopes 4 five
    (string-append (quote-implicit-equals-transformer-part 4) five))
  "correct"
  "Test that a `let-implicit-equals-transformer-bindings` can have two bindings which each have a different set of scopes")

(check-equal?
  (test-multiple-scopes 5 five
    (string-append (quote-implicit-equals-transformer-part 5) five))
  "correct"
  "Test that a `let-implicit-equals-transformer-bindings` involving multiple sets of scopes actually lets us use the scope sets to distinguish the bindings")

(define-simple-macro
  (test-multiple-scopes-with-multiple-variables
    four:expr five:id six:expr seven:id body:expr)
  (let-implicit-equals-transformer-bindings
    ([four "co"] [5 "rr"] [six "ec"] [7 "t"])
    (let-syntax
      (
        [
          five
          (syntax-id-rules ()
            [_ (quote-implicit-equals-transformer-part 5)])]
        [
          seven
          (syntax-id-rules ()
            [_ (quote-implicit-equals-transformer-part 7)])])
      body)))

(check-equal?
  (test-multiple-scopes-with-multiple-variables 4 five 6 seven
    (string-append
      (quote-implicit-equals-transformer-part 4)
      five
      (quote-implicit-equals-transformer-part 6)
      seven))
  "correct"
  "Test that a `let-implicit-equals-transformer-bindings` can have two pairs of bindings, each pair of which shares a different set of scopes")

(check-equal?
  (let-implicit-equals-bindings
    (
      [4 "c" "o" "r"]
      [5 "r" "e" "c" "t"])
    (string-append
      (quote-implicit-equals-transformer-part 4)
      (apply string-append
        (local-implicit-equals-run-time-parts-as-list 4))
      (quote-implicit-equals-transformer-part 5)
      (apply string-append
        (local-implicit-equals-run-time-parts-as-list 5))))
  "correct"
  "Test that `let-implicit-equals-bindings` can bind both transformer bindings (available via `quote-implicit-equals-transformer-part`) and run-time bindings (available via `local-implicit-equals-run-time-parts-as-list`)")
