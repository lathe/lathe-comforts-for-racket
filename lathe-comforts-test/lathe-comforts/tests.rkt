#lang parendown racket/base

; lathe-comforts/tests
;
; Unit tests.

;   Copyright 2018 The Lathe Authors
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


(require #/only-in racket/match match match-lambda)
(require rackunit)

(require lathe-comforts)

; (We provide nothing from this module.)


; TODO: Write more unit tests.

(check-equal? (pass 3 add1) 4
  "Test `pass`")

(check-equal?
  (destx (list #'(a b c) (list #'d #'e)))
  (list '(a b c) (list 'd 'e))
  "Test `destx`")


; Tests corresponding to documentation examples

(check-equal?
  (match (list 1 2 3)
    [(list) #f]
    [(cons first rest) rest])
  (list 2 3))

(check-equal?
  (pass (list 1 2 3) #/match-lambda
    [(list) #f]
    [(cons first rest) rest])
  (list 2 3))

(check-equal?
  (w- ([a 1] [b 2])
    (+ a b))
  3
  "Test `w-` with full verbosity.")

(check-equal?
  (w- [a 1 b 2]
    (+ a b))
  3
  "Test `w-` with medium verbosity.")

(check-equal?
  (w- a 1 b 2
    (+ a b))
  3
  "Test `w-` with low verbosity.")

(check-equal?
  (hash-map (hash 'a 1 'b 2) #/fn k v
    (format "(~s, ~s)" k v))
  (list "(a, 1)" "(b, 2)"))

(check-equal?
  (build-list 5 #/fn ~ #/* 10 ~)
  (list 0 10 20 30 40))

(check-equal?
  (w-loop next original (list 1 2 3) result (list)
    (expect original (cons first rest) result
    #/next rest #/cons (* first first) result))
  (list 9 4 1))

(define (rev lst)
  (w-loop next lst lst result (list)
    
    ; If the list is empty, we're done.
    (mat lst (list) result
    
    ; Take apart the list, which must be a cons cell. If this doesn't
    ; work, raise an error.
    #/expect lst (cons first rest)
      (error "Expected a list")
    
    ; Continue the loop, removing `first` from the input and adding it
    ; to the output.
    #/next rest #/cons first result)))

(check-equal?
  (rev #/list 1 2 3)
  (list 3 2 1))

(check-exn exn:fail? #/fn
  (rev 3))
