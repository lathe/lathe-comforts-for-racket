#lang parendown racket/base

; lathe-comforts/tests
;
; Unit tests.

;   Copyright 2018, 2019 The Lathe Authors
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


(require #/only-in racket/contract/base contract? listof or/c)
(require #/only-in racket/match match match-lambda)
(require #/only-in racket/port with-output-to-string)
(require rackunit)

(require #/only-in parendown pd)

(require lathe-comforts)
(require lathe-comforts/contract)
(require lathe-comforts/match)
(require lathe-comforts/private/experimental/match)
(require lathe-comforts/struct)

; (We provide nothing from this module.)


; TODO: Write more unit tests.

(check-equal? (pass 3 add1) 4
  "Test `pass`")


(struct should-only-contain-strings (value))

(define-match-expander-attenuated make-should-only-contain-strings
  should-only-contain-strings string?)

(check-equal?
  (dissect (make-should-only-contain-strings "hello")
    (make-should-only-contain-strings value)
    value)
  "hello"
  "Call and match on an attenuated match expander successfully")

(check-exn exn:fail:contract?
  (fn
    (make-should-only-contain-strings 4))
  "Erroneously call an attenuated match expander with values that don't meet the contracts")

(define test-ns (make-base-namespace))
(void
  (eval
    '(begin
       (require lathe-comforts/match)
       (define-match-expander-attenuated
         make-should-only-contain-strings
         should-only-contain-strings
         string?))
    test-ns))

(check-exn exn:fail:syntax?
  (fn
    (eval
      '(make-should-only-contain-strings "hello" "world")
      test-ns))
  "Erroneously call an attenuated match expander with the wrong number of values")

(check-exn exn:fail:syntax?
  (fn
    (eval 'make-should-only-contain-strings test-ns))
  "Erroneously use an attenuated match expander as an identifier")


(define-match-expander-via-lists my-vector vector
  (fn v n #/and (vector? v) (= n #/vector-length v) #/vector->list v))

(check-equal?
  (dissect (my-vector 1 2 3) (my-vector a b c)
    c)
  3
  "Call and match on a via-lists match expander successfully")

(check-equal?
  (object-name my-vector)
  'vector
  "The name of a via-lists match expander is not modified from its function version")

(let ()
  (define-imitation-simple-struct foo foo? (foo-val1 foo-val2)
    (current-inspector)
    'foo
    (auto-write))
  
  (check-equal?
    (with-output-to-string #/fn #/write #/foo (list 1) 2)
    "#<foo: (1) 2>"
    "Writing a structure whose structure type uses (auto-write) writes an unreadable value with all the contents exposed")
  
  (check-equal?
    (with-output-to-string #/fn #/print #/foo (list 1) 2)
    "(foo '(1) 2)"
    "Printing a structure whose structure type uses (auto-write) with quoting depth 0 writes a constructor-style expression with all the contents exposed")
  
  (check-equal?
    (with-output-to-string #/fn
      (print (foo (list 1) 2) (current-output-port) 1))
    "#<foo: (1) 2>"
    "Writing a structure whose structure type uses (auto-write) with quoting depth 1 writes an unreadable value with all the contents exposed")
  
  )


; Tests corresponding to documentation examples

(check-equal?
  (match (list 1 2 3)
    [(list) #f]
    [(cons first rest) rest])
  (list 2 3))

(check-equal?
  (pd / pass (list 1 2 3) / match-lambda
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
  (pd / hash-map (hash 'a 1 'b 2) / fn k v
    (format "(~s, ~s)" k v))
  (list "(a, 1)" "(b, 2)"))

(check-equal?
  (pd / build-list 5 / fn ~ / * 10 ~)
  (list 0 10 20 30 40))

(check-equal?
  (pd / w-loop next original (list 1 2 3) result (list)
    (expect original (cons first rest) result
    / next rest / cons (* first first) result))
  (list 9 4 1))

(pd / define (rev lst)
  (w-loop next lst lst result (list)
    
    ; If the list is empty, we're done.
    (mat lst (list) result
    
    ; Take apart the list, which must be a cons cell. If this doesn't
    ; work, raise an error.
    / expect lst (cons first rest)
      (error "Expected a list")
    
    ; Continue the loop, removing `first` from the input and adding it
    ; to the output.
    / next rest / cons first result)))

(check-equal?
  (rev (list 1 2 3))
  (list 3 2 1))

(check-exn exn:fail? #/fn
  (rev 3))

(check-equal?
  (contract?
    (fix/c simple-s-expression/c
      (or/c symbol? (listof simple-s-expression/c))))
  #t)
