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
