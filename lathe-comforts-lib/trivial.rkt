#lang parendown racket/base

; lathe-comforts/trivial
;
; A structure type, `trivial`, intended for values that never vary.

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


(require #/only-in racket/contract/base -> any/c contract-out)

(require #/only-in lathe-comforts/struct struct-easy)
(require #/only-in lathe-comforts/match
  define-match-expander-attenuated)


(provide trivial)
(provide #/contract-out [trivial? (-> any/c boolean?)])


(module private/struct racket/base
  (require #/only-in lathe-comforts/struct struct-easy)
  (provide #/all-defined-out)
  (struct-easy (trivial) #:equal))
(require #/prefix-in struct: 'private/struct)

(define-match-expander-attenuated trivial struct:trivial)

(define (trivial? v)
  (struct:trivial? v))
