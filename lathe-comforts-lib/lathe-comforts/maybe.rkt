#lang parendown racket/base

; lathe-comforts/maybe
;
; A pair of structure types, `nothing` and `just`, intended for values
; that may or may not be provided.

;   Copyright 2017-2018 The Lathe Authors
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


(require #/only-in racket/contract/base
  -> any/c chaperone-contract? or/c struct/c)
(require #/only-in racket/contract/region define/contract)

(require #/only-in lathe-comforts expect fn)
(require #/only-in lathe-comforts/struct struct-easy)


(provide
  (struct-out nothing)
  (struct-out just)
  maybe? maybe/c
  
  ; TODO: Document these exports.
  maybe-bind
  maybe-map
)


(struct-easy (nothing) #:equal)
(struct-easy (just value) #:equal)

(define/contract (maybe? x)
  (-> any/c boolean?)
  (or (nothing? x) (just? x)))

(define/contract (maybe/c c)
  (-> chaperone-contract? chaperone-contract?)
  (or/c nothing? #/struct/c just c))

(define/contract (maybe-bind m func)
  (-> maybe? (-> any/c maybe?) maybe?)
  (expect m (just value) (nothing)
  #/func value))

(define/contract (maybe-map m func)
  (-> maybe? (-> any/c any/c) maybe?)
  (maybe-bind m #/fn value
  #/just #/func value))
