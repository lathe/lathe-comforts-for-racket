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
  -> any/c contract? contract-name contract-out or/c rename-contract)
(require #/only-in racket/contract/combinator coerce-contract)

(require #/only-in lathe-comforts expect fn w-)
(require #/only-in lathe-comforts/struct
  auto-equal auto-write define-imitation-simple-struct)
(require #/only-in lathe-comforts/match match/c)


(provide nothing just)
(provide #/contract-out
  [nothing? (-> any/c boolean?)]
  [just? (-> any/c boolean?)]
  [just-value (-> just? any/c)]
  [maybe? (-> any/c boolean?)]
  [maybe/c (-> contract? contract?)]
  [maybe-bind (-> maybe? (-> any/c maybe?) maybe?)]
  [maybe-map (-> maybe? (-> any/c any/c) maybe?)]
  [maybe-if (-> any/c (-> any/c) maybe?)]
)


(define-imitation-simple-struct (nothing?) nothing
  'nothing (current-inspector) (auto-write) (auto-equal))

(define-imitation-simple-struct (just? just-value) just
  'just (current-inspector) (auto-write) (auto-equal))

(define (maybe? v)
  (or (nothing? v) (just? v)))

; TODO: Give the resulting contract a better name, check that it has
; good `contract-stronger?` behavior, etc.
(define (maybe/c c)
  (w- c (coerce-contract 'maybe/c c)
  #/rename-contract (or/c nothing? #/match/c just c)
    `(maybe/c ,(contract-name c))))

(define (maybe-bind m func)
  (expect m (just value) (nothing)
  #/func value))

(define (maybe-map m func)
  (maybe-bind m #/fn value
  #/just #/func value))

(define (maybe-if condition get-value)
  (if condition
    (just #/get-value)
    (nothing)))
