#lang parendown racket/base

; lathe-comforts/maybe
;
; A pair of structure types, `nothing` and `just`, intended for values
; that may or may not be provided.

;   Copyright 2017-2019, 2022 The Lathe Authors
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

(require #/only-in lathe-comforts expect fn w-)
(require #/only-in lathe-comforts/struct
  auto-equal auto-write define-imitation-simple-struct)
(require #/only-in lathe-comforts/match match/c)


(provide
  nothing
  just)
(provide #/own-contract-out
  nothing?
  just?
  just-value
  maybe?
  maybe/c
  maybe-bind
  maybe-map
  maybe-if
  maybe-min-zip*)


(define-imitation-simple-struct (nothing?) nothing
  'nothing (current-inspector) (auto-write) (auto-equal))
(ascribe-own-contract nothing? (-> any/c boolean?))

(define-imitation-simple-struct (just? just-value) just
  'just (current-inspector) (auto-write) (auto-equal))
(ascribe-own-contract just? (-> any/c boolean?))
(ascribe-own-contract just-value (-> just? any/c))

(define/own-contract (maybe? v)
  (-> any/c boolean?)
  (or (nothing? v) (just? v)))

; TODO: Give the resulting contract a better name, check that it has
; good `contract-stronger?` behavior, etc.
(define/own-contract (maybe/c c)
  (-> contract? contract?)
  (w- c (coerce-contract 'maybe/c c)
  #/rename-contract (or/c nothing? #/match/c just c)
    `(maybe/c ,(contract-name c))))

(define/own-contract (maybe-bind m func)
  (-> maybe? (-> any/c maybe?) maybe?)
  (expect m (just value) (nothing)
  #/func value))

(define/own-contract (maybe-map m func)
  (-> maybe? (-> any/c any/c) maybe?)
  (maybe-bind m #/fn value
  #/just #/func value))

(define/own-contract (maybe-if condition get-value)
  (-> any/c (-> any/c) maybe?)
  (if condition
    (just #/get-value)
    (nothing)))

(define/own-contract (maybe-min-zip* maybe-list)
  (-> (listof maybe?) (maybe/c list?))
  (expect maybe-list (cons maybe maybe-list) (just #/list)
  #/maybe-bind maybe #/fn element
  #/maybe-map (maybe-min-zip* maybe-list) #/fn element-list
    (cons element element-list)))
