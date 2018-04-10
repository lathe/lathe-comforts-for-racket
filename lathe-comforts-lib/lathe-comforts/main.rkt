#lang parendown racket/base

; lathe-comforts
;
; Evergreen utilities.

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


(require #/only-in racket/contract/base -> any any/c contract-out)

(require lathe-comforts/private)

(provide
  
  ; === Evergreen utilities for binding syntax and FP ===
  
  ; == Binding syntax utilities ==
  binds
  define-simple-normalizing-binder
  
  ; == Functional programming utilities ==
  
  ; Bindings and recursion
  (contract-out [pass (-> any/c (-> any/c any) any)])
  w-
  fn
  w-loop
  loopfn
  
  ; Conditionals
  mat
  expect
  matfns
  expectfn
  dissect
  dissectfn
  
  
  ; === Unit testing utilities ===
  ;
  ; TODO: Move this into its own module.
  
  (contract-out [destx (-> any/c any)])
)
