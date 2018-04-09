#lang parendown racket/base

; Copyright 2018 The Lathe Authors
;
; Licensed under the Apache License, Version 2.0 (the "License"); you
; may not use this file except in compliance with the License. You may
; obtain a copy of the License at
;
;     http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS,
; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
; implied. See the License for the specific language governing
; permissions and limitations under the License.


(require #/only-in racket/contract/base -> any any/c contract-out)

(require lathe-comforts/private)

(provide
  fn
  thunk
  zap!
  w-
  ret
  accum
  magic-withlike
  namedlet
  named
  rfn
  letr
  w-stxparam
  w-anaphor
  define-simple-syntax-parameter
  a
  b
  abfn
  it
  zapit!
  next
  nextlet
  nextfn
  (contract-out [pass (-> any/c (-> any/c any) any)])
  mat
  matfns
  expect
  expectfn
  dissect
  dissectfn
  destx
)
