#lang parendown racket/base

; lathe-comforts/trivial
;
; A structure type, `trivial`, intended for values that never vary.

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


(require #/only-in racket/contract/base -> any/c contract-out)

(require #/only-in lathe-comforts/struct
  auto-equal auto-write define-imitation-simple-struct)


(provide trivial)
(provide #/contract-out [trivial? (-> any/c boolean?)])


(define-imitation-simple-struct (trivial?) trivial
  'trivial (current-inspector) (auto-write) (auto-equal))
