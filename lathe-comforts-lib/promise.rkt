#lang parendown/slash racket/base

; lathe-comforts/promise
;
; Utilities for promises.

;   Copyright 2024 The Lathe Authors
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


(provide /own-contract-out
  promise-map)


(define/own-contract (promise-map promise on-value)
  (-> (promise/c any/c) (-> any/c any/c) (promise/c any/c))
  (delay /on-value /force promise))
