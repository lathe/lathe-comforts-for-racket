#lang parendown racket/base

; lathe-comforts/contract
;
; Utilities for contracts.

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


(require #/for-syntax #/only-in syntax/parse expr id)
(require #/only-in syntax/parse/define define-simple-macro)

(require #/only-in lathe-comforts w-)

; TODO: Document these exports.
(provide fix/c)


; NOTE: This takes the same options `recursive-contract` does, and it
; passes them along unmodified.
(define-simple-macro (fix/c var:id options ... contract:expr)
  (let ()
    (define var
      (w- var (recursive-contract var options ...)
        contract))
    var))
