#lang parendown/slash racket/base

; shim.rkt
;
; Import lists, debugging constants, and other utilities that are
; useful primarily for this codebase.

;   Copyright 2022 The Lathe Authors
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


(require /only-in reprovide/reprovide reprovide)

(reprovide lathe-comforts/scribblings/private/codebasewide-requires)

(provide
  init-shim)


; NOTE: If we ever have something we want to do at the beginning of
; each module, we'll put it here.
(define-syntax-parse-rule (init-shim)
  (begin))
