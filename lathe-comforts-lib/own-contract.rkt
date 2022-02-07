#lang reprovide

; lathe-comforts/own-contract
;
; A framework for defining contracts close by to the functions they're
; for, with configuration options for suppressing external contracts
; and/or activating certain internal contracts.

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


(only-in lathe-comforts/private/shim
  define-own-contract-policies
  own-contract-out
  ascribe-own-contract
  define/own-contract)
