#lang parendown/slash reprovide

; codebasewide-requires.rkt
;
; An import list that's useful primarily for this codebase.

;   Copyright 2022, 2024 The Lathe Authors
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


(only-in scribble/example examples make-eval-factory)
(only-in syntax/parse/define define-syntax-parse-rule)

(for-label /combine-in/fallback
  
  (combine-in
    (only-in racket/contract struct-type-property/c)
    (only-in racket/contract/base
      -> </c and/c any any/c cons/c contract-out chaperone-contract? contract? contract-name flat-contract? listof none/c non-empty-listof or/c promise/c recursive-contract struct/c unconstrained-domain->)
    (only-in racket/contract/combinator
      coerce-chaperone-contract coerce-contract coerce-flat-contract make-chaperone-contract make-contract make-flat-contract)
    (only-in racket/contract/region
      define/contract invariant-assertion)
    (only-in racket/generic define-generics)
    (only-in racket/list append-map)
    (only-in racket/match exn:misc:match? match match-lambda)
    (only-in racket/math natural?)
    (only-in racket/promise force promise?)
    (only-in racket/sequence sequence/c)
    (only-in racket/stream stream*)
    (only-in racket/struct make-constructor-style-printer)
    (only-in racket/struct-info extract-struct-info)
    (only-in syntax/parse expr id define-syntax-class)
    (only-in syntax/parse/define define-simple-macro)
    
    (only-in parendown pd)
    
    lathe-comforts
    lathe-comforts/contract
    lathe-comforts/hash
    lathe-comforts/knowable
    lathe-comforts/list
    lathe-comforts/match
    lathe-comforts/maybe
    lathe-comforts/own-contract
    lathe-comforts/promise
    lathe-comforts/sequence
    lathe-comforts/string
    lathe-comforts/struct
    lathe-comforts/trivial
    lathe-comforts/yknow)
  
  racket/base)
