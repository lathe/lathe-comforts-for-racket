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


(for-syntax /combine-in/fallback
  (combine-in
    (only-in racket/contract/base -> or/c)
    (only-in racket/format ~a)
    (only-in racket/list append* check-duplicates last range)
    (only-in racket/provide-transform
      make-provide-pre-transformer pre-expand-export)
    (only-in racket/struct-info extract-struct-info struct-info?)
    (only-in racket/syntax format-id syntax-local-eval)
    (only-in racket/unit rename)
    (only-in syntax/contract wrap-expr/c)
    (only-in syntax/parse
      ...+ ~! ~alt ~and ~bind ~fail ~literal ~not ~optional ~or* ~parse ~seq ~var attribute define-splicing-syntax-class define-syntax-class expr expr/c id keyword nat pattern syntax-parse this-syntax))
  racket/base)

(only-in racket/contract/base
  -> ->* ->i </c and/c any any/c chaperone-contract? cons/c contract? contract-name contract-out flat-contract? get/build-late-neg-projection list/c listof none/c non-empty-listof or/c promise/c procedure-arity-includes/c recontract-out recursive-contract rename-contract struct-type-property/c unconstrained-domain->)
(only-in racket/contract/combinator
  blame-add-context coerce-chaperone-contract coerce-contract coerce-flat-contract contract-first-order-passes? make-chaperone-contract make-contract make-flat-contract raise-blame-error)
(only-in racket/contract/region invariant-assertion)
(only-in racket/extflonum extflonum? extfl<= extfl= extfl>=)
(only-in racket/hash hash-union)
(only-in racket/list append-map range)
(only-in racket/match
  define-match-expander match match-define match/derived match-lambda)
(only-in racket/math nan? natural?)
(only-in racket/promise delay delay/strict force promise?)
(only-in racket/sequence sequence/c sequence-map sequence-ref)
(only-in racket/stream stream*)
(only-in racket/struct make-constructor-style-printer)
(only-in racket/vector vector-copy vector-count)
(only-in syntax/parse/define
  define-syntax-parser define-syntax-parse-rule)

; TODO SHIM: See if we should add `lathe-comforts/private`'s imports from
; `racket/parse`. to the shim. Our other modules don't depend on
; `syntax/parse`'s parsing framework at run time.
