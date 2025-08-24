#lang parendown/slash racket/base

; shim.rkt
;
; Import lists, debugging constants, and other utilities that are
; useful primarily for this codebase.

;   Copyright 2022, 2025 The Lathe Authors
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


; TODO: Putting `for-label` in
; `lathe-comforts/scribblings/private/codebasewide-requires` doesn't
; work, so we use a very unhygienic macro that expands to `require`
; here intsead. Didn't it work at some point? Technically we could be
; meticulous and only adjust the scopes on the module names we're
; requiring here, but that makes it harder to pull these back out into
; a regular `require` if we figure out a way to do so.
;
(define-syntax-parse-rule/autoptic (init-shim)
  
  #:with result
  (datum->syntax this-syntax /syntax->datum
    #`
    (begin
      (require /only-in
        reprovide/require-transformer/combine-in-fallback
        combine-in/fallback)
      (require /for-label /combine-in/fallback
        (combine-in
          (only-in racket/contract/base
            -> </c and/c any any/c cons/c contract-out chaperone-contract? contract? contract-name flat-contract? listof or/c recursive-contract struct/c)
          (only-in racket/contract/combinator
            coerce-chaperone-contract coerce-contract coerce-flat-contract make-chaperone-contract make-contract make-flat-contract)
          (only-in racket/contract/region
            define/contract invariant-assertion)
          (only-in racket/generic define-generics)
          (only-in racket/list append-map)
          (only-in racket/match exn:misc:match? match match-lambda)
          (only-in racket/math natural?)
          (only-in racket/struct make-constructor-style-printer)
          (only-in racket/struct-info extract-struct-info)
          (only-in syntax/parse expr id this-syntax syntax-parse)
          (only-in syntax/parse/define define-syntax-parse-rule)
          
          (only-in parendown pd)
          
          lathe-comforts
          lathe-comforts/contract
          lathe-comforts/hash
          lathe-comforts/list
          lathe-comforts/match
          lathe-comforts/maybe
          lathe-comforts/own-contract
          lathe-comforts/string
          lathe-comforts/struct
          lathe-comforts/syntax
          lathe-comforts/trivial)
        
        racket/base)
      
      (define syntax-doc '(lib "syntax/scribblings/syntax.scrbl"))
      
      ))
  
  result)
