#lang parendown/slash racket/base

; lathe-comforts/sequence
;
; Utilities for sequences.

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

(require lathe-comforts)
(require lathe-comforts/list)


(provide
  sequence*)
(provide /own-contract-out
  sequence-first
  endless-sequence/c)


(begin-for-syntax /define-syntax-class multi-value-expr
  (pattern ({~literal values} _:expr ...))
  (pattern _:expr))

(define-match-expander sequence*
  ; TODO: Use a syntax class for match patterns rather than `expr`
  ; here, if one ever exists.
  (syntax-parser / (_ elem:multi-value-expr ... rest:expr)
    #'(app sequence->stream /stream* elem ... rest))
  (syntax-parser / (_ elem:multi-value-expr ... rest)
    #:declare rest (expr/c #'sequence? #:name "rest argument")
    #'(stream* elem ... /sequence->stream rest.c)))

(define/own-contract (sequence-first s)
  (-> sequence? any)
  (define-values (elem-values next) (sequence-generate* s))
  (unless elem-values
    (raise-arguments-error 'sequence-first
      "expected a non-empty sequence"
      "s" s))
  (apply values elem-values))

(define/own-contract (endless-sequence/c . element-value/cs)
  (-> contract? contract?)
  (w- element-value/cs
    (for/list ([element-value/c (in-list element-value/cs)])
      (coerce-contract 'endless-sequence/c element-value/c))
  /w- n (length element-value/cs)
  /w- name
    `
    (endless-sequence/c
      ,@
      (for/list ([element-value/c (in-list element-value/cs)])
        (contract-name element-value/c)))
  /make-contract
    
    #:name name
    
    #:first-order
    (fn v
      (sequence? v))
    
    #:late-neg-projection
    (fn blame
      (w- element-value/c-projections
        (for/list
          (
            [i (in-naturals)]
            [element-value/c (in-list element-value/cs)])
          ( (get/build-late-neg-projection element-value/c)
            (blame-add-context blame (format "element value ~a of" i))))
      /fn v missing-party
        (expect (sequence? v) #t
          (raise-blame-error blame #:missing-party missing-party v
            '(expected: "a sequence" given: "~e")
            v)
        /w- next-pos
          (fn get-pos
            (begin (define-values (elem-values next) (get-pos))
            /mat elem-values #f
              (raise-blame-error blame #:missing-party missing-party v
                '(expected: "an endless sequence" given: "~e")
                v)
            /expect (list-length=nat? elem-values n) #t
              (raise-blame-error blame #:missing-party missing-party v
                '(expected: "a sequence where each element has ~a values" given: "~e")
                n v)
            /w- elem-values
              (for/list
                (
                  [ element-value/c-projection
                    (in-list element-value/c-projections)]
                  [elem-value (in-list elem-values)])
                (element-value/c-projection elem-value missing-party))
            /list elem-values next))
        /make-do-sequence /fn /initiate-sequence
          
          #:pos->element
          (dissectfn (list elem-values next)
            (apply values elem-values))
          
          #:early-next-pos (dissectfn (list elem-values next) next)
          #:next-pos (fn next /next-pos /fn /next)
          #:init-pos (next-pos /fn /sequence-generate* v))))))
