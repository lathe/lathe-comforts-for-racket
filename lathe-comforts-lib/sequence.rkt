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


(provide /own-contract-out
  endless-sequence/c)
(provide
  sequence*)
(provide /own-contract-out
  sequence-first
  sequence-zip*-map
  endless-sequence-zip*-map)


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
        /w- next-possible-pos
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
        /make-do-sequence /fn /lathe-initiate-sequence
          
          #:init-possible-pos
          (next-possible-pos /fn /sequence-generate* v)
          
          #:pos->element
          (dissectfn (list elem-values next)
            (apply values elem-values))
          
          #:pos->lightweight-pos
          (dissectfn (list elem-values next)
            next)
          
          #:next-possible-pos-for-lightweight-pos
          (fn next
            (next-possible-pos /fn /next)))))))

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

; TODO: See if we'll want to export this.
(define
  (lathe-initiate-sequence
    #:init-possible-pos init-possible-pos
    #:continue-with-possible-pos? [continue-with-possible-pos? #f]
    #:pos->element pos->element
    #:pos->lightweight-pos [pos->lightweight-pos (fn pos pos)]
    #:continue-with-element? [continue-with-element? #f]
    
    #:continue-after-lightweight-pos+element?
    [continue-after-lightweight-pos+element? #f]
    
    #:next-possible-pos-for-lightweight-pos
    next-possible-pos-for-lightweight-pos
    
    )
  ; TODO: After we depend on Racket 8.11 for more things, we can use
  ; `initiate-sequence` instead of `values` here.
  (values
    #;#:pos->element pos->element
    #;#:early-next-pos pos->lightweight-pos
    #;#:next-pos next-possible-pos-for-lightweight-pos
    #;#:init-pos init-possible-pos
    #;#:continue-with-pos? continue-with-possible-pos?
    #;#:continue-with-val? continue-with-element?
    #;#:continue-after-pos+element? continue-after-lightweight-pos+element?))

(define/own-contract (sequence-zip*-map sequences on-element)
  (->
    (non-empty-listof (sequence/c any/c))
    (-> (non-empty-listof any/c) any/c)
    (sequence/c any/c))
  ; NOTE: If `in-parallel` asserted that its given sequences were the
  ; same length, we could do this. Instead, `in-parallel` doesn't
  ; document what happens if its sequences are differing lengths. In
  ; practice, it stops once any one of them stops, and `(in-parallel)`
  ; with no arguments never stops, but we probably shouldn't rely on
  ; either of these facts.
;  (sequence-map
;    (lambda elements /on-element elements)
;    (apply in-parallel sequences)))
  (define (next-possible-pos entries next-entry)
    (w- result
      (list-map entries /fn entry
        (begin (define-values (elem-values next) (next-entry entry))
        /and elem-values
        /dissect elem-values (list elem)
        /list elem next))
    /if (list-all result /fn entry entry)
      result
    /if (not /list-any result /fn entry entry)
      #f
    ; TODO: See if we can add relevant information to this error message.
    /raise-arguments-error 'sequence-zip*-map
      "expected the given sequences to be of the same length"))
  (make-do-sequence /fn /lathe-initiate-sequence
    
    #:init-possible-pos
    (next-possible-pos sequences /fn s /sequence-generate* s)
    
    #:continue-with-possible-pos? (fn possible-pos possible-pos)
    
    #:pos->element
    (fn pos
      (on-element /list-map pos /dissectfn (list elem next) elem))
    
    #:pos->lightweight-pos
    (fn pos
      (list-map pos /dissectfn (list elem next) next))
    
    #:next-possible-pos-for-lightweight-pos
    (fn nexts
      (next-possible-pos nexts /fn next /next))))

(define/own-contract (endless-sequence-zip*-map sequences on-element)
  (-> (listof (endless-sequence/c any/c)) (-> list? any/c)
    (endless-sequence/c any/c))
  (mat sequences (list)
    (sequence-zip*-map (list /in-cycle /list #f) /dissectfn (list #f)
      (on-element /list))
  /sequence-zip*-map sequences on-element))
