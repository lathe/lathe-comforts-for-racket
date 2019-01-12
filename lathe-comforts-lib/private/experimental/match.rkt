#lang parendown racket/base

; lathe-comforts/private/experimental/match
;
; Experimental utilities for match expanders.

;   Copyright 2019 The Lathe Authors
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


(require #/for-syntax racket/base)

(require #/for-syntax #/only-in syntax/parse
  expr expr/c id syntax-parse)

(require #/for-syntax #/only-in lathe-comforts w-)

(require #/only-in racket/contract/base -> and/c any/c or/c)
(require #/only-in racket/match define-match-expander)
(require #/only-in racket/math natural?)

(require #/only-in lathe-comforts fn)

(provide define-match-expander-via-lists)


(define-syntax (define-match-expander-via-lists stx)
  (syntax-protect
  #/syntax-parse stx
  #/ (_ name:id function-version:expr inst->maybe-list)
    
    #:declare inst->maybe-list
    (expr/c #'(-> any/c natural? #/or/c #f list?)
      #:name "inst->maybe-list argument")
    
    #'(begin
        
        (define inst->maybe-list-result inst->maybe-list.c)
        
        (define-match-expander name
          (lambda (stx)
            ; TODO: We should really use a syntax class for match
            ; patterns rather than `expr` here, but it doesn't look
            ; like one exists yet.
            (syntax-protect
            #/syntax-parse stx #/ (_ arg:expr #/... ...)
            #/w- n (length #/syntax->list #'#/arg #/... ...)
              #`(app (fn v #/inst->maybe-list-result v #,n)
                #/list arg #/... ...)))
          (lambda (stx)
            (syntax-protect #/syntax-parse stx
              [_:id #'function-version]
              [ (_ arg #/... ...)
                #'(function-version arg #/... ...)]))))))
