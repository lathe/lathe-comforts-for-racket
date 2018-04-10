#lang parendown racket/base

; lathe-comforts/private
;
; Implementation details.

;   Copyright 2011, 2017-2018 The Lathe Authors
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

(require #/only-in syntax/parse/define define-simple-macro expr id)

(provide #/all-defined-out)



; ===== Evergreen utilities for binding syntax and FP ================


(module part1 racket/base
  (require #/only-in syntax/parse
    ~or* ~peek-not ~seq define-splicing-syntax-class expr id pattern)
  (require #/only-in syntax/parse/define expr define-simple-macro)
  (provide #/all-defined-out)
  
  
  ; === Binding syntax utilities, part 1 ===
  
  (define-splicing-syntax-class binds
    #:attributes ([var 1] [val 1])
    (pattern
      (~or*
        ([var:id val:expr] ...)
        [(~seq var:id val:expr) ...]
        (~seq (~seq var:id val:expr) ... (~peek-not _:id)))))
  
  (define-splicing-syntax-class bindbody
    #:attributes ([var 1] [val 1] [body 1])
    (pattern
      (~seq vars:binds body:expr ...)
      #:attr [var 1] #'(vars.var ...)
      #:attr [val 1] #'(vars.val ...)))
  
  
  ; === Functional programming utilities, part 1 ===
  
  (define-simple-macro (fn parms ... body:expr)
    (lambda (parms ...)
      body))
  
)
(require 'part1)
(require #/for-syntax 'part1)
(provide #/all-from-out 'part1)


; === Binding syntax utilities, part 2 ===

(define-simple-macro (normalize-binds [op ...] bb:bindbody)
  (op ... ([bb.var bb.val] ...)
    bb.body ...))


; === Functional programming utilities ===


; == Bindings and recursion ==

(define (pass arg func)
  (func arg))

(define-simple-macro (w- bb:bindbody)
  (normalize-binds (let) bb))

(define-simple-macro (w-loop next:id bb:bindbody)
  (normalize-binds (let next) bb))

(define-simple-macro (loopfn name:id parms ... body:expr)
  (letrec ([name (fn parms ... body)])
    name))


; == Conditionals ==

(define-simple-macro
  (mat subject:expr pattern:expr then:expr else:expr ...)
  (match subject [pattern then] [_ (void) else ...]))

(define-simple-macro (matfns pattern:expr then:expr elsefn:expr)
  (match-lambda [pattern then] [subject (elsefn subject)]))

(define-simple-macro
  (expect subject:expr pattern:expr else:expr then:expr ...)
  (match subject [pattern (void) then ...] [_ else]))

(define-simple-macro (expectfn pattern:expr else:expr then:expr ...)
  (match-lambda [pattern (void) then ...] [_ else]))

(define-simple-macro (dissect subject:expr pattern:expr then:expr ...)
  (match subject [pattern (void) then ...]))

(define-simple-macro (dissectfn pattern:expr then:expr ...)
  (match-lambda [pattern (void) then ...]))


; ===== Unit testing utilities =======================================

(define (destx x)
  (syntax->datum #/datum->syntax #'foo x))
