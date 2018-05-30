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

(require #/only-in racket/match match match-lambda)
(require #/only-in syntax/parse/define define-simple-macro expr id)

(provide #/all-defined-out)



; ===== Evergreen utilities for binding syntax and FP ================


(module part1 racket/base
  (require #/only-in syntax/parse
    ~or* ~peek-not ~seq define-splicing-syntax-class expr id pattern)
  (require #/only-in syntax/parse/define define-simple-macro expr id)
  (provide #/all-defined-out)
  
  
  ; === Binding syntax utilities, part 1 ===
  
  (define-splicing-syntax-class binds
    #:attributes ([var 1] [val 1])
    (pattern
      (~or*
        ([var:id val:expr] ...)
        [(~seq var:id val:expr) ...]
        (~seq (~seq var:id val:expr) ... (~peek-not _:id)))))
  
  
  ; === Functional programming utilities, part 1 ===
  
  
  ; == Bindings and recursion, part 1 ==
  
  (define-simple-macro (fn args:id ... body:expr)
    (lambda (args ...)
      body))
  
)
(require 'part1)
(require #/for-syntax 'part1)
(provide #/all-from-out 'part1)


; === Binding syntax utilities, part 2 ===

(define-simple-macro
  (define-simple-normalizing-binder (name:id pattern:expr ...)
    (template:expr ...))
  (define-simple-macro
    (name pattern ... vars:binds body:expr #/... ...)
    (template ... ([vars.var vars.val] #/... ...)
      body #/... ...)))


; === Functional programming utilities, part 2 ===


; == Bindings and recursion, part 2 ==

(define (pass arg func)
  (func arg))

(define-simple-normalizing-binder (w-)
  (let))

(define-simple-normalizing-binder (w-loop proc:id)
  (let proc))

(define-simple-macro (loopfn proc:id args:id ... body:expr)
  (letrec ([proc (fn args ... body)])
    proc))


; == Conditionals ==

(define-simple-macro
  (mat subject:expr pattern:expr then:expr else:expr)
  (match subject [pattern then] [_ else]))

(define-simple-macro
  (expect subject:expr pattern:expr else:expr then:expr)
  (match subject [pattern then] [_ else]))

(define-simple-macro (matfns pattern:expr then:expr elsefn:expr)
  (match-lambda [pattern then] [subject (elsefn subject)]))

(define-simple-macro (expectfn pattern:expr else:expr then:expr)
  (match-lambda [pattern then] [_ else]))

(define-simple-macro (dissect subject:expr pattern:expr then:expr)
  (match subject [pattern then]))

(define-simple-macro (dissectfn pattern:expr then:expr)
  (match-lambda [pattern then]))