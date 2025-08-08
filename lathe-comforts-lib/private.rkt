#lang parendown racket/base

; lathe-comforts/private
;
; Implementation details.

;   Copyright 2011, 2017-2019, 2022, 2025 The Lathe Authors
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

(require #/only-in racket/match match match/derived match-lambda)

(provide
  define-syntax-parse-rule/autoptic
  define-syntax-parse-rule/autoptic/loc
  autoptic-binds-to
  fn
  define-simple-normalizing-binder)
(provide #/own-contract-out
  pass)
(provide
  w-
  w-loop
  loopfn
  mat
  expect
  matfns
  expectfn)
(provide #/for-syntax
  expand-dissect/derived)
(provide
  dissect
  dissect/derived)
(provide #/for-syntax
  expand-dissectfn/derived)
(provide
  dissectfn
  dissectfn/derived)



; ===== Evergreen utilities for binding syntax and FP ================


(module part7 racket/base
  
  (require #/for-syntax #/submod lathe-comforts/private/shim part4)
  
  ; TODO SHIM: See if we should add these to the shim. Our other
  ; modules don't depend on `syntax/parse`'s parsing framework at run
  ; time.
  (require #/only-in syntax/parse
    ~or* ~peek-not ~seq define-splicing-syntax-class expr id pattern)
  
  (require #/submod lathe-comforts/private/shim part3)
  (require #/submod lathe-comforts/private/shim part4)
  
  (require lathe-comforts/private/shim)
  (init-shim)
  
  (provide #/all-defined-out)
  
  
  ; === Utilities for binding syntax, part 1 ===
  
  (define-splicing-syntax-class (autoptic-binds-to surrounding-stx)
    #:attributes ([var 1] [val 1])
    (pattern
      (~or*
        {~autoptic-list-to
          (begin
            (unless (syntax? surrounding-stx)
              (raise-arguments-error '~autoptic-list
                "expected surrounding-stx to be a syntax object"
                "surrounding-stx" surrounding-stx))
            surrounding-stx)
          ({~autoptic-list-to surrounding-stx [var:id val:expr]} ...)}
        {~autoptic-list-to surrounding-stx
          [{~seq var:id val:expr} ...]}
        {~seq {~seq var:id val:expr} ...
          {~peek-not #/~seq _:id _:expr}})))
  
  
  ; === Utilities for functional programming, part 1 ===
  
  
  ; == Bindings and recursion, part 1 ==
  
  (define-syntax-parse-rule/autoptic/loc (fn args:id ... body:expr)
    (lambda (args ...)
      body))
  
  )


(require #/for-syntax #/submod lathe-comforts/private/shim part4)
(require #/for-syntax 'part7)
(require 'part7)


; === Utilities for binding syntax, part 2 ===

(define-syntax-parse-rule/autoptic/loc
  (define-simple-normalizing-binder
    {~autoptic-list (name:id pattern:expr ...)}
    {~autoptic-list (template:expr ...)})
  (define-syntax-parse-rule/autoptic/loc
    (name pattern ... vars body:expr #/... ...)
    #:declare vars (autoptic-binds-to this-syntax)
    (template ... ([vars.var vars.val] #/... ...)
      body
      (... ...))))


; === Utilities for functional programming, part 2 ===


; == Bindings and recursion, part 2 ==

(define/own-contract (pass arg func)
  (-> any/c (-> any/c any) any)
  (func arg))

(define-simple-normalizing-binder (w-)
  (let))

(define-simple-normalizing-binder (w-loop proc:id)
  (let proc))

(define-syntax-parse-rule/autoptic/loc
  (loopfn proc:id args:id ... body:expr)
  (letrec ([proc (fn args ... body)])
    proc))


; == Conditionals ==

(define-syntax-parse-rule/autoptic
  (mat subject:expr pattern:expr then:expr else:expr)
  (match subject [pattern then] [_ else]))

(define-syntax-parse-rule/autoptic
  (expect subject:expr pattern:expr else:expr then:expr)
  (match subject [pattern then] [_ else]))

(define-syntax-parse-rule/autoptic/loc
  (matfns pattern:expr then:expr elsefn)
  #:declare elsefn (expr/c #'(-> any/c any) #:name "elsefn argument")
  (match-lambda [pattern then] [subject (elsefn.c subject)]))

(define-syntax-parse-rule/autoptic/loc
  (expectfn pattern:expr else:expr then:expr)
  (match-lambda [pattern then] [_ else]))

(define-for-syntax (expand-dissect/derived orig-stx args)
  (syntax-protect
  #/syntax-parse args #:context orig-stx #/
    (subject:expr pattern:expr then:expr)
    #`(match/derived subject #,orig-stx [pattern then])))

(define-syntax (dissect stx)
  (syntax-parse stx #/ {~autoptic-list (_ args ...)}
  #/expand-dissect/derived stx #'(args ...)))

(define-syntax (dissect/derived stx)
  (syntax-parse stx #/ {~autoptic-list (_ orig-stx args ...)}
  #/expand-dissect/derived #'orig-stx #'(args ...)))

(define-for-syntax (expand-dissectfn/derived orig-stx args)
  (syntax-protect
  #/syntax-parse args #:context orig-stx #/
    (pattern:expr then:expr)
    (quasisyntax/loc orig-stx
      (fn subject
        (match/derived subject #,orig-stx
          [pattern then])))))

(define-syntax (dissectfn stx)
  (syntax-parse stx #/ {~autoptic-list (_ args ...)}
  #/expand-dissectfn/derived stx #'(args ...)))

(define-syntax (dissectfn/derived stx)
  (syntax-parse stx #/ {~autoptic-list (_ orig-stx args ...)}
  #/expand-dissectfn/derived #'orig-stx #'(args ...)))
