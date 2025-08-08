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


(module part1 racket/base
  
  (require lathe-comforts/private/shim)
  (init-shim)
  
  (provide #/all-defined-out)
  
  
  (define (scopes-empty? stx)
    (bound-identifier=?
      (datum->syntax stx 'x)
      (datum->syntax #f 'x)
      0))
  
  (define (scopes<=? a b)
    (define b-scopes-fn
      (make-syntax-delta-introducer (datum->syntax b 'x) #f 0))
    (scopes-empty? #/b-scopes-fn a 'remove))
  
  (define (autoptic-to? surrounding-stx stx)
    (scopes<=? surrounding-stx stx))
  
  (define (autoptic-list-to? surrounding-stx lst)
    (if (syntax? lst)
      (and (autoptic-to? surrounding-stx lst)
        (autoptic-list-to? surrounding-stx (syntax-e lst)))
      (match lst
        [(cons elem lst) (autoptic-list-to? surrounding-stx lst)]
        [(list) #t]
        [_ #f])))
  
  )


(module part2 racket/base
  
  ; TODO SHIM: See if we should add these to the shim. Our other
  ; modules don't depend on `syntax/parse`'s parsing framework at run
  ; time.
  (require #/only-in syntax/parse
    define-syntax-class pattern this-syntax)
  
  (require #/submod ".." part1)
  
  (require lathe-comforts/private/shim)
  (init-shim)
  
  (provide #/all-defined-out)
  
  
  ; === Utilities for macros, part 1 ===
  
  (define-syntax-class (autoptic-to surrounding-stx)
    (pattern _
      #:when
      (and (syntax? surrounding-stx)
        (autoptic-to? surrounding-stx this-syntax))))
  
  (define-syntax-class (autoptic-list-to surrounding-stx)
    (pattern _
      #:when (autoptic-list-to? surrounding-stx this-syntax)))
  
  )


(module part3 racket/base
  
  ; TODO SHIM: See if we should add these to the shim. Our other
  ; modules don't depend on `syntax/parse`'s parsing framework at run
  ; time.
  (require #/for-syntax #/only-in syntax/parse syntax-parser)
  
  (require #/for-syntax #/submod ".." part2)
  
  ; TODO SHIM: See if we should add these to the shim. Our other
  ; modules don't depend on `syntax/parse`'s parsing framework at run
  ; time.
  (require #/only-in syntax/parse
    ~and ~var expr/c pattern-expander this-syntax)
  
  (require #/submod ".." part2)
  
  (require lathe-comforts/private/shim)
  (init-shim)
  
  (provide #/all-defined-out)
  
  
  ; === Utilities for macros, part 2 ===
  
  (define-syntax ~autoptic-list-to #/pattern-expander
    (syntax-parser #/
      {~and {~var _ #/autoptic-list-to this-syntax}
        {_ surrounding-stx pattern}}
      
      #:declare surrounding-stx
      (expr/c #'syntax? #:name "surrounding-stx argument")
      
      #'{~and {~var _ #/autoptic-list-to surrounding-stx.c} pattern}))
  
  )


(module part4 racket/base
  
  ; TODO SHIM: See if we should add these to the shim. Our other
  ; modules don't depend on `syntax/parse`'s parsing framework at run
  ; time.
  (require #/for-syntax #/only-in syntax/parse syntax-parser)
  
  (require #/for-syntax #/submod ".." part3)
  
  ; TODO SHIM: See if we should add these to the shim. Our other
  ; modules don't depend on `syntax/parse`'s parsing framework at run
  ; time.
  (require #/only-in syntax/parse pattern-expander this-syntax)
  
  (require #/submod ".." part3)
  
  (require lathe-comforts/private/shim)
  (init-shim)
  
  (provide #/all-defined-out)
  
  
  ; === Utilities for macros, part 3 ===
  
  (define-syntax ~autoptic-list #/pattern-expander #/syntax-parser #/
    {~autoptic-list-to this-syntax {_ pattern}}
    #'
    {~autoptic-list-to
      (let ([stx this-syntax])
        (unless (syntax? stx)
          (raise-arguments-error '~autoptic-list
            "expected the current result of this-syntax to be a syntax object"
            "this-syntax" stx))
        stx)
      pattern})
  
  )


(module part5 racket/base
  
  ; TODO SHIM: See if we should add these to the shim. Our other
  ; modules don't depend on `syntax/parse`'s parsing framework at run
  ; time.
  (require #/for-syntax #/only-in syntax/parse syntax-parser)
  
  (require #/for-syntax #/submod ".." part2)
  (require #/for-syntax #/submod ".." part4)
  
  ; TODO SHIM: See if we should add these to the shim. Our other
  ; modules don't depend on `syntax/parse`'s parsing framework at run
  ; time.
  (require #/only-in syntax/parse ~and ~var pattern-expander)
  
  (require lathe-comforts/private/shim)
  (init-shim)
  
  (provide #/all-defined-out)
  
  
  ; === Utilities for macros, part 4 ===
  
  (define-syntax ~autoptic #/pattern-expander #/syntax-parser #/
    {~autoptic-list {_ pattern}}
    #'
    {~and
      {~var _ #/autoptic-to
        (let ([stx this-syntax])
          (unless (syntax? stx)
            (raise-arguments-error '~autoptic
              "expected the current result of this-syntax to be a syntax object"
              "this-syntax" stx))
          stx)}
      pattern})
  
  )


(module part6 racket/base
  
  (require #/for-syntax #/submod ".." part4)
  
  ; TODO SHIM: See if we should add these to the shim. Our other
  ; modules don't depend on `syntax/parse`'s parsing framework at run
  ; time.
  (require #/only-in syntax/parse
    ~or* ~peek-not ~seq define-splicing-syntax-class expr id pattern)
  
  (require #/submod ".." part3)
  (require #/submod ".." part4)
  
  (require lathe-comforts/private/shim)
  (init-shim)
  
  (provide #/all-defined-out)
  
  
  ; === Utilities for macros, part 5 ===
  
  ; NOTE: We define `define-syntax-parse-rule/autoptic/loc` so it's
  ; easy to use something like `define-syntax-parse-rule` but with
  ; `syntax/loc`. If we don't use `syntax/loc` like this, stack traces
  ; which involve functions generated by the macro will use the source
  ; location of the macro, rather than the location where the macro
  ; was used.
  
  (define-syntax (define-syntax-parse-rule/autoptic/loc stx)
    (syntax-parse stx #/
      {~autoptic-list
        (_ {~autoptic-list (name:id pat:expr ...)} pattern-directive ... template)}
      (syntax/loc stx
      #/define-syntax (name stx)
        (syntax-parse stx #:track-literals #/
          {~autoptic-list (_ pat ...)}
          pattern-directive ...
        #/syntax/loc stx
          template))))
  
  (define-syntax-parse-rule/autoptic/loc
    (define-syntax-parse-rule/autoptic
      {~autoptic-list (name:id pat:expr ...)}
      pattern-directive ...
      template)
    (define-syntax (name stx)
      (syntax-parse stx #:track-literals #/
        {~autoptic-list (_ pat ...)}
        pattern-directive ...
        #'template)))
  
  
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


(require #/for-syntax 'part4)
(require #/for-syntax 'part6)
(require 'part6)


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
