#lang parendown/slash racket/base

; shim.rkt
;
; Import lists, debugging constants, other utilities that are useful
; primarily for this codebase, preliminary utilities for defining
; these things, and various utilities that could come in handy in
; other codebases for making shim files like this one.

;   Copyright 2021-2022, 2025 The Lathe Authors
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

(reprovide lathe-comforts/private/codebasewide-requires)

; TODO: Consider exporting many of these from public modules.
(provide
  scopes-empty?
  scopes<=?
  autoptic-to?
  autoptic-list*-to?
  autoptic-list-to?
  define-syntax-parse-rule/autoptic/loc
  define-syntax-parse-rule/autoptic
  (for-syntax
    eval-for-so-to-speak)
  so-to-speak
  ifc
  whenc
  unlessc
  condc
  (for-syntax
    autoptic-pattern-directive-to)
  define-provide-pre-transformer-syntax-parse-rule/autoptic
  so-to-speak-out
  ifc-out
  contract-ignored-out
  contract-whenc-out
  contract-unlessc-out
  recontract-whenc-out
  recontract-unlessc-out
  (for-syntax
    module-path)
;  define-requirer
;  define-requirer-in
  (for-syntax
    own-contract-scope
    own-contract-policy-scope
    make-own-contract-policy-id
    make-signature-contract-id)
  define-own-contract-policies
  (for-syntax
    own-contracted-id)
  own-contract-out
  own-contract-ignored-out
  own-contract-whenc-out
  own-contract-unlessc-out
  ascribe-own-contract
  (for-syntax
    lambda-param
    lambda-params)
  define/own-contract
  (for-syntax
    suppressing-external-contracts?
    activating-internal-contracts?)
  init-shim)



(module part1 racket/base
  
  (require lathe-comforts/private/codebasewide-requires)
  
  (provide /all-defined-out)
  
  
  (define (scopes-empty? stx)
    (bound-identifier=?
      (datum->syntax stx 'x)
      (datum->syntax #f 'x)
      0))
  
  (define (scopes<=? a b)
    (define b-scopes-fn
      (make-syntax-delta-introducer (datum->syntax b 'x) #f 0))
    (scopes-empty? /b-scopes-fn a 'remove))
  
  (define (autoptic-to? surrounding-stx stx)
    (scopes<=? surrounding-stx stx))
  
  (define (autoptic-list*-to? surrounding-stx lst)
    (if (syntax? lst)
      (let ([lst-e (syntax-e lst)])
        (or (not /or (pair? lst-e) (null? lst-e))
          (and (autoptic-to? surrounding-stx lst)
            (autoptic-list*-to? surrounding-stx lst-e))))
      (match lst
        [(cons elem lst) (autoptic-list*-to? surrounding-stx lst)]
        [_ #t])))
  
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
  (require /only-in syntax/parse
    define-syntax-class pattern this-syntax)
  
  (require lathe-comforts/private/codebasewide-requires)
  
  (require /submod ".." part1)
  
  (provide /all-defined-out)
  
  
  (define-syntax-class (autoptic-to surrounding-stx)
    (pattern _
      #:when
      (and (syntax? surrounding-stx)
        (autoptic-to? surrounding-stx this-syntax))))
  
  (define-syntax-class (autoptic-list*-to surrounding-stx)
    #:attributes (smuggle)
    (pattern _
      #:when (autoptic-list*-to? surrounding-stx this-syntax)
      #:attr smuggle
      (lambda (lctx elements rest)
        (let next ([stx this-syntax] [elems elements])
          (if (syntax? stx)
            (datum->syntax
              #;lctx lctx
              (next (syntax-e stx) elems)
              #;srcloc stx
              #;prop stx)
          /match stx
            [ (cons original-elem stx)
              (match elems
                [(cons elem elems) (cons elem /next stx elems)]
                [ (list)
                  (raise-arguments-error 'autoptic-list*-to
                    "not enough replacement elements supplied to the smuggle procedure"
                    "elements" elements
                    "original syntax" this-syntax)]
                [ _
                  (raise-arguments-error 'autoptic-list*-to
                    "expected a proper list of replacement elements"
                    "elements" elements)])]
            [ _
              (match elems
                [(list) rest]
                [ (cons elem elems)
                  (raise-arguments-error 'autoptic-list*-to
                    "too many replacement elements supplied to the smuggle procedure"
                    "elements" elements
                    "original syntax" this-syntax)]
                [_
                  (raise-arguments-error 'autoptic-list*-to
                    "expected a proper list of replacement elements"
                    "elements" elements)])])))))
  
  (define-syntax-class (autoptic-list-to surrounding-stx)
    #:attributes (smuggle)
    (pattern _
      #:when (autoptic-list-to? surrounding-stx this-syntax)
      #:attr smuggle
      (lambda (lctx elements)
        (let next ([stx this-syntax] [elems elements])
          (if (syntax? stx)
            (datum->syntax
              #;lctx lctx
              (next (syntax-e stx) elems)
              #;srcloc stx
              #;prop stx)
          /match stx
            [ (cons original-elem stx)
              (match elems
                [(cons elem elems) (cons elem /next stx elems)]
                [ (list)
                  (raise-arguments-error 'autoptic-list-to
                    "not enough replacement elements supplied to the smuggle procedure"
                    "elements" elements
                    "original syntax" this-syntax)]
                [ _
                  (raise-arguments-error 'autoptic-list-to
                    "expected a proper list of replacement elements"
                    "elements" elements)])]
            [ (list)
              (match elems
                [(list) (list)]
                [ (cons elem elems)
                  (raise-arguments-error 'autoptic-list-to
                    "too many replacement elements supplied to the smuggle procedure"
                    "elements" elements
                    "original syntax" this-syntax)]
                [ _
                  (raise-arguments-error 'autoptic-list-to
                    "expected a proper list of replacement elements"
                    "elements" elements)])])))))
  
  )


(module part3 racket/base
  
  ; TODO SHIM: See if we should add these to the shim. Our other
  ; modules don't depend on `syntax/parse`'s parsing framework at run
  ; time.
  (require /for-syntax /only-in syntax/parse syntax-parser)
  
  (require /for-syntax /submod ".." part2)
  
  ; TODO SHIM: See if we should add these to the shim. Our other
  ; modules don't depend on `syntax/parse`'s parsing framework at run
  ; time.
  (require /only-in syntax/parse
    ~and ~bind ~var expr/c pattern-expander this-syntax)
  
  (require lathe-comforts/private/codebasewide-requires)
  
  (require /submod ".." part2)
  
  (provide /all-defined-out)
  
  
  (define-syntax ~autoptic-to /pattern-expander /syntax-parser /
    {~and {~var _ /autoptic-list-to this-syntax}
      {_ surrounding-stx pattern}}
    
    #:declare surrounding-stx
    (expr/c #'syntax? #:name "surrounding-stx argument")
    
    #'{~and {~var _ /autoptic-to surrounding-stx.c} pattern})
  
  (define-syntax ~autoptic-list*-to /pattern-expander /syntax-parser
    [
      {~and {~var _ /autoptic-list-to this-syntax}
        {_ surrounding-stx pattern}}
      
      #:declare surrounding-stx
      (expr/c #'syntax? #:name "surrounding-stx argument")
      
      #'{~and {~var _ /autoptic-list*-to surrounding-stx.c} pattern}]
    [
      {~and {~var _ /autoptic-list-to this-syntax}
        {_ surrounding-stx
          {~and #:smuggle /~var _ /autoptic-to this-syntax}
          smuggle:id
          pattern}}
      
      #:declare surrounding-stx
      (expr/c #'syntax? #:name "surrounding-stx argument")
      
      #'
      {~and {~var lst /autoptic-list*-to surrounding-stx.c}
        {~bind / smuggle /datum lst.smuggle}
        pattern}])
  
  (define-syntax ~autoptic-list-to /pattern-expander /syntax-parser
    [
      {~and {~var _ /autoptic-list-to this-syntax}
        {_ surrounding-stx pattern}}
      
      #:declare surrounding-stx
      (expr/c #'syntax? #:name "surrounding-stx argument")
      
      #'{~and {~var _ /autoptic-list-to surrounding-stx.c} pattern}]
    [
      {~and {~var _ /autoptic-list-to this-syntax}
        {_ surrounding-stx
          {~and #:smuggle /~var _ /autoptic-to this-syntax}
          smuggle:id
          pattern}}
      
      #:declare surrounding-stx
      (expr/c #'syntax? #:name "surrounding-stx argument")
      
      #'
      {~and {~var lst /autoptic-list-to surrounding-stx.c}
        {~bind / smuggle /datum lst.smuggle}
        pattern}])
  
  )


(module part4 racket/base
  
  ; TODO SHIM: See if we should add these to the shim. Our other
  ; modules don't depend on `syntax/parse`'s parsing framework at run
  ; time.
  (require /for-syntax /only-in syntax/parse syntax-parser)
  
  (require /for-syntax /submod ".." part2)
  (require /for-syntax /submod ".." part3)
  
  ; TODO SHIM: See if we should add these to the shim. Our other
  ; modules don't depend on `syntax/parse`'s parsing framework at run
  ; time.
  (require /only-in syntax/parse pattern-expander this-syntax)
  
  (require lathe-comforts/private/codebasewide-requires)
  
  (require /submod ".." part3)
  
  (provide /all-defined-out)
  
  
  (define-syntax ~autoptic-list* /pattern-expander /syntax-parser
    [ {~autoptic-list-to this-syntax {_ pattern}}
      #'
      {~autoptic-list*-to
        (let ([stx this-syntax])
          (unless (syntax? stx)
            (raise-arguments-error '~autoptic-list*
              "expected the current result of this-syntax to be a syntax object"
              "this-syntax" stx))
          stx)
        pattern}]
    [
      {~autoptic-list-to this-syntax
        {_
          {~and #:smuggle /~var _ /autoptic-to this-syntax}
          smuggle:id
          pattern}}
      #'
      {~autoptic-list*-to
        (let ([stx this-syntax])
          (unless (syntax? stx)
            (raise-arguments-error '~autoptic-list*
              "expected the current result of this-syntax to be a syntax object"
              "this-syntax" stx))
          stx)
        #:smuggle smuggle
        pattern}]
    )
  
  (define-syntax ~autoptic-list /pattern-expander /syntax-parser
    [ {~autoptic-list-to this-syntax {_ pattern}}
      #'
      {~autoptic-list-to
        (let ([stx this-syntax])
          (unless (syntax? stx)
            (raise-arguments-error '~autoptic-list
              "expected the current result of this-syntax to be a syntax object"
              "this-syntax" stx))
          stx)
        pattern}]
    [
      {~autoptic-list-to this-syntax
        {_
          {~and #:smuggle /~var _ /autoptic-to this-syntax}
          smuggle:id
          pattern}}
      #'
      {~autoptic-list-to
        (let ([stx this-syntax])
          (unless (syntax? stx)
            (raise-arguments-error '~autoptic-list
              "expected the current result of this-syntax to be a syntax object"
              "this-syntax" stx))
          stx)
        #:smuggle smuggle
        pattern}]
    )
  
  )


(module part5 racket/base
  
  ; TODO SHIM: See if we should add these to the shim. Our other
  ; modules don't depend on `syntax/parse`'s parsing framework at run
  ; time.
  (require /for-syntax /only-in syntax/parse syntax-parser)
  
  (require /for-syntax /submod ".." part2)
  (require /for-syntax /submod ".." part4)
  
  ; TODO SHIM: See if we should add these to the shim. Our other
  ; modules don't depend on `syntax/parse`'s parsing framework at run
  ; time.
  (require /only-in syntax/parse ~and ~var pattern-expander)
  
  (require lathe-comforts/private/codebasewide-requires)
  
  (require /submod ".." part2)
  
  (provide /all-defined-out)
  
  
  (define-syntax ~autoptic /pattern-expander /syntax-parser /
    {~autoptic-list {_ pattern}}
    #'
    {~and
      {~var _ /autoptic-to
        (let ([stx this-syntax])
          (unless (syntax? stx)
            (raise-arguments-error '~autoptic
              "expected the current result of this-syntax to be a syntax object"
              "this-syntax" stx))
          stx)}
      pattern})
  
  )


(module part6 racket/base
  
  (require /for-syntax /submod ".." part4)
  
  ; TODO SHIM: See if we should add these to the shim. Our other
  ; modules don't depend on `syntax/parse`'s parsing framework at run
  ; time.
  (require /only-in syntax/parse
    ~and ~bind ~or* ~seq ~var define-splicing-syntax-class expr expr/c id nat pattern)
  
  (require /submod ".." part3)
  
  (require lathe-comforts/private/codebasewide-requires)
  
  (provide /all-defined-out)
  
  
  (define (contextualize-stx lctx-stx basis-stx)
    (datum->syntax
      #;lctx lctx-stx
      (syntax-e basis-stx)
      #;srcloc basis-stx
      #;prop basis-stx))
  
  (define-splicing-syntax-class
    (autoptic-pattern-directive-to surrounding-stx #:phase phase)
    #:attributes (smuggle-parts)
    (pattern
      {~or*
        {~and
          {~or*
            {~seq {~autoptic-to surrounding-stx #:post} _:expr}
            {~seq {~autoptic-to surrounding-stx #:and} _:expr}
            {~seq {~autoptic-to surrounding-stx #:with} _ _:expr}
            {~seq {~autoptic-to surrounding-stx #:when} _:expr}
            {~seq {~autoptic-to surrounding-stx #:cut}}}
          {~seq kw args ...}
          {~bind / smuggle-parts /lambda (lctx)
            `
            (,(contextualize-stx lctx #'kw)
              ,@(syntax->list #'(args ...)))}}
        {~and
          {~seq
            {~and kw
              {~autoptic-to surrounding-stx
                (~or* #:fail-when #:fail-unless)}}
            condition:expr
            {~var message
              (expr/c #'(or/c string? #f)
                #:phase phase
                #:name "the message string or #f")}}
          {~bind / smuggle-parts /lambda (lctx)
            `
            (,(contextualize-stx lctx #'kw)
              ,#'condition
              ,#'message.c)}}
        {~and
          {~seq
            {~and kw /~autoptic-to surrounding-stx #:declare}
            var:id
            {~or*
              {~and stxclass:id
                {~bind / smuggle-stxclass /lambda (lctx) #'stxclass}}
              {~and
                {~autoptic-list-to surrounding-stx
                  #:smuggle smuggle-stxclass-call
                  (stxclass:id arg ...)}
                {~bind / smuggle-stxclass /lambda (lctx)
                  ((datum smuggle-stxclass-call) lctx
                    `(,#'stxclass ,@(syntax->list #'(arg ...))))}}}
            {~or*
              {~seq {~bind / smuggle-role /lambda (lctx) `()}}
              {~and
                {~seq
                  {~and role-kw /~autoptic-to surrounding-stx #:role}
                  {~var role-expr
                    (expr/c #'(or/c string? #f)
                      #:phase phase
                      #:name "the role string or #f")}}
                {~bind / smuggle-role /lambda (lctx)
                  `
                  (,(contextualize-stx lctx #'role-kw)
                    ,#'role-expr.c)}}}}
          {~bind / smuggle-parts /lambda (lctx)
            `
            (,(contextualize-stx lctx #'kw)
              ,#'var
              ,((datum smuggle-stxclass) lctx)
              ,@((datum smuggle-role) lctx))}}
        {~and
          {~seq {~and kw /~autoptic-to surrounding-stx #:attr}
            {~or*
              {~and var:id
                {~bind / smuggle-var /lambda (lctx) #'var}}
              {~and
                {~autoptic-list-to surrounding-stx
                  #:smuggle smuggle-var-list
                  [var:id {~autoptic-to surrounding-stx depth:nat}]}
                {~bind / smuggle-var /lambda (lctx)
                  ((datum smuggle-var-list) lctx
                    `(,#'var ,(contextualize-stx lctx #'depth)))}}}
            val:expr}
          {~bind / smuggle-parts /lambda (lctx)
            `
            (,(contextualize-stx lctx #'kw)
              ,((datum smuggle-var) lctx)
              ,#'val)}}
        {~and
          {~seq
            {~and kw /~autoptic-to surrounding-stx /~or* #:do #:undo}
            {~autoptic-list-to surrounding-stx #:smuggle smuggle-body
              [body:expr ...]}}
          {~bind / smuggle-parts /lambda (lctx)
            `
            (,(contextualize-stx lctx #'kw)
              ,
              ( (datum smuggle-body) lctx
                (syntax->list #'(body ...))))}}}))
  
  )


(module part7 racket/base
  
  (require /for-syntax /submod ".." part4)
  (require /for-syntax /submod ".." part6)
  
  ; TODO SHIM: See if we should add these to the shim. Our other
  ; modules don't depend on `syntax/parse`'s parsing framework at run
  ; time.
  (require /only-in syntax/parse expr id)
  
  (require /submod ".." part3)
  (require /submod ".." part4)
  
  (require lathe-comforts/private/codebasewide-requires)
  
  (provide /all-defined-out)
  
  
  ; NOTE: We define `define-syntax-parse-rule/autoptic/loc` so it's
  ; easy to use something like `define-syntax-parse-rule` but with
  ; `syntax/loc`. If we don't use `syntax/loc` like this, stack traces
  ; which involve functions generated by the macro will use the source
  ; location of the macro, rather than the location where the macro
  ; was used.
  
  (define-syntax (define-syntax-parse-rule/autoptic/loc stx)
    (syntax-parse stx /
      {~autoptic-list
        (_ {~autoptic-list (name:id pat:expr ...)}
          {~and
            {~var directive
              (autoptic-pattern-directive-to this-syntax
                #:phase (add1 /syntax-local-phase-level))}
            {~bind / [directive-parts 1]
              ((datum directive.smuggle-parts) this-syntax)}}
          ...
          template)}
      (syntax/loc stx
      /define-syntax (name stx)
        (syntax-parse stx #:track-literals /
          {~autoptic-list (_ pat ...)}
          {~@ directive-parts ...}
          ...
        /syntax/loc stx
          template))))
  
  (define-syntax-parse-rule/autoptic/loc
    (define-syntax-parse-rule/autoptic
      {~autoptic-list (name:id pat:expr ...)}
      {~and
        {~var directive
          (autoptic-pattern-directive-to this-syntax
            #:phase (add1 /syntax-local-phase-level))}
        {~bind / [directive-parts 1]
          ((datum directive.smuggle-parts) this-syntax)}}
      ...
      template)
    (define-syntax (name stx)
      (syntax-parse stx #:track-literals /
        {~autoptic-list (_ pat ...)}
        {~@ directive-parts ...}
        ...
        #'template)))
  
  )


(require /for-syntax 'part3)
(require /for-syntax 'part4)
(require /for-syntax 'part5)
(require /for-syntax 'part6)

(require 'part1)
(require 'part7)


(define-for-syntax (eval-for-so-to-speak this-syntax body)
  (with-syntax ([(body ...) body])
    (define result (syntax-local-eval #'(let () body ...)))
    (unless (syntax? result)
      
      ; TODO: See if there's a good way to embed the non-syntax result
      ; value into the error message, perhaps using a combination of
      ;`raise-syntax-error`'s `message-suffix` argument and
      ; `error-value->string-handler`. Or perhaps `body` should be
      ; subjected to `expr/c` with `#:phase 1`.
      ;
      (raise-syntax-error #f
        (~a
          "does not evaluate to a syntax object at phase "
          (add1 /syntax-local-phase-level))
        this-syntax
        (last /syntax->list #'/body ...)))
    result))

(define-syntax-parse-rule/autoptic (so-to-speak body:expr ...+)
  #:with result (eval-for-so-to-speak this-syntax #'(body ...))
  result)

(define-for-syntax (eval-for-condc this-syntax body)
  (define result (syntax-local-eval body))
  (unless (boolean? result)
    
    ; TODO: See if there's a good way to embed the non-boolean result
    ; value into the error message, perhaps using a combination of
    ; `raise-syntax-error`'s `message-suffix` argument and
    ; `error-value->string-handler`. Or perhaps `body` should be
    ; subjected to `expr/c` with `#:phase 1`.
    ;
    (raise-syntax-error #f
      (~a
        "does not evaluate to a boolean at phase "
        (add1 /syntax-local-phase-level))
      this-syntax
      body))
  result)

(define-syntax-parser condc /
  {~autoptic-list
    (_
      {~and {~seq clause ...}
        {~seq
          {~autoptic-list
            [
              {~and next-phase-condition:expr
                {~not /~or* {~literal else} {~literal =>}}}
              {~and branch:expr
                {~not /~or* {~literal else} {~literal =>}}}
              ...]}
          ...
          {~optional
            {~and {~bind [has-else-clause? #t]}
              {~autoptic-list
                [ {~literal else}
                  {~and else-branch:expr
                    {~not /~or* {~literal else} {~literal =>}}}
                  ...]}}
            #:defaults
            ([has-else-clause? #f] [(else-branch 1) (list)])}}})}
  (when (equal-always? 'expression (syntax-local-context))
    (unless (attribute has-else-clause?)
      (raise-syntax-error #f
        "must have an else branch when in an expression context"
        this-syntax)))
    (for
      ( [clause (in-list /syntax->list #'/clause ...)]
        #:when (syntax-parse clause [[_] #t] [_ #f]))
      (raise-syntax-error #f
        "not permitted to have empty branches when in an expression context"
        this-syntax
        #f
        (list clause)))
  (for/first
    (
      [ entry
        (in-list /syntax->list
          #'(
              [next-phase-condition (begin branch ...)]
              ...
              [#t (begin else-branch ...)]))]
      #:do
      [(match-define (list condition branch) (syntax->list entry))]
      #:when (eval-for-condc this-syntax #`(let () #,condition)))
    branch))

(define-syntax-parse-rule/autoptic
  (ifc next-phase-condition:expr then:expr els:expr)
  (condc [next-phase-condition then] [else els]))
  ; TODO: Figure out why we can't use one of these implementations
  ; (and make a corresponding simplification to `condc`).
  ; Specifically, when `then` or `else` contains definitions, these
  ; don't allow the definitions to be visible outside the form.
;  (so-to-speak /if next-phase-condition
;    #'then
;    #'else))
;  (so-to-speak /if next-phase-condition
;    (quote-syntax then #:local)
;    (quote-syntax else #:local)))

(define-syntax-parser whenc /
  {~autoptic-list (_ next-phase-condition:expr body:expr ...+)}
  (when (equal-always? 'expression (syntax-local-context))
    (raise-syntax-error #f
      "not permitted in an expression context"
      this-syntax))
  #'(condc [next-phase-condition body ...]))

(define-syntax-parser unlessc /
  {~autoptic-list (_ next-phase-condition:expr body:expr ...+)}
  (when (equal-always? 'expression (syntax-local-context))
    (raise-syntax-error #f
      "not permitted in an expression context"
      this-syntax))
  #'(whenc (not next-phase-condition) body ...))

(define-syntax-parse-rule/autoptic/loc
  (define-provide-pre-transformer-syntax-parse-rule/autoptic
    {~autoptic-list (name:id pattern ...)}
    {~and
      {~var directive
        (autoptic-pattern-directive-to this-syntax
          #:phase (add1 /syntax-local-phase-level))}
      {~bind / [directive-parts 1]
        ((datum directive.smuggle-parts) this-syntax)}}
    ...
    template)
  (define-syntax name
    (make-provide-pre-transformer /lambda (stx modes)
      (syntax-parse stx #:track-literals /
        {~autoptic-list (_ pattern ...)}
        {~@ directive-parts ...}
        ...
        (pre-expand-export #'template modes)))))

; TODO: See if we'll use this.
(define-provide-pre-transformer-syntax-parse-rule/autoptic
  (so-to-speak-out body:expr ...+)
  #:with result (eval-for-so-to-speak this-syntax #'(body ...))
  result)

(define-provide-pre-transformer-syntax-parse-rule/autoptic
  (ifc-out next-phase-condition:expr then-out else-out arg ...)
  
  #:with result
  (if (eval-for-condc this-syntax #'(let () next-phase-condition))
    #'(then-out arg ...)
    #'(else-out arg ...))
  
  result)

(define-provide-pre-transformer-syntax-parse-rule/autoptic
  (contract-ignored-out
    {~autoptic-list
      [ {~and var:id /~not /~or* {~literal struct} {~literal rename}}
        val/c]}
    ...)
  
  #:declare val/c
  (expr/c #'contract? #:name "one of the contracts to ignore")
  
  ; We evaluate the contracted expressions just to ensure that we
  ; don't ignore any syntax, scoping, or run time errors they have.
  ;
  (contract-out [var (begin val/c.c any/c)] ...))

(define-provide-pre-transformer-syntax-parse-rule/autoptic
  (contract-whenc-out next-phase-condition:expr
    {~autoptic-list
      [ {~and var:id /~not /~or* {~literal struct} {~literal rename}}
        val/c]}
    ...)
  
  #:declare val/c
  (expr/c #'contract?
    #:name "one of the contracts to apply to the exports")
  
  (ifc-out next-phase-condition contract-out contract-ignored-out
    [var val/c.c]
    ...))

(define-provide-pre-transformer-syntax-parse-rule/autoptic
  (contract-unlessc-out next-phase-condition:expr
    {~autoptic-list
      [ {~and var:id /~not /~or* {~literal struct} {~literal rename}}
        val/c]}
    ...)
  
  #:declare val/c
  (expr/c #'contract?
    #:name "one of the contracts to apply to the exports")
  
  (contract-whenc-out (not next-phase-condition)
    [var val/c.c]
    ...))

(define-provide-pre-transformer-syntax-parse-rule/autoptic
  (recontract-whenc-out next-phase-condition:expr var:id ...)
  (ifc-out next-phase-condition recontract-out combine-out var ...))

(define-provide-pre-transformer-syntax-parse-rule/autoptic
  (recontract-unlessc-out next-phase-condition:expr var:id ...)
  (recontract-whenc-out (not next-phase-condition) var ...))

(begin-for-syntax /define-syntax-class module-path
  (pattern path:expr
    
    ; TODO: See if we can verify `path` is a module path. We don't
    ; really want to check it like this, because the module path DSL
    ; is expression-like enough that we'd rather respect its
    ; self-determination of semantics rather than code-walking it.
    ; When we respect Racket expressions this way, we can still check
    ; that they're of the expected form by wrapping them in a larger
    ; expression that constrains their behavior, but module paths
    ; don't seem to have a way to do that.
    ;
    ; Note that if for some reason we do decide to use this
    ; `syntax->datum` approach, we'll want to use a version of
    ; `syntax->datum` that only strips syntax objects if they're
    ; autoptic to some surrounding syntax.
    ;
;    #:when (module-path? /syntax->datum #'path)
    
    ))

; TODO: See if this works, and see if we'll use it. For now, we're
; achieving the same purposes using `#lang reprovide` in
; `lathe-comforts/private/codebasewide-requires`.
#;
(define-syntax-parser define-requirer /
  {~autoptic-list
    (_ name:id
      {~and clause
        {~or*
          {~autoptic-list
            ({~literal only-in} _:module-path _:id ...)}
          _:module-path}}
      ...)}
  (quasisyntax/loc this-syntax
    (define-syntax-parser name / {~autoptic-list (_)}
      #`
      (require
        #,@
        (list
          #,@
          (for/list ([clause (in-list /syntax->list #'/clause ...)])
            (syntax-parse clause
              [ [path var:id ...]
                #'#`
                (only-in #,(syntax-local-introduce 'path) var ...)]
              [path #'(syntax-local-introduce 'path)])))))))

; TODO: See if this works, and see if we'll use it. For now, we're
; achieving the same purposes using `#lang reprovide` in
; `lathe-comforts/private/codebasewide-requires`.
#;
(define-syntax-parser define-requirer-in /
  {~autoptic-list
    (_ name:id
      {~and clause
        {~or*
          {~autoptic-list
            ({~literal only-in} _:module-path _:id ...)}
          _:module-path}}
      ...)}
  #`
  (define-syntax name /make-require-transformer
    #,
    (quasisyntax-loc this-syntax
      (syntax-parser / {~autoptic-list (_)}
        (expand-import
          #`
          (combine-in
            #,@
            (list
              #,@
              (for/list
                ([clause (in-list /syntax->list #'/clause ...)])
                (syntax-parse clause
                  [ [path var:id ...]
                    #'#`
                    (only-in #,(syntax-local-introduce 'path)
                      var ...)]
                  [path #'(syntax-local-introduce 'path)])))))))))


(define-for-syntax own-contract-scope (make-syntax-introducer))
(define-for-syntax own-contract-policy-scope (make-syntax-introducer))
(define-for-syntax (make-own-contract-policy-id stx name)
  (own-contract-policy-scope /datum->syntax stx name))

(define-for-syntax
  (make-signature-contract-id antecedent-land orig on-unbound)
  (
    (or
      (syntax-local-value
        (make-own-contract-policy-id antecedent-land
          '#%own-contract-policy-make-signature-contract-id)
        (lambda () /on-unbound))
      
      ; NOTE: By default, we just add a scope to the identifier. This
      ; works within a single module, but the identifier's bindings
      ; won't be visible to `module*` or `module+` submodules. To
      ; achieve that visibility, the name needs to be interned, with a
      ; user-supplied naming convention to set it apart from other
      ; variables in that context. This is what the
      ; `#:make-signature-contract-id` policy is for.
      ;
      (lambda (orig) /own-contract-scope orig))
    orig))

(define-syntax-parse-rule/autoptic
  (define-own-contract-policies
    {~alt
      {~optional {~seq #:antecedent-land antecedent-land}
        #:defaults
        ([antecedent-land (datum->syntax this-syntax '())])}
      {~optional
        {~seq #:make-signature-contract-id make-signature-contract-id}
        #:defaults ([make-signature-contract-id.c #'#f])}
      {~optional
        {~seq #:suppressing-external-contracts?
          suppressing-external-contracts?}
        #:defaults ([suppressing-external-contracts?.c #'#f])}
      {~optional
        {~seq #:activating-internal-contracts?
          activating-internal-contracts?}
        #:defaults ([activating-internal-contracts?.c #'#f])}}
    ...)
  
  #:declare make-signature-contract-id
  (expr/c #'(or/c #f /-> identifier? identifier?)
    #:phase (add1 /syntax-local-phase-level)
    #:name "the signature contract identifier naming policy")
  
  #:declare suppressing-external-contracts?
  (expr/c #'boolean? #:phase (add1 /syntax-local-phase-level)
    #:name "the external contract suppression policy")
  
  #:declare activating-internal-contracts?
  (expr/c #'boolean? #:phase (add1 /syntax-local-phase-level)
    #:name "the internal contract activation policy")
  
  #:with result
  #`(begin
      (define-syntax
        #,(make-own-contract-policy-id #'antecedent-land
            '#%own-contract-policy-make-signature-contract-id)
        make-signature-contract-id.c)
      (define-syntax
        #,(make-own-contract-policy-id #'antecedent-land
            '#%own-contract-policy-suppressing-external-contracts?)
        suppressing-external-contracts?.c)
      (define-syntax
        #,(make-own-contract-policy-id #'antecedent-land
            '#%own-contract-policy-activating-internal-contracts?)
        activating-internal-contracts?.c))
  
  result)

(begin-for-syntax /define-syntax-class
  (own-contracted-id who antecedent-land)
  #:attributes (val/c)
  (pattern
    {~and var:id /~not /~or* {~literal struct} {~literal rename}}
    
    #:with val/c-unguarded
    (make-signature-contract-id
      (syntax-local-introduce antecedent-land)
      #'var
      (lambda ()
        (raise-syntax-error #f
          "expected a define-own-contract-policies definition before using the policies"
          this-syntax)))
    
    #:declare val/c-unguarded
    (expr/c #'(promise/c contract?)
      #:name "the ascribe-own-contract value of a variable")
    
    #:attr val/c
    #`(begin
        (when (eq? var val/c-unguarded)
          (raise-arguments-error '#,who
            (format "no ascribe-own-contract information for ~a" 'var)))
        (force val/c-unguarded.c))))

(define-provide-pre-transformer-syntax-parse-rule/autoptic
  (own-contract-ignored-out
    {~optional {~seq #:antecedent-land antecedent-land}
      #:defaults ([antecedent-land (datum->syntax this-syntax '())])}
    var ...)
  
  #:declare var
  (own-contracted-id 'own-contract-ignored-out #'antecedent-land)
  
  (contract-ignored-out [var var.val/c] ...))

(define-provide-pre-transformer-syntax-parse-rule/autoptic
  (own-contract-out
    {~optional {~seq #:antecedent-land antecedent-land}
      #:defaults ([antecedent-land (datum->syntax this-syntax '())])}
    var ...)
  
  #:declare var
  (own-contracted-id 'own-contract-out #'antecedent-land)
  
  #:with result
  #`(
    #,(if
        (syntax-local-value
          (make-own-contract-policy-id
            (syntax-local-introduce #'antecedent-land)
            '#%own-contract-policy-suppressing-external-contracts?)
          (lambda ()
            (raise-syntax-error #f
              "expected a define-own-contract-policies definition before using the policies"
              this-syntax)))
        #'contract-ignored-out
        #'contract-out)
    [var var.val/c]
    ...)
  
  result)

(define-provide-pre-transformer-syntax-parse-rule/autoptic
  (own-contract-whenc-out next-phase-condition:expr
    {~optional {~seq #:antecedent-land antecedent-land}
      #:defaults ([antecedent-land (datum->syntax this-syntax '())])}
    var ...)
  
  #:declare var
  (own-contracted-id 'own-contract-whenc-out #'antecedent-land)
  
  #:with result
  #`(
    #,(let
        (
          [condition-holds (syntax-local-eval #'next-condition)]
          [ suppressing
            (syntax-local-value
              (make-own-contract-policy-id
                (syntax-local-introduce #'antecedent-land)
                '#%own-contract-policy-suppressing-external-contracts?)
              (lambda ()
                (raise-syntax-error #f
                  "expected a define-own-contract-policies definition before using the policies"
                  this-syntax)))])
        (if (and condition-holds (not suppressing))
          #'contract-ignored-out
          #'contract-out))
    [var var.val/c]
    ...)
  
  result)

(define-provide-pre-transformer-syntax-parse-rule/autoptic
  (own-contract-unlessc-out next-phase-condition:expr
    {~optional {~seq #:antecedent-land antecedent-land}
      #:defaults ([antecedent-land (datum->syntax this-syntax '())])}
    var ...)
  
  #:declare var
  (own-contracted-id 'own-contract-unlessc-out #'antecedent-land)
  
  (contract-whenc-out (not next-phase-condition)
    [var var.val/c]
    ...))

(define-syntax-parse-rule/autoptic
  (ascribe-own-contract var:id val/c
    {~optional {~seq #:antecedent-land antecedent-land}
      #:defaults ([antecedent-land (datum->syntax this-syntax '())])})
  
  #:declare val/c
  (expr/c #'contract?
    #:name "the variable's ascribed contract")
  
  #:with own-contract-var
  (make-signature-contract-id #'antecedent-land #'var /lambda ()
    (raise-syntax-error #f
      "expected a define-own-contract-policies definition before using the policies"
      this-syntax))
  
  #:do
  [
    (when (equal-always? 'expression (syntax-local-context))
      (raise-syntax-error #f
        "not allowed in an expression context"
        this-syntax))]
  
  (define own-contract-var (delay val/c.c)))

(begin-for-syntax /define-splicing-syntax-class
  (lambda-param surrounding-stx)
  #:attributes (kw var default smuggle)
  (pattern
    {~seq
      {~or*
        {~and {~seq} /~bind / smuggle-kw /lambda (lctx) '()}
        {~and {~autoptic-to surrounding-stx kw:keyword}
          {~bind / smuggle-kw /lambda (lctx)
            (list /contextualize-stx lctx #'kw)}}}
      {~or*
        {~and var:id /~bind / smuggle-var /lambda (lctx) #'var}
        {~and
          {~autoptic-list-to surrounding-stx
            #:smuggle smuggle-var-and-default
            [var:id default:expr]}
          {~bind / smuggle-var /lambda (lctx)
            ( (datum smuggle-var-and-default) lctx
              `[,#'var ,#'default])}}}}
    
    #:attr smuggle
    (lambda (lctx)
      `(,@((datum smuggle-kw) lctx) ,((datum smuggle-var) lctx)))
    
    ))

(begin-for-syntax /define-syntax-class (lambda-params surrounding-stx)
  #:attributes (smuggle)
  (pattern
    {~autoptic-list*-to surrounding-stx #:smuggle smuggle-params
      (
        {~var param (lambda-param surrounding-stx)}
        ...
        .
        {~or*
          {~and rest:id /~bind / smuggle-rest /lambda (lctx) #'rest}
          {~and nil ()
            {~bind / smuggle-rest /lambda (lctx)
              (contextualize-stx lctx #'nil)}}})}
    
    #:fail-when
    (check-duplicates #:key syntax-e
      (syntax->list #'({~? param.kw} ...)))
    "duplicate keyword for argument"
    
    #:fail-when
    (check-duplicate-identifier /syntax->list
      #'(param.var ... {~? rest}))
    "duplicate argument identifier"
    
    #:attr smuggle
    (lambda (lctx)
      ( (datum smuggle-params) lctx
        (append* /for/list
          ([smuggle-param (in-list (datum (param.smuggle ...)))])
          (smuggle-param lctx))
        ((datum smuggle-rest) lctx)))
    
    ))

(define-syntax-parser define/own-contract /
  {~autoptic-list
    (_ copattern
      {~var val/c
        (expr/c #'contract?
          #:name "the variable's ascribed contract")}
      {~optional {~seq {~autoptic #:antecedent-land} antecedent-land}
        #:defaults ([antecedent-land (datum->syntax this-syntax '())])
        }
      body:expr ...+)}
  (define stx this-syntax)
  (let next
    ([copattern #'copattern] [body (syntax->list #'(body ...))])
    (syntax-parse copattern
      [ {~autoptic (head . {~var args (lambda-params stx)})}
        (next #'head
          (list
            (quasisyntax/loc stx
              (lambda #,((datum args.smuggle) stx) #,@body))))]
      [ var:id
        (match body
          [ (list val)
            (with-syntax
              (
                [ own-contract-var
                  (make-signature-contract-id #'antecedent-land #'var
                    (lambda ()
                      (raise-syntax-error #f
                        "expected a define-own-contract-policies definition before using the policies"
                        stx)))])
              #`
              (begin
                (ascribe-own-contract var val/c.c
                  #:antecedent-land antecedent-land)
                (define var
                  #,
                  (if
                    (syntax-local-value
                      (make-own-contract-policy-id
                        (syntax-local-introduce #'antecedent-land)
                        '#%own-contract-policy-activating-internal-contracts?)
                      (lambda ()
                        (raise-syntax-error #f
                          "expected a define-own-contract-policies definition before using the policies"
                          stx)))
                    #`
                    (invariant-assertion (force own-contract-var)
                      #,val)
                    val))))]
          [ _
            (raise-syntax-error #f
              "bad syntax (multiple expressions after identifier)"
              stx)])])))



; Should be `#f` unless we're debugging to determine if contracts are
; a performance bottleneck.
;
(define-for-syntax suppressing-external-contracts? #f)

; Should be `#f` unless we're debugging this library's internal call
; graph.
;
(define-for-syntax activating-internal-contracts? #f)

(define-syntax-parse-rule/autoptic
  (init-shim
    {~optional {~seq #:antecedent-land antecedent-land}
      #:defaults ([antecedent-land (datum->syntax this-syntax '())])})
  
  #:with result
  #`(define-own-contract-policies #:antecedent-land antecedent-land
      
      #:suppressing-external-contracts?
      #,(datum->syntax #'() suppressing-external-contracts?)
      
      #:activating-internal-contracts?
      #,(datum->syntax #'() activating-internal-contracts?))
  
  result)
