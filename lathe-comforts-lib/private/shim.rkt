#lang parendown/slash racket/base

; shim.rkt
;
; Import lists, debugging constants, other utilities that are useful
; primarily for this codebase, preliminary utilities for defining
; these things, and various utilities that could come in handy in
; other codebases for making shim files like this one.

;   Copyright 2021, 2022 The Lathe Authors
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
  (for-syntax
    eval-for-so-to-speak)
  so-to-speak
  ifc
  whenc
  unlessc
  condc
  (for-syntax
    pattern-directive)
  define-provide-pre-transformer-syntax-parse-rule
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
    mangle-for-own-contract)
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
    using-external-contracts?
    using-internal-contracts?)
  init-shim)


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

(define-syntax-parse-rule (so-to-speak body:expr ...+)
  #:with result (eval-for-so-to-speak this-syntax #'(body ...))
  result)

(define-syntax-parse-rule
  (ifc next-phase-condition:expr then:expr else:expr)
  (so-to-speak /if next-phase-condition
    #'then
    #'else))

(define-syntax-parser whenc /
  (_ next-phase-condition:expr body:expr ...+)
  (when (equal? 'expression (syntax-local-context))
    (raise-syntax-error #f
      "not permitted in an expression context"
      this-syntax))
  #'(ifc next-phase-condition
      (begin body ...)
      (begin)))

(define-syntax-parser unlessc /
  (_ next-phase-condition:expr body:expr ...+)
  (when (equal? 'expression (syntax-local-context))
    (raise-syntax-error #f
      "not permitted in an expression context"
      this-syntax))
  #'(whenc (not next-phase-condition) body ...))

(define-syntax-parser condc /
  (_
    {~and {~seq clause ...}
      {~seq
        [
          {~and next-phase-condition:expr
            {~not /~or {~literal else} {~literal =>}}}
          {~and branch:expr
            {~not /~or {~literal else} {~literal =>}}}
          ...]
        ...
        {~optional
          {~and {~bind [has-else-clause? #t]}
            [ {~literal else}
              {~and else-branch:expr
                {~not /~or {~literal else} {~literal =>}}}
              ...]}
          #:defaults
          ([has-else-clause? #f] [(else-branch 1) (list)])}}})
  (when (equal? 'expression (syntax-local-context))
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
  #'(so-to-speak /cond
      [next-phase-condition #'/begin branch ...]
      ...
      [else #'/begin else-branch ...]))

(begin-for-syntax /define-splicing-syntax-class pattern-directive
  #:attributes ([parts 1])
  (pattern
    {~and
      {~or
        {~seq
          #:declare _:id {~or _:id (_:id _ ...)}
          {~optional /~seq #:role _:expr}}
        {~seq #:post _:expr}
        {~seq #:and _:expr}
        {~seq #:with _ _:expr}
        {~seq #:attr {~or _:id [_:id _:nat]} _:expr}
        {~seq #:fail-when _:expr _:expr}
        {~seq #:fail-unless _:expr _:expr}
        {~seq #:when _:expr}
        {~seq #:do [_:expr ...]}
        {~seq #:undo [_:expr ...]}
        {~seq #:cut}}
      {~seq parts ...}}))

(define-syntax-parse-rule
  (define-provide-pre-transformer-syntax-parse-rule
    (name:id . pattern)
    directive:pattern-directive ...
    template)
  (define-syntax name
    (make-provide-pre-transformer /lambda (stx modes)
      (syntax-parse stx / (_ . pattern)
        directive.parts ... ...
        (pre-expand-export #'template modes)))))

; TODO: See if we'll use this.
(define-provide-pre-transformer-syntax-parse-rule
  (so-to-speak-out body:expr ...+)
  #:with result (eval-for-so-to-speak this-syntax #'(body ...))
  result)

(define-provide-pre-transformer-syntax-parse-rule
  (ifc-out next-phase-condition:expr then-out else-out . args)
  
  #:with result
  (if (syntax-local-eval #'next-phase-condition)
    #'(then-out . args)
    #'(else-out . args))
  
  result)

(define-provide-pre-transformer-syntax-parse-rule
  (contract-ignored-out
    [ {~and var:id /~not /~or {~literal struct} {~literal rename}}
      val/c]
    ...)
  
  #:declare val/c
  (expr/c #'contract? #:name "one of the contracts to ignore")
  
  ; We evaluate the contracted expressions just to ensure that we
  ; don't ignore any syntax, scoping, or run time errors they have.
  ;
  (contract-out [var (begin val/c.c any/c)] ...))

(define-provide-pre-transformer-syntax-parse-rule
  (contract-whenc-out next-phase-condition:expr
    [ {~and var:id /~not /~or {~literal struct} {~literal rename}}
      val/c]
    ...)
  
  #:declare val/c
  (expr/c #'contract?
    #:name "one of the contracts to apply to the exports")
  
  (ifc-out next-phase-condition contract-out contract-ignored-out
    [var val/c.c]
    ...))

(define-provide-pre-transformer-syntax-parse-rule
  (contract-unlessc-out next-phase-condition:expr
    [ {~and var:id /~not /~or {~literal struct} {~literal rename}}
      val/c]
    ...)
  
  #:declare val/c
  (expr/c #'contract?
    #:name "one of the contracts to apply to the exports")
  
  (contract-whenc-out (not next-phase-condition)
    [var val/c.c]
    ...))

(define-provide-pre-transformer-syntax-parse-rule
  (recontract-whenc-out next-phase-condition:expr var:id ...)
  (ifc-out next-phase-condition recontract-out combine-out var ...))

(define-provide-pre-transformer-syntax-parse-rule
  (recontract-unlessc-out next-phase-condition:expr var:id ...)
  (recontract-whenc-out (not next-phase-condition) var ...))

(begin-for-syntax /define-syntax-class module-path
  (pattern path #:when (module-path? /syntax->datum #'path)))

; TODO: See if this works, and see if we'll use it. For now, we're
; achieving the same purposes using `#lang reprovide` in
; `lathe-comforts/private/codebasewide-requires`.
#;
(define-syntax-parser define-requirer /
  (_ name:id
    {~and clause
      {~or _:module-path ({~literal only-in} _:module-path _:id ...)}}
    ...)
  #`(define-syntax-parser name / (_)
      #`(require
          #,@(list
               #,@(for/list
                    ([clause (in-list /syntax->list #'/clause ...)])
                    (syntax-parse clause
                      [path #'(syntax-local-introduce 'path)]
                      [
                        [path var:id ...]
                        #'#`(only-in #,(syntax-local-introduce 'path)
                              var ...)]))))))

; TODO: See if this works, and see if we'll use it. For now, we're
; achieving the same purposes using `#lang reprovide` in
; `lathe-comforts/private/codebasewide-requires`.
#;
(define-syntax-parser define-requirer-in /
  (_ name:id
    {~and clause
      {~or _:module-path ({~literal only-in} _:module-path _:id ...)}}
    ...)
  #`(define-syntax name /make-require-transformer /syntax-parser / (_)
      (expand-import
        #`(combine-in
            #,@(list
                 #,@(for/list
                      ([clause (in-list /syntax->list #'/clause ...)])
                      (syntax-parse clause
                        [path #'(syntax-local-introduce 'path)]
                        [
                          [path var:id ...]
                          #'#`(only-in
                                #,(syntax-local-introduce 'path)
                                var ...)])))))))


(define-for-syntax own-contract-scope (make-syntax-introducer))
(define-for-syntax own-contract-policy-scope (make-syntax-introducer))
(define-for-syntax (make-own-contract-policy-id stx name)
  (own-contract-policy-scope /datum->syntax stx name))

(define-for-syntax
  (mangle-for-own-contract stx-where-policy-is-in-scope orig)
  (
    (syntax-local-value
      (make-own-contract-policy-id stx-where-policy-is-in-scope
        '#%own-contract-policy-mangle)
      
      ; NOTE: By default, we just add a scope to the identifier. This
      ; works within a single module, but the identifier's bindings
      ; won't be visible to `module*` or `module+` submodules. To
      ; achieve that visibility, the name needs to be interned, with a
      ; user-supplied naming convention to set it apart from other
      ; variables in that context. This is what the mangle policy is
      ; for.
      ;
      (lambda () /lambda (orig) /own-contract-scope orig))
    orig))

(define-syntax-parse-rule
  (define-own-contract-policies
    #:mangle mangle
    #:using-external-contracts? using-external-contracts?
    #:using-internal-contracts? using-internal-contracts?)
  
  #:declare mangle
  (expr/c #'(-> identifier? identifier?)
    #:phase (add1 /syntax-local-phase-level)
    #:name "the mangle policy")
  
  #:declare using-external-contracts?
  (expr/c #'boolean? #:phase (add1 /syntax-local-phase-level)
    #:name "the external contract policy")
  
  #:declare using-internal-contracts?
  (expr/c #'boolean? #:phase (add1 /syntax-local-phase-level)
    #:name "the internal contract policy")
  
  #:with result
  #`(begin
      (define-syntax
        #,(make-own-contract-policy-id this-syntax
            '#%own-contract-policy-mangle)
        mangle.c)
      (define-syntax
        #,(make-own-contract-policy-id this-syntax
            '#%own-contract-policy-using-external-contracts?)
        using-external-contracts?.c)
      (define-syntax
        #,(make-own-contract-policy-id this-syntax
            '#%own-contract-policy-using-internal-contracts?)
        using-internal-contracts?.c))
  
  result)

(begin-for-syntax /define-syntax-class own-contracted-id
  #:attributes (val/c)
  (pattern
    {~and var:id /~not /~or {~literal struct} {~literal rename}}
    
    #:with val/c-unguarded
    (mangle-for-own-contract (syntax-local-introduce this-syntax)
      #'var)
    
    #:declare val/c-unguarded
    (expr/c #'contract?
      #:name "the ascribe-own-contract value of a variable")
    
    #:attr val/c #'val/c-unguarded.c))

(define-provide-pre-transformer-syntax-parse-rule
  (own-contract-ignored-out var:own-contracted-id ...)
  (contract-ignored-out [var var.val/c] ...))

(define-provide-pre-transformer-syntax-parse-rule
  (own-contract-out var:own-contracted-id ...)
  
  #:with result
  #`(contract-whenc-out
      (syntax-local-value
        #'#,(make-own-contract-policy-id this-syntax
              '#%own-contract-policy-using-external-contracts?)
        (lambda () #t))
      [var var.val/c]
      ...)
  
  result)

(define-provide-pre-transformer-syntax-parse-rule
  (own-contract-whenc-out next-phase-condition:expr
    var:own-contracted-id ...)
  
  #:with result
  #`(contract-whenc-out
      (and next-phase-condition
        (syntax-local-value
          #'#,(make-own-contract-policy-id this-syntax
                '#%own-contract-policy-using-external-contracts?)
          (lambda () #t)))
      [var var.val/c]
      ...)
  
  result)

(define-provide-pre-transformer-syntax-parse-rule
  (own-contract-unlessc-out next-phase-condition:expr
    var:own-contracted-id ...)
  (contract-whenc-out (not next-phase-condition)
    [var var.val/c]
    ...))

(define-syntax-parse-rule (ascribe-own-contract var:id val/c)
  
  #:declare val/c
  (expr/c #'contract?
    #:name "the variable's ascribed contract")
  
  #:with own-contract-var (mangle-for-own-contract this-syntax #'var)
  
  #:do
  [
    (when (equal? 'expression (syntax-local-context))
      (raise-syntax-error #f
        "not allowed in an expression context"
        this-syntax))]
  
  (define own-contract-var val/c.c))

(begin-for-syntax /define-splicing-syntax-class lambda-param
  #:attributes (kw var default)
  (pattern
    {~seq {~optional kw:keyword} {~or var:id [var:id default:expr]}}))

(begin-for-syntax /define-syntax-class lambda-params
  #:attributes ()
  (pattern
    (param:lambda-param ... . {~or rest:id ()})
    
    #:fail-when
    (check-duplicates #:key syntax-e
      (syntax->list #'({~? param.kw} ...)))
    "duplicate keyword for argument"
    
    #:fail-when
    (check-duplicate-identifier /syntax->list
      #'(param.var ... {~? rest}))
    "duplicate argument identifier"))

(define-syntax-parser define/own-contract
  [
    (_ var:id val/c body:expr)
    
    #:declare val/c
    (expr/c #'contract?
      #:name "the variable's ascribed contract")
    
    #:with own-contract-var (mangle-for-own-contract this-syntax #'var)
    
    #`(begin
        #,(datum->syntax this-syntax /syntax->list
            #'(ascribe-own-contract var val/c.c))
        #,(if
            (syntax-local-value
              (make-own-contract-policy-id
                (syntax-local-introduce this-syntax)
                '#%own-contract-policy-using-internal-contracts?)
              (lambda () #f))
            #'(define var (invariant-assertion own-contract-var body))
            #'(define var body)))]
  [
    (_ (head . args:lambda-params) val/c:expr body:expr ...+)
    (datum->syntax this-syntax /syntax->list
      #'(define/own-contract head val/c (lambda args body ...)))])



; Should be `#t` unless we're debugging to determine if contracts are
; a performance bottleneck.
;
(define-for-syntax using-external-contracts? #t)

; Should be `#f` unless we're debugging this library's internal call
; graph.
;
(define-for-syntax using-internal-contracts? #f)

(define-syntax-parse-rule (init-shim)
  
  #:with result
  (datum->syntax this-syntax
    `(,#'define-own-contract-policies
       
       #:mangle
       ,#`(lambda (orig)
            (own-contract-scope orig) #;
            (format-id orig "~a/sig-c" orig #:subs? #t))
       
       #:using-external-contracts?
       ,(datum->syntax #'() using-external-contracts?)
       
       #:using-internal-contracts?
       ,(datum->syntax #'() using-internal-contracts?)))
  
  result)
