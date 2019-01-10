#lang racket

; lathe-comforts/tests/experimental/cross-phase-utils
;
; Experimental cross-phase manipulations and utilities. We've defined
; a more convenient way to develop cross-phase persistent modules
; (`module/cross-phase`), although it has rather terrible error
; messaging if the module uses features that aren't allowed for
; cross-phase persistent modules. We've defined a utility
; `define-eternal-struct` that creates a struct-like encapsulated
; product type that (hopefully) can only be taken apart using a
; variable reference of a module that was loaded from the same module
; path index.

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


(require (for-syntax (only-in racket/list range)))
(require (for-syntax syntax/parse))

; TODO: Use `syntax-protect` for the macros defined here.


(module m racket/kernel
  (#%declare #:cross-phase-persistent))

(define-syntax (module/cross-phase stx)
  (syntax-parse stx
    [ (_ name (#:require require-spec ...) body ...)
      (syntax-parse
        (local-expand
          #`(module name racket/kernel
              (#%require require-spec ...)
              body ...)
          'top-level
          (list))
        #:literals (module)
        [
          (module name (~datum racket/kernel)
            ( (~datum #%module-begin)
              ((~datum #%require) require-spec ...)
              body ...))
          #`(module name racket/kernel
              (#%declare #:cross-phase-persistent)
              #,@(syntax->datum #'(body ...)))])]))

(module/cross-phase m2
  (#:require racket)
  (provide blah-get)
  (define blah 2)
  (define (blah-get) blah)
  (define (blah-pass f) (f blah))
  (define-struct my-struct (val) #:omit-define-syntaxes))

(module/cross-phase eternals
  (#:require racket)
  (provide variable-reference->eternalizer)
  
  (define-values (prop:eternal eternal? eternal-ref)
    (make-struct-type-property 'eternal #f))
  
  (define-values (variable-reference->eternalizer)
    (lambda (name vr)
      (unless (and (symbol? name) (symbol-interned? name))
        (raise-arguments-error 'variable-reference->eternalizer
          "expected name to be an interned symbol"
          "name" name))
      (unless (variable-reference? vr)
        (raise-arguments-error 'variable-reference->eternalizer
          "expected vr to be a variable reference"
          "vr" vr))
      (define mpi (variable-reference->module-path-index vr))
      (unless mpi
        (raise-arguments-error 'variable-reference->eternalizer
          "expected vr to be a variable reference for a module varible"
          "vr" vr))
      (define-values (prop:eternalizer eternalizer? eternalizer-ref)
        (make-struct-type-property name
          (lambda (v struct-info)
            ; NOTE: Since we return `#f` as a sentinel result of
            ; `eternalized-ref`, we don't allow it to be the actual
            ; value.
            (unless v
              (raise-arguments-error name
                "expected the value to be something other than #f"))
            v)
          (list
            ; When this structure type property is used, it populates
            ; the `prop:eternal` structure type property so all phase
            ; levels of the same module can access the information from
            ; there.
            (cons prop:eternal
              (lambda (v) (vector-immutable name mpi v))))))
      (define (eternalized-ref v)
        (define vect (eternal-ref v #f))
        (and
          vect
          (eq? name (vector-ref vect 0))
          (equal? mpi (vector-ref vect 1))
          (vector-ref vect 2)))
      (values prop:eternalizer eternalized-ref))))

(require 'eternals)

(define-syntax (define-constructor-matcher stx)
  (syntax-parse stx
    [ (_ name (arg/c ...) vect->inst inst->maybe-vect)
      
      #:declare arg/c
      (expr/c #'contract? #:name "an argument contract")
      #:declare vect->inst
      (expr/c #'(-> any/c any/c) #:name "vect->inst argument")
      #:declare inst->maybe-vect
      (expr/c #'(-> any/c any/c) #:name "inst->maybe-vect argument")
      
      #:with (arg/c-result ...) (generate-temporaries #'(arg/c ...))
      #:with (local ...) (generate-temporaries #'(arg/c ...))
      #:with (local.c ...)
      (for/list ([var (in-list (syntax->list #'(local ...)))])
        (datum->syntax var
          (string->symbol (format "~a.c" (syntax-e var)))))
      
      #'(begin
          
          (define arg/c-result arg/c.c)
          ...
          (define vect->inst-result vect->inst.c)
          (define inst->maybe-vect-result inst->maybe-vect.c)
          
          (define-match-expander name
            (lambda (stx)
              (syntax-parse stx
                ; TODO: We should really use a syntax class for match
                ; patterns rather than `expr` here, but it doesn't
                ; look like one exists yet.
                [ (_ local ...)
                  (~@ #:declare local expr) ...
                  #'(app inst->maybe-vect-result
                      (? immutable? (vector local ...)))]))
            (lambda (stx)
              (syntax-parse stx
                [ (_ local ...)
                  ; TODO: See if we can give this `expr/c` a better
                  ; `#:name`.
                  (~@ #:declare local
                    (expr/c #'arg/c-result #:name "an argument"))
                  ...
                  #'(vect->inst-result
                      (vector-immutable local.c ...))]))))]))

(define-syntax (define-eternal-struct stx)
  (syntax-parse stx
    [
      (_ name:id inst-matcher inst? make-inst vr
        ([inst-field field/c] ...))
      #:declare vr (expr/c #'variable-reference? #:name "vr argument")
      #:declare field/c (expr/c #'contract? #:name "a field contract")
      #:with name-expr #''name
      #:declare name-expr
        (expr/c #'(and/c symbol? symbol-interned?)
          #:name "name argument")
      #:with (local ...) (generate-temporaries #'(inst-field ...))
      #:with (field-i ...)
      (range (length (syntax->list #'(inst-field ...))))
      #'(begin
          (define-values
            (
              vector->inst inst->maybe-vector inst? make-inst
              inst-field ...)
            (let ()
              (define vr-result vr.c)
              (define-values
                (prop:inst-eternalizer inst-eternalized-ref)
                (variable-reference->eternalizer
                  name-expr.c vr-result))
              (struct name (rep)
                #:property prop:inst-eternalizer
                (lambda (v)
                  (match v
                    [(name rep) rep])))
              (values
                (lambda (vect)
                  (name vect))
                (lambda (v)
                  (define rep (inst-eternalized-ref v))
                  (and rep (rep v)))
                (lambda (v)
                  (and (inst-eternalized-ref v) #t))
                (lambda (local ...)
                  (name (vector-immutable local ...)))
                (lambda (inst)
                  (define rep (inst-eternalized-ref inst))
                  (unless rep
                    (raise-arguments-error 'inst-field
                      ; TODO: Improve this error message.
                      "expected an instance of the correct external struct"
                      "inst" inst))
                  (vector-ref (rep inst) 'field-i))
                ...)))
          (define-constructor-matcher inst-matcher
            (field/c.c ...)
            vector->inst
            inst->maybe-vector))]))

(define-eternal-struct just just-matcher just? make-just
  (#%variable-reference)
  ([just-value any/c]))

(make-just 4)
(just? (make-just 4))
(just-value (make-just 4))
(just-value (just-matcher 4))
(match (just-matcher 4)
  [(just-matcher x) x])

(variable-reference->module-path-index (#%variable-reference))

; TODO: Test that the `just?` values are cross-phase persistent.

; TODO: Test that the `just?` values can't be taken apart by another
; module that uses `define-eternal-struct` using a variable reference
; that refers to that module's import of one of the identifiers of
; this module.
