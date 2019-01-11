#lang racket

; lathe-comforts/tests/experimental/cross-phase-utils-b
;
; Experimental manipulations of cross-phase utilities. In this module,
; we've mostly just made sure it's possible to have a cross-phase
; persistent submodule in one non-cross-phase-persistent module file
; that requires a cross-phase persistent submodule of another
; non-cross-phase-persistent module file.

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


(require (for-syntax syntax/parse))

(module m racket/kernel
  (#%declare #:cross-phase-persistent)
  (#%require
    (submod lathe-comforts/tests/experimental/cross-phase-utils m)))

(require 'm)

(define-syntax (go stx)
  #`'#,
  (local-expand
    #'
    (module m racket/kernel
      (#%declare #:cross-phase-persistent)
      ; NOTE: We can't require `racket` because this is a cross-phase
      ; persistent module.
;      (#%require racket)
      (#%require
        (submod lathe-comforts/tests/experimental/cross-phase-utils
          m)))
    'top-level
    (list)))

(go)


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
  (require
    (submod lathe-comforts/tests/experimental/cross-phase-utils m))
  (define blah 2))

(require 'm2)

(module/cross-phase m3
  (#:require racket)
  (require
    (submod lathe-comforts/tests/experimental/cross-phase-utils m2))
  (provide get-from-blah-get-m3)
  (define (get-from-blah-get-m3) (blah-get)))
(require 'm3)

(get-from-blah-get-m3)

(module m4 racket/kernel
  (#%declare #:cross-phase-persistent)
  (#%require
    (submod lathe-comforts/tests/experimental/cross-phase-utils m2))
  (#%provide get-from-blah-get-m4)
  (define-values (get-from-blah-get-m4)
    (lambda () (blah-get))))
(require 'm4)

(get-from-blah-get-m4)

(module mod1 racket)
(module mod2 racket (require (submod ".." mod1)))



; We test that eternal structs like `just?` are cross-phase
; persistent.

; Returns `'caught` because eternal structs are cross-phase
; persistent.
(parameterize ([current-namespace (make-base-namespace)])
  (eval
    '(require lathe-comforts/tests/experimental/cross-phase-utils))
  (define just? (eval 'just?))
  (with-handlers ([pair? (lambda (v) 'not-caught)])
    (with-handlers
      (
        [
          (lambda (v) (and (pair? v) (just? (cdr v))))
          (lambda (v) 'caught)])
      (eval
        '(module m racket
          (require (for-syntax lathe-comforts/tests/experimental/cross-phase-utils))
          (begin-for-syntax (raise (cons 1 (make-just 4)))))))))

; Returns `'not-caught` because regular structs are different in each
; phase level.
(parameterize ([current-namespace (make-base-namespace)])
  (eval
    '(require lathe-comforts/tests/experimental/cross-phase-utils))
  (define struct-just? (eval 'struct-just?))
  (with-handlers ([pair? (lambda (v) 'not-caught)])
    (with-handlers
      (
        [
          (lambda (v) (and (pair? v) (struct-just? (cdr v))))
          (lambda (v) 'caught)])
      (eval
        '(module m racket
          (require (for-syntax lathe-comforts/tests/experimental/cross-phase-utils))
          (begin-for-syntax (raise (cons 1 (struct-just 4)))))))))


; We test that the eternal structure types procured by one module
; can't be procured by another module (short of sharing their
; `(#%variable-reference)` results).

; Returns `#f` because variable references for different module path
; indexes obtain different eternal structure types.

(module m5 racket
  (require lathe-comforts/tests/experimental/cross-phase-utils)
  
  (provide result)
  
  (define-eternal-struct just
    imitation-just-matcher imitation-just? make-imitation-just
    (#%variable-reference)
    ([imitation-just-value any/c]))
  
  (define result (imitation-just? (make-just 4))))

(parameterize ([current-namespace (make-base-namespace)])
  (eval
    '(require
       (submod lathe-comforts/tests/experimental/cross-phase-utils-b
         m5)))
  (eval 'result))


; This returns `(list #t #f)` because eternal structure types procured
; by the same module are the same if and only if they have the same
; name.

(module m6 racket
  (require lathe-comforts/tests/experimental/cross-phase-utils)
  
  (provide result)
  
  (define-eternal-struct s1 s1-matcher s1? make-s1
    (#%variable-reference)
    ())
  (define-eternal-struct s1 s2-matcher s2? make-s2
    (#%variable-reference)
    ())
  (define-eternal-struct s3 s3-matcher s3? make-s3
    (#%variable-reference)
    ())
  
  (define result (list (s2? (make-s1)) (s3? (make-s1)))))

(parameterize ([current-namespace (make-base-namespace)])
  (eval
    '(require
       (submod lathe-comforts/tests/experimental/cross-phase-utils-b
         m6)))
  (eval 'result))


; This returns `#f` because the different module registries contain
; different instances of the
; `(submod lathe-comforts/tests/experimental/cross-phase-utils eternals)`
; module itself. That's the module where the implementation details of
; eternal structure types are defined, so they're only as "eternal" as
; that module is.

; TODO: See if we can make eternal structure types "more eternal" than
; this.
;
; Maybe the approach we can take is, instead of designing libraries
; like Lathe Comforts to define their eternal structure types from
; within the Racket module language, we design them to be affected by
; compile-time parameters in such a way that they can they can
; sometimes reuse structs and other top-level objects supplied to them
; by the compiler.
;
; The `current-module-name-resolver` parameter seems like a perfect
; choice: If we put each top-level object definition in its own tiny
; "config" module, then a custom resolver can cause a different module
; to be loaded in each one's place.
;
; Backwards compatibility would be backwards for config modules:
; Putting a new export into an existing config module would *break*
; compatibility since not all clients would necessarily have that
; export in their substitute implementations. This is usually the case
; for metaprogramming over other people's code -- since treating that
; code as data makes it possible for any code change to be a breaking
; change -- but we can still do a little bit better than usual. By
; making sure we only introduce new config options in *new config
; modules*, and even then only in ways that remain invisible when not
; using *new library features*, we can maintain backwards
; compatibility for any clients whose only use of metaprogramming is
; to replace the config modules.
;
; Maybe we can take a hybrid approach, where we use a config module to
; define hybrid structure types, and we use a cross-persistent module
; to implement that module at first, but any client who can use
; `current-module-name-resolver` to establish a "more eternal"
; implementation for their use case is welcome to do so.

(let ()
  (define make-just
    (parameterize ([current-namespace (make-base-namespace)])
      (eval
        '(require
           lathe-comforts/tests/experimental/cross-phase-utils))
      (eval 'make-just)))
  (define just?
    (parameterize ([current-namespace (make-base-namespace)])
      (eval
        '(require
           lathe-comforts/tests/experimental/cross-phase-utils))
      (eval 'just?)))
  (just? (make-just 4)))
