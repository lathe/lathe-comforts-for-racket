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
