#lang parendown/slash racket/base

; lathe-comforts/private/experimental/match
;
; Experimental utilities for match expanders.

;   Copyright 2019, 2022, 2025 The Lathe Authors
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

(require /for-syntax /only-in lathe-comforts w-)
(require /for-syntax /only-in lathe-comforts/syntax ~autoptic-list)

(require /only-in lathe-comforts fn)


(provide define-match-expander-via-lists)


(define-syntax-parser define-match-expander-via-lists /
  {~autoptic-list
    (_ name:id function-version:expr
      {~var inst->maybe-list
        (expr/c #'(-> any/c natural? (or/c #f list?))
          #:name "inst->maybe-list argument")})}
  #`
  (begin
    
    #,
    (quasisyntax/loc this-syntax
      (define inst->maybe-list-result inst->maybe-list.c))
    
    (define-match-expander name
      #,
      (quasisyntax/loc this-syntax
        ; TODO: We should really use a syntax class for match
        ; patterns rather than `expr` here, but it doesn't look
        ; like one exists yet.
        (syntax-parser / {~autoptic-list (_ arg:expr /... ...)}
          (w- n (length /syntax->list #'(arg /... ...))
            #`
            (app (fn v /inst->maybe-list-result v #,n)
              (list arg /... ...)))))
      #,
      (quasisyntax/loc this-syntax
        (syntax-parser
          [_:id #'function-version]
          [ {~autoptic-list (_ arg /... ...)}
            #'(function-version arg /... ...)])))))
