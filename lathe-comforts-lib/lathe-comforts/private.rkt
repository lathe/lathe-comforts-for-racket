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

(require #/only-in racket/stxparam define-syntax-parameter)
(require #/only-in syntax/parse/define
  ~or* ~peek-not ~seq define-simple-macro expr id syntax-parse)

(provide #/all-defined-out)


(module part1 racket/base
  (require #/only-in syntax/parse/define expr define-simple-macro)
  (provide #/all-defined-out)
  
  (define-simple-macro (fn parms ... body:expr)
    (lambda (parms ...)
      body))
)
(require 'part1)
(require #/for-syntax 'part1)
(provide #/all-from-out 'part1)

; ====================================================================

; utils-part-2.arc
;
; Miscellaneous utilities, part 2.
;
; This is in several parts so that certain utilities can be used in
; the transformation phases of other utilities.


(define-simple-macro (thunk body:expr ...)
  (fn #/begin (void) body ...))


; ====================================================================


(define-simple-macro (zap! func:expr place:expr args:expr ...)
  (set! place (func place args ...)))

(define-syntax (w- stx)
  (syntax-parse stx
    [(_ result:expr) #'result]
    [ (_ var:id val:expr body ...)
      #'(let ([var val]) (w- body ...))]
    [ (_ command:expr body ...)
      #'(let () command (w- body ...))]))

(define-simple-macro (ret var:id val:expr body:expr ...)
  (w- [var val]
    body ...
    var))

(define-simple-macro (accum var:id body:expr ...)
  (ret result null
    (w- [var (fn elem (push! elem result))]
      body ...)))

(define-simple-macro
  (magic-withlike [op ...]
    (~or*
      ([(~seq var:id val:expr) ...] body:expr ...)
      ((~seq var:id val:expr) ... (~peek-not _:id) body:expr ...)))
  (op ... ([var val] ...)
    body ...))

(define-simple-macro (namedlet next:id binds-and-body)
  (magic-withlike (let next) binds-and-body))

(define-simple-macro (named name:id body:expr ...)
  (letrec ([name (begin (void) body ...)])
    name))

(define-simple-macro (rfn name:id parms ... body:expr)
  (named name (fn parms ... body)))

(define-syntax-rule (letr binds-and-body)
  (magic-withlike (letrec) binds-and-body))


; This is inspired by <http://blog.racket-lang.org/2008/02/
; dirty-looking-hygiene.html>.

(define-syntax-rule (w-stxparam binds-and-body)
  (magic-withlike (syntax-parameterize) binds-and-body))

(define-simple-macro
  (longhand-w-anaphor ([name:id transformer:id] ...) body:expr ...)
  (syntax-parameterize
    ([name (make-rename-transformer #'transformer)] ...)
    body ...))

(define-syntax-rule (w-anaphor binds-and-body)
  (magic-withlike (longhand-w-anaphor) binds-and-body))

(define-simple-macro (define-simple-syntax-parameter name:id)
  (define-syntax-parameter name #/fn stx
    (raise-syntax-error #f
      (string-append
        "Used syntax parameter `" (symbol->string 'name) "` "
        "without binding it first")
      stx)))

(define-simple-syntax-parameter a)
(define-simple-syntax-parameter b)

(define-simple-macro (abfn body:expr ...)
  (fn our-a our-b
    (w-anaphor [a our-a b our-b]
      body ...)))

(define-simple-syntax-parameter it)

(define-simple-macro (zapit! place:expr body:expr ...)
  (w- our-it place
    (w-anaphor it our-it
      (set! place (begin body ...)))))

(define-simple-syntax-parameter next)

(define-simple-macro
  (longhand-nextlet ([var:id val:expr] ...) body:expr ...)
  (let our-next ([var val] ...)
    (w-anaphor [next our-next]
      body ...)))

(define-syntax-rule (nextlet binds-and-body)
  (magic-withlike (longhand-nextlet) binds-and-body))

(define-simple-macro (nextfn parms ... body:expr)
  (rfn our-next parms ...
    (w-anaphor next our-next
      body)))


(define-simple-macro (push! elem:expr seq:expr)
  (zapit! seq (cons elem it)))


(define (pass arg func)
  (func arg))

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


; This takes something that might or might not be syntax, and it
; "de-syntaxes" it recursively.
(define (destx x)
  (syntax->datum #/datum->syntax #'foo x))
