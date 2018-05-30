#lang parendown racket/base

; lathe-comforts/struct
;
; Utilities for structs.

;   Copyright 2017-2018 The Lathe Authors
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
(require #/for-syntax #/only-in syntax/parse expr id syntax-parse)

(require #/for-syntax #/only-in lathe-comforts
  expect fn mat w- w-loop)

(require #/only-in racket/contract/region define/contract)
(require #/only-in racket/struct make-constructor-style-printer)

(require #/only-in lathe-comforts dissectfn expect fn w-)

; TODO: Document this export.
(provide struct-easy)



(define (guard-easy guard)
  (lambda slots-and-name
    (expect (reverse slots-and-name) (cons name rev-slots)
      (error "Expected a guard procedure to be called with at least a struct name argument")
    #/w- slots (reverse rev-slots)
      (apply guard slots)
      (apply values slots))))

(define-syntax (struct-easy stx)
  (syntax-parse stx #/ (_ (name:id slot:id ...) rest ...)
  #/w-loop next
    rest #'(rest ...)
    has-write #f
    ; NOTE: It's tempting to use `nothing` and `just` from
    ; `lathe-comforts/maybe` instead of a subsingleton list, but this
    ; way avoids a circular dependency between modules.
    maybe-phrase (list)
    options #'()
    
    (w- next
      (fn rest has-write-now maybe-phrase options-suffix
        (next rest (or has-write has-write-now) maybe-phrase
        #`#/#,@options #,@options-suffix))
    #/syntax-parse rest
      
      [()
      #/if has-write
        #`(begin
          ; TODO: This raises an error if the phrase expression
          ; doesn't evaluate to a string, but it could probably raise
          ; a better error. It raises a "broke its own contract"
          ; error, but ideally the error would say the caller broke
          ; the contract of `struct-easy`.
          (define/contract phrase string?
            #,(mat maybe-phrase (list phrase) phrase
              (format "a value of the ~s structure type"
              #/symbol->string #/syntax-e #'name)))
          (struct name (slot ...) #,@options))
        (next #'(#:write #/fn this #/list slot ...) #f maybe-phrase
        #'#/)]
      
      [(#:other rest ...) #/next #'() #f maybe-phrase #'#/rest ...]
      
      [(#:error-message-phrase phrase:expr rest ...)
      #/expect maybe-phrase (list)
        (error "Supplied #:error-message-phrase more than once")
      #/next #'(rest ...) #f (list #'phrase) #'#/rest ...]
      
      [(#:write writefn:expr rest ...)
      #/if has-write
        (error "Supplied #:write more than once")
      #/next #'(rest ...) #t maybe-phrase
      #'#/#:methods gen:custom-write #/
        (define write-proc
          (make-constructor-style-printer
            (fn this 'name)
            (fn this
              (expect this (name slot ...)
                (error #/string-append "Expected this to be " phrase)
              #/writefn this))))]
      
      [(#:equal rest ...)
      #/next #'(rest ...) #f maybe-phrase
      #'#/#:methods gen:equal+hash #/
        (define (equal-proc a b recursive-equal?)
          (expect a (name slot ...)
            (error #/string-append "Expected a to be " phrase)
          #/w- a-slots (list slot ...)
          #/expect b (name slot ...)
            (error #/string-append "Expected b to be " phrase)
          #/w- b-slots (list slot ...)
          ; NOTE: It's tempting to use `list-kv-all` from
          ; `lathe-comforts/list` instead of `andmap`, but this way
          ; avoids a circular dependency between modules.
          #/andmap (fn a b #/recursive-equal? a b) a-slots b-slots))
        (define (hash-proc this recursive-equal-hash-code)
          (expect this (name slot ...)
            (error #/string-append "Expected this to be " phrase)
          #/recursive-equal-hash-code #/list slot ...))
        (define (hash2-proc this recursive-equal-secondary-hash-code)
          (expect this (name slot ...)
            (error #/string-append "Expected this to be " phrase)
          #/recursive-equal-secondary-hash-code #/list slot ...))]
      
      [((#:guard-easy body:expr ...) rest ...)
      #/next #'(rest ...) #f maybe-phrase
      #`#/
        #:guard
        #,#/syntax-protect #'#/guard-easy #/lambda (slot ...)
          body ...])))
