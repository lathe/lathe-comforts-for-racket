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
(require #/for-syntax #/only-in racket/list append*)
(require #/for-syntax #/only-in racket/struct-info
  extract-struct-info struct-info?)
(require #/for-syntax #/only-in syntax/parse expr id syntax-parse)

(require #/for-syntax #/only-in lathe-comforts
  dissect expect fn mat w- w-loop)

; NOTE: The Racket documentation says `get/build-late-neg-projection`
; is in `racket/contract/combinator`, but it isn't. It's in
; `racket/contract/base`. Since it's also in `racket/contract` and the
; documentation correctly says it is, we require it from there.
(require #/only-in racket/contract get/build-late-neg-projection)
(require #/only-in racket/contract/base contract-name flat-contract?)
(require #/only-in racket/contract/combinator
  blame-add-context contract-first-order-passes? make-contract
  make-flat-contract raise-blame-error)
(require #/only-in racket/contract/region define/contract)
(require #/only-in racket/struct make-constructor-style-printer)

(require #/only-in lathe-comforts dissect dissectfn expect fn w-)

(provide struct-easy)

; TODO: Export and document the commented-out ones if and when we have
; a use for them.
(provide
;  struct-descriptor
;  struct-constructor
  struct-predicate
  struct-accessor-by-name
;  struct-mutator-by-name
  istruct/c
  )



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
              (format "an instance of the ~s structure type"
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
          ; NOTE: It's tempting to use `list-zip-all` from
          ; `lathe-comforts/list` instead of `for/and`, but `for/and`
          ; lets us avoid a circular dependency between modules.
          #/for/and ([a (in-list a-slots)] [b (in-list b-slots)])
            (recursive-equal? a b)))
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


(define-for-syntax (get-struct-info stx name)
  (w- struct-info (syntax-local-value name)
  #/expect (struct-info? struct-info) #t
    (raise-syntax-error #f
      "expected a structure type identifier"
      stx name)
  #/extract-struct-info struct-info))

(define-syntax (struct-descriptor stx)
  (syntax-parse stx #/ (_ name:id)
  #/dissect (get-struct-info stx #'name)
    (list struct:foo make-foo foo? rev-getters rev-setters super)
  #/or struct:foo
    (raise-syntax-error #f
      (format "structure type ~a does not have an associated descriptor"
        (syntax->datum #'name))
      stx #'name)))

(define-syntax (struct-constructor stx)
  (syntax-parse stx #/ (_ name:id)
  #/dissect (get-struct-info stx #'name)
    (list struct:foo make-foo foo? rev-getters rev-setters super)
  #/or make-foo
    (raise-syntax-error #f
      (format "structure type ~a does not have an associated constructor"
        (syntax->datum #'name))
      stx #'name)))

(define-syntax (struct-predicate stx)
  (syntax-parse stx #/ (_ name:id)
  #/dissect (get-struct-info stx #'name)
    (list struct:foo make-foo foo? rev-getters rev-setters super)
  #/or foo?
    (raise-syntax-error #f
      (format "structure type ~a does not have an associated predicate"
        (syntax->datum #'name))
      stx #'name)))

(define-for-syntax
  (look-up-field
    struct-name field-name identifiers stx
    error-phrase-singular error-phrase-plural)
  (w- pasted-field-name
    (string->symbol
      (string-append
        (symbol->string #/syntax-e struct-name)
        "-"
        (symbol->string #/syntax-e field-name)))
  #/w- ids
    (append* #/for/list ([id (in-list identifiers)])
      (if
        (and (identifier? id)
          (eq? pasted-field-name #/syntax-e id))
        (list id)
        (list)))
  #/mat ids (list)
    (raise-syntax-error #f
      (format "structure type ~a does not have an associated ~a named ~a"
        (syntax-e struct-name)
        error-phrase-singular
        pasted-field-name)
      stx field-name)
  #/expect ids (list id)
    (raise-syntax-error #f
      (format "structure type ~a has multiple associated ~a named ~a"
        (syntax-e struct-name)
        error-phrase-plural
        pasted-field-name)
      stx field-name)
    id))

(define-syntax (struct-accessor-by-name stx)
  (syntax-parse stx #/ (_ struct-name:id field-name:id)
  #/dissect (get-struct-info stx #'name)
    (list struct:foo make-foo foo? rev-getters rev-setters super)
  ; TODO: Have this look up the result from the struct info of `super`
  ; if it's not directly in this list.
  #/look-up-field #'struct-name #'field-name rev-getters stx
    "field accessor" "field accessors"))

(define-syntax (struct-mutator-by-name stx)
  (syntax-parse stx #/ (_ struct-name:id field-name:id)
  #/dissect (get-struct-info stx #'name)
    (list struct:foo make-foo foo? rev-getters rev-setters super)
  ; TODO: Have this look up the result from the struct info of `super`
  ; if it's not directly in this list.
  #/look-up-field #'struct-name #'field-name rev-setters stx
    "field mutator" "field mutators"))


(define (istruct/c-impl foo-name foo? foo?-name make-foo fields)
  (w- field/cs
    (for/list ([field (in-list fields)])
      (dissect field (list get-field field/c field-blame-message)
        field/c))
  #/w- name
    (list* 'istruct/c foo-name
    #/for/list ([field/c (in-list field/cs)])
      (contract-name field/c))
  #/w- first-order
    (fn v
      (and (foo? v)
        (for/and ([field (in-list fields)])
          (dissect field (list get-field field/c field-blame-message)
          #/contract-first-order-passes? field/c #/get-field v))))
  #/if
    (for/and ([field/c (in-list field/cs)])
      (flat-contract? field/c))
    (make-flat-contract #:name name #:first-order first-order)
  #/make-contract #:name name #:first-order first-order
    
    #:late-neg-projection
    (fn blame
      (w- fields-with-projections
        (for/list ([field (in-list fields)])
          (dissect field (list get-field field/c field-blame-message)
          #/list get-field
            ( (get/build-late-neg-projection field/c)
              (blame-add-context blame field-blame-message))))
      #/fn v missing-party
        (expect (foo? v) #t
          (raise-blame-error blame #:missing-party missing-party v
            '(expected: "~e" given: "~e")
            foo?-name v)
        #/apply make-foo
          (for/list ([field (in-list fields)])
            (dissect field (list get-field field-late-neg-projection)
            #/field-late-neg-projection (get-field v)
              missing-party)))))))

(define-syntax (istruct/c stx)
  (syntax-parse stx #/ (_ name:id field/c:expr ...)
  #/dissect (get-struct-info stx #'name)
    (list struct:foo make-foo foo? rev-getters rev-setters super)
  #/mat make-foo #f
    (raise-syntax-error #f
      (format "structure type ~a does not have an associated constructor"
        (syntax->datum #'name))
      stx #'name)
  #/mat foo? #f
    (raise-syntax-error #f
      (format "structure type ~a does not have an associated predicate"
        (syntax->datum #'name))
      stx #'name)
  #/mat (for/and ([getter (in-list rev-getters)]) getter) #f
    (raise-syntax-error #f
      (format "structure type ~a is not associated with a full list of field accessors"
        (syntax->datum #'name))
      stx #'name)
  #/w- field/cs (syntax->list #'(field/c ...))
  #/w- n (length rev-getters)
  #/expect (= n #/length field/cs) #t
    (raise-syntax-error #f
      (format "expected ~a ~a because structure type ~a has ~a ~a"
        n
        (mat n 1 "contract" "contracts")
        (syntax->datum #'name)
        n
        (mat n 1 "field" "fields"))
      stx)
    #`(istruct/c-impl 'name
        #,foo?
        '#,foo?
        #,make-foo
        (list
          #,@(for/list
               (
                 [i (in-naturals)]
                 [getter (in-list #/reverse rev-getters)]
                 [field/c (in-list field/cs)])
               #`(list #,getter #,field/c
                   #,(format "field ~e (position ~a) of"
                       (syntax-e getter)
                       i)))))))
