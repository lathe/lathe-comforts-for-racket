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
(require #/for-syntax #/only-in racket/list append* range)
(require #/for-syntax #/only-in racket/struct-info
  extract-struct-info struct-info?)
(require #/for-syntax #/only-in syntax/parse
  ~or* expr expr/c id nat syntax-parse)

(require #/for-syntax #/only-in lathe-comforts
  dissect expect fn mat w- w-loop)

; NOTE: The Racket documentation says `get/build-late-neg-projection`
; is in `racket/contract/combinator`, but it isn't. It's in
; `racket/contract/base`. Since it's also in `racket/contract` and the
; documentation correctly says it is, we require it from there.
(require #/only-in racket/contract get/build-late-neg-projection)
(require #/only-in racket/contract/base
  -> ->i </c and/c any/c cons/c contract? contract-name contract-out
  flat-contract? listof none/c or/c)
(require #/only-in racket/contract/combinator
  blame-add-context contract-first-order-passes? make-contract
  make-flat-contract raise-blame-error)
(require #/only-in racket/contract/region define/contract)
(require #/only-in racket/list range)
(require #/only-in racket/math natural?)
(require #/only-in racket/struct make-constructor-style-printer)

(require #/only-in lathe-comforts dissect dissectfn expect fn mat w-)

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
; TODO: Document all the exports below.
; TODO: Add keywords or utilities that can be used with the
; `...-simple-struct` forms to make it as convenient to create
; automatic `write` and `equal?` behaviors with them as it is to use
; `struct-easy`.
(provide #/contract-out
  [tupler? (-> any/c boolean?)]
  [tupler-length (-> tupler? natural?)]
  [tupler/c (-> flat-contract? flat-contract?)]
  [tupler-make-fn
    (->i ([t tupler?]) #/_ (t) #/-> any/c ... #/tupler-pred?-fn t)]
  [tupler-pred?-fn (-> tupler? #/-> any/c boolean?)]
  [tupler-ref-fn
    (->i ([t tupler?]) #/_ (t)
    #/-> (tupler-pred?-fn t) (and/c natural? (</c #/tupler-length t))
      any/c)]
  [tupler-from-pred-and-ref-and-make
    (->i
      (
        [length natural?]
        [pred?-fn (-> any/c boolean?)]
        [ref-fn (length pred?-fn)
          (-> pred?-fn (and/c natural? (</c length)) any/c)]
        [make-fn (pred?-fn) (-> any/c ... pred?-fn)])
      [_ tupler?])]
  [tupler-from-pred-and-projs-and-make
    (->i
      (
        [pred?-fn (-> any/c boolean?)]
        [proj-fns (pred?-fn) (listof #/-> pred?-fn any/c)]
        [make-fn (pred?-fn) (-> any/c ... pred?-fn)])
      [_ tupler?])]
  [tupler-for-simple-make-struct-type
    (->i
      (
        [inspector (or/c inspector? #f 'prefab)]
        [reflection-name symbol?]
        [length natural?]
        [props (inspector)
          (listof
            (mat inspector #f none/c
            #/cons/c struct-type-property? any/c))])
      [_ tupler?])]
  )
(provide
  tupler-for-simple-struct
  tupler-for-simple-struct/derived
  define-pred-and-projs-from-tupler
  define-value-imitation-simple-struct
  define-value-imitation-simple-struct/derived
  define-match-expander-from-tupler
  define-syntax-and-value-imitation-simple-struct
  define-syntax-and-value-imitation-simple-struct/derived
  define-imitation-simple-struct
  define-imitation-simple-struct/derived
  )



(define (guard-easy guard)
  (lambda slots-and-name
    (expect (reverse slots-and-name) (cons name rev-slots)
      (raise-arguments-error 'guard-easy
        "expected a guard procedure to be called with at least a struct name argument")
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
          (define phrase
            #,(mat maybe-phrase (list phrase) phrase
              (format "an instance of the ~s structure type"
              #/symbol->string #/syntax-e #'name)))
          (struct name (slot ...) #,@options))
        (next #'(#:write #/fn this #/list slot ...) #f maybe-phrase
        #'#/)]
      
      [(#:other rest ...) #/next #'() #f maybe-phrase #'#/rest ...]
      
      [ (#:error-message-phrase phrase rest ...)
        #:declare
        phrase
        (expr/c #'string? #:context stx
          #:name "option #:error-message-phrase")
      #/expect maybe-phrase (list)
        (raise-syntax-error #f
          "supplied #:error-message-phrase more than once"
          stx #'phrase)
      #/next #'(rest ...) #f (list #'phrase.c) #'#/rest ...]
      
      [ (#:write writefn rest ...)
        #:declare
        writefn
        (expr/c #'(-> any/c list?) #:context stx
          #:name "option #:write")
      #/if has-write
        (raise-syntax-error #f
          "supplied #:write more than once"
          stx #'writefn)
      #/next #'(rest ...) #t maybe-phrase
      #'#/#:methods gen:custom-write #/
        (define write-proc
          (make-constructor-style-printer
            (fn this 'name)
            (fn this
              (expect this (name slot ...)
                (raise-arguments-error 'write-proc
                  (string-append "expected this to be " phrase)
                  "this" this)
              #/writefn.c this))))]
      
      [(#:equal rest ...)
      #/next #'(rest ...) #f maybe-phrase
      #'#/#:methods gen:equal+hash #/
        (define (equal-proc a b recursive-equal?)
          (expect a (name slot ...)
            (raise-arguments-error 'equal-proc
              (string-append "expected a to be " phrase)
              "a" a)
          #/w- a-slots (list slot ...)
          #/expect b (name slot ...)
            (raise-arguments-error 'equal-proc
              (string-append "expected b to be " phrase)
              "b" b)
          #/w- b-slots (list slot ...)
          #/for/and ([a (in-list a-slots)] [b (in-list b-slots)])
            (recursive-equal? a b)))
        (define (hash-proc this recursive-equal-hash-code)
          (expect this (name slot ...)
            (raise-arguments-error 'hash-proc
              (string-append "expected this to be " phrase)
              "this" this)
          #/recursive-equal-hash-code #/list slot ...))
        (define (hash2-proc this recursive-equal-secondary-hash-code)
          (expect this (name slot ...)
            (raise-arguments-error 'hash2-proc
              (string-append "expected this to be " phrase)
              "this" this)
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
          (for/list ([field (in-list fields-with-projections)])
            (dissect field (list get-field field-late-neg-projection)
            #/field-late-neg-projection (get-field v)
              missing-party)))))))

(define-syntax (istruct/c stx)
  (syntax-parse stx #/ (_ name:id field/c ...)
    #:declare field/c (expr/c #'contract? #:name "one of the fields")
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
  #/w- field/cs (syntax->list #'(field/c.c ...))
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


(struct -tupler (length pred?-fn ref-fn proj-fns make-fn)
  #:reflection-name 'tupler)

(define (tupler? v)
  (-tupler? v))

(define (tupler-length tupler)
  (-tupler-length tupler))

(define (tupler/c length/c)
  (make-flat-contract #:name `(tupler/c ,(contract-name length/c))
    #:first-order
    (fn v
      (and (tupler? v)
        (contract-first-order-passes? length/c #/tupler-length v)))))

(define (tupler-make-fn tupler)
  (-tupler-make-fn tupler))

(define (tupler-pred?-fn tupler)
  (-tupler-pred?-fn tupler))

(define (tupler-ref-fn tupler)
  (-tupler-ref-fn tupler))

(define (tupler-proj-fns tupler)
  (-tupler-proj-fns tupler))

(define
  (tupler-from-pred-and-ref-and-make length pred?-fn ref-fn make-fn)
  (-tupler
    length
    (fn v #/pred?-fn v)
    (fn self i #/ref-fn self i)
    (build-list length #/fn i
      (fn self #/ref-fn self i))
    (lambda args #/apply make-fn args)))

(define
  (tupler-from-pred-and-projs-and-make make-fn pred?-fn proj-fns)
  (w- vec-proj-fns (list->vector proj-fns)
  #/-tupler
    (vector-length vec-proj-fns)
    (fn v #/pred?-fn v)
    (fn self i
      ((vector-ref vec-proj-fns i) self))
    (for/list ([proj-fn (in-list proj-fns)])
      (fn self #/proj-fn self))
    (lambda args #/apply make-fn args)))


(define
  (tupler-for-simple-make-struct-type
    inspector reflection-name length props)
  (define-values (struct:inst make-inst inst? inst-ref inst-set!)
    (make-struct-type
      #;name reflection-name
      #;super-type #f
      #;init-field-cnt length
      #;auto-field-cnt 0
      #;auto-v #f
      #;props props
      #;inspector inspector
      #;proc-spec #f
      #;immutables (range length)
      ))
  (tupler-from-pred-and-ref-and-make length inst? inst-ref make-inst))


(define-for-syntax
  (expand-tupler-for-simple-struct/derived orig-stx args)
  (syntax-protect
  #/syntax-parse args #/
    (inspector reflection-name len:nat
      (~or*
        (#:prop prop-key prop-val)
        (#:gen gen-name:id methods ...))
      ...)
    
    #:declare inspector
    (expr/c #'(or/c inspector? #f 'prefab) #:context orig-stx
      #:name "inspector argument")
    
    #:declare reflection-name
    (expr/c #'symbol? #:name "reflection-name argument")
    
    #:declare prop-key
    (expr/c #'struct-type-property? #:context orig-stx
      #:name "one of the structure type properties")
    
    #:declare prop-val
    (expr/c #'any/c #:context orig-stx
      #:name "one of the associated structure type property values")
    
    #:with ((field inst-field) ...)
    (build-list (syntax-e #'len) #/fn i
      (for/list ([format-string (in-list (list "~a" "inst-~a"))])
        (datum->syntax #'anything
          (string->symbol #/format format-string i))))
    
    #`(let ()
        (define-struct/derived #,orig-stx inst (field ...)
          #:constructor-name inst
          #:inspector inspector.c
          #:reflection-name reflection-name.c
          #:omit-define-syntaxes
          (~?
            (~@ #:property prop-key.c prop-val.c)
            (~@ #:methods gen-name [methods ...]))
          ...)
        (tupler-from-make-and-pred-and-projs inst inst?
          (list inst-field ...)))))

(define-syntax (tupler-for-simple-struct stx)
  (syntax-parse stx #/ (_ arg ...)
  #/expand-tupler-for-simple-struct/derived stx #'(arg ...)))

(define-syntax (tupler-for-simple-struct/derived stx)
  (syntax-parse stx #/ (_ orig-stx arg ...)
  #/expand-tupler-for-simple-struct/derived #'orig-stx #'(arg ...)))


(define-syntax (define-pred-and-projs-from-tupler stx)
  (syntax-protect
  #/syntax-parse stx #/ (_ pred?:id proj:id ... tupler)
    #:declare tupler
    (expr/c #`(tupler/c #,(length #/syntax->list #'(proj ...)))
      #:name "tupler argument")
    #:with (proj-fn ...) (generate-temporaries #'(proj ...))
    #'(define-values (pred? proj ...)
        (w- tupler-val tupler.c
          (match-define (list proj-fn ...)
            (tupler-proj-fns tupler-val))
          (values
            (procedure-rename (tupler-pred?-fn tupler-val) 'pred?)
            (procedure-rename proj-fn 'proj)
            ...)))))


(define-for-syntax
  (expand-define-value-imitation-simple-struct/derived orig-stx args)
  (syntax-protect
  #/syntax-parse args #/
    (tupler:id pred?:id (proj:id ...) inspector reflection-name
      (~or*
        (#:prop prop-key prop-val)
        (#:gen gen-name:id methods ...))
      ...)
    
    #:declare inspector
    (expr/c #'(or/c inspector? #f 'prefab) #:context orig-stx
      #:name "inspector argument")
    
    #:declare reflection-name
    (expr/c #'symbol? #:name "reflection-name argument")
    
    #:declare prop-key
    (expr/c #'struct-type-property? #:context orig-stx
      #:name "one of the structure type properties")
    
    #:declare prop-val
    (expr/c #'any/c #:context orig-stx
      #:name "one of the associated structure type property values")
    
    #`(begin
        (define tupler
          (tupler-for-simple-struct/derived #,orig-stx
            inspector.c reflection-name.c
            #,(length #/syntax->list #'(proj ...))
            (~?
              (#:prop prop-key.c prop-val.c)
              (#:gen gen-name methods ...))
            ...))
        (define-pred-and-projs-from-tupler pred? proj ... tupler))))

(define-syntax (define-value-imitation-simple-struct stx)
  (syntax-parse stx #/ (_ arg ...)
  #/expand-define-value-imitation-simple-struct/derived
    stx
    #'(arg ...)))

(define-syntax (define-value-imitation-simple-struct/derived stx)
  (syntax-parse stx #/ (_ orig-stx arg ...)
  #/expand-define-value-imitation-simple-struct/derived
    #'orig-stx
    #'(arg ...)))


(define-syntax (define-match-expander-from-tupler stx)
  (syntax-protect #/syntax-parse stx #/ (_ name:id len:nat tupler)
    
    #:declare tupler
    (expr/c #'(tupler/c len) #:name "tupler argument")
    
    #:with (arg ...) (generate-temporaries #/range #/syntax-e #'len)
    
    #'(begin
        
        (define tupler-result tupler.c)
        (define pred? (tupler-pred?-fn tupler-result))
        (define projs (tupler-proj-fns tupler-result))
        (define make (tupler-make-fn tupler-result))
        
        (define-match-expander name
          (lambda (stx)
            ; TODO: We should really use a syntax class for match
            ; patterns rather than `expr` here, but it doesn't look
            ; like one exists yet.
            (syntax-protect
            #/syntax-parse stx #/ (_ arg ...)
              (~@ #:declare arg expr) ...
              #`(app
                  (fn v #/let ()
                    (and (pred? v)
                    #/for/list ([proj (in-list projs)])
                      (proj v)))
                #/list arg ...)))
          (lambda (stx)
            (syntax-protect #/syntax-parse stx
              [_:id #'function-version]
              [(_ arg ...) #'(make arg ...)]))))))



(define-for-syntax
  (expand-define-syntax-and-value-imitation-simple-struct/derived
    orig-stx args)
  (syntax-protect
  #/syntax-parse args #/
    (make:id tupler:id pred?:id (proj:id ...)
      inspector
      reflection-name
      (~or*
        (#:prop prop-key prop-val)
        (#:gen gen-name:id methods ...))
      ...)
    
    #:declare inspector
    (expr/c #'(or/c inspector? #f 'prefab) #:context orig-stx
      #:name "inspector argument")
    
    #:declare reflection-name
    (expr/c #'symbol? #:name "reflection-name argument")
    
    #:declare prop-key
    (expr/c #'struct-type-property? #:context orig-stx
      #:name "one of the structure type properties")
    
    #:declare prop-val
    (expr/c #'any/c #:context orig-stx
      #:name "one of the associated structure type property values")
    
    #`(begin
        (define-value-imitation-simple-struct/derived #,orig-stx
          tupler pred? (proj ...) inspector.c reflection-name.c
          (~?
            (#:prop prop-key.c prop-val.c)
            (#:gen gen-name methods ...))
          ...)
        (define-match-expander-from-tupler make
          #,(length #/syntax->list #'(proj ...))
          tupler))))

(define-syntax (define-syntax-and-value-imitation-simple-struct stx)
  (syntax-parse stx #/ (_ arg ...)
  #/expand-define-syntax-and-value-imitation-simple-struct/derived
    stx
    #'(arg ...)))

(define-syntax
  (define-syntax-and-value-imitation-simple-struct/derived stx)
  (syntax-parse stx #/ (_ orig-stx arg ...)
  #/expand-define-syntax-and-value-imitation-simple-struct/derived
    #'orig-stx
    #'(arg ...)))

(define-syntax (define-imitation-simple-struct stx)
  (syntax-parse stx #/ (_ make arg ...)
  #/expand-define-syntax-and-value-imitation-simple-struct/derived
    stx
    #'(make tupler arg ...)))

(define-syntax (define-imitation-simple-struct/derived stx)
  (syntax-parse stx #/ (_ orig-stx make arg ...)
  #/expand-define-syntax-and-value-imitation-simple-struct/derived
    #'orig-stx
    #'(make tupler arg ...)))
