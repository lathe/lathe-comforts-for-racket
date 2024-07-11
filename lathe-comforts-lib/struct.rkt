#lang parendown racket/base

; lathe-comforts/struct
;
; Utilities for structs.

;   Copyright 2017-2020, 2022, 2024 The Lathe Authors
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

(require #/for-syntax #/only-in lathe-comforts
  dissect expect fn mat w- w-loop)

(require #/only-in lathe-comforts
  dissect dissectfn expect fn mat w- w-loop)

; TODO: Deprecate `struct-easy` in favor of using
; `define-imitation-simple-struct` with `auto-write` and `auto-equal`.
(provide struct-easy)

; TODO: Export and document the commented-out ones if and when we have
; a use for them.
(provide #/own-contract-out
;  known-to-be-immutable-struct-type?
  prefab-struct?
  immutable-prefab-struct?
  mutable-prefab-struct?)
(provide
;  struct-descriptor
;  struct-constructor
  struct-predicate
  struct-accessor-by-name
;  struct-mutator-by-name
  istruct/c
  )
(provide #/own-contract-out
  tupler?
  tupler-length
  tupler/c
  tupler-pred?-fn
  tupler-ref-fn
  tupler-proj-fns
  tupler-make-fn
  ; NOTE: These are unsafe because they can create misbehaving
  ; tuplers. If we ever want to export them anyway, their
  ; documentation is available but commented out in
  ; lathe-comforts.scrbl.
;  tupler-from-pred-and-ref-and-make
;  tupler-from-pred-and-projs-and-make
  ; TODO: When we need to export this, uncomment it, and uncomment its
  ; documentation in lathe-comforts.scrbl.
;  tupler-for-simple-make-struct-type
  )
; TODO: When we need to export these tupler-related macros and
; `define-imitation-simple-struct/derived`, uncomment and document
; these exports.
(provide
;  tupler-for-simple-struct
;  tupler-for-simple-struct/derived
;  define-pred-and-projs-from-tupler
;  define-value-imitation-simple-struct
;  define-value-imitation-simple-struct/derived
;  define-match-expander-from-tupler
  define-syntax-and-value-imitation-simple-struct
;  define-syntax-and-value-imitation-simple-struct/derived
  define-imitation-simple-struct
;  define-imitation-simple-struct/derived
  auto-write
  auto-equal
  define-imitation-simple-generics
  )



; NOTE: There's an implementation of this in `lathe-comforts/list` as
; well, but we implement it here too to avoid a dependency cycle
; between modules.
(define/own-contract (list-length=nat? lst n)
  (-> list? natural? boolean?)
  (expect lst (cons _ lst) (= 0 n)
  #/and (not #/= 0 n)
  #/list-length=nat? lst (sub1 n)))


(define/own-contract (known-to-be-immutable-struct-type? v)
  (-> any/c boolean?)
  (and (struct-type? v)
  #/w-loop next v v
    (define-values
      (
        name
        init-field-cnt
        auto-field-cnt
        accessor-proc
        mutator-proc
        immutable-k-list
        super-type
        skipped?)
      (struct-type-info v))
    (and
      (not skipped?)
      (list-length=nat? immutable-k-list init-field-cnt)
      (or (not super-type) (next super-type)))))

(define/own-contract (known-to-be-mutable-struct-type? v)
  (-> any/c boolean?)
  (and (struct-type? v)
  #/w-loop next v v
    (define-values
      (
        name
        init-field-cnt
        auto-field-cnt
        accessor-proc
        mutator-proc
        immutable-k-list
        super-type
        skipped?)
      (struct-type-info v))
    (or
      (not #/list-length=nat? immutable-k-list init-field-cnt)
      (and super-type (next super-type)))))

(define/own-contract (prefab-struct? v)
  (-> any/c boolean?)
  (not #/not #/prefab-struct-key v))

(define/own-contract (immutable-prefab-struct? v)
  (-> any/c boolean?)
  (and (prefab-struct? v)
  #/let-values ([(type skipped?) (struct-info v)])
  #/and
    (not skipped?)
    type
    (known-to-be-immutable-struct-type? type)))

(define/own-contract (mutable-prefab-struct? v)
  (-> any/c boolean?)
  (and (prefab-struct? v)
  #/let-values ([(type skipped?) (struct-info v)])
  #/and type (known-to-be-mutable-struct-type? type)))


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
  #/dissect (get-struct-info stx #'struct-name)
    (list struct:foo make-foo foo? rev-getters rev-setters super)
  ; TODO: Have this look up the result from the struct info of `super`
  ; if it's not directly in this list.
  #/look-up-field #'struct-name #'field-name rev-getters stx
    "field accessor" "field accessors"))

(define-syntax (struct-mutator-by-name stx)
  (syntax-parse stx #/ (_ struct-name:id field-name:id)
  #/dissect (get-struct-info stx #'struct-name)
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

(define/own-contract (tupler? v)
  (-> any/c boolean?)
  (-tupler? v))

(define/own-contract (tupler-length tupler)
  (-> tupler? natural?)
  (-tupler-length tupler))

(define/own-contract (tupler/c length/c)
  (-> flat-contract? flat-contract?)
  (make-flat-contract #:name `(tupler/c ,(contract-name length/c))
    #:first-order
    (fn v
      (and (tupler? v)
        (contract-first-order-passes? length/c #/tupler-length v)))))

(define/own-contract (tupler-pred?-fn tupler)
  (-> tupler? #/-> any/c boolean?)
  (-tupler-pred?-fn tupler))

(define/own-contract (tupler-ref-fn tupler)
  (->i ([t tupler?]) #/_ (t)
    (-> (tupler-pred?-fn t) (and/c natural? (</c #/tupler-length t))
      any/c))
  (-tupler-ref-fn tupler))

(define/own-contract (tupler-proj-fns tupler)
  (->i ([t tupler?]) #/_ (t)
    (listof #/-> (tupler-pred?-fn t) any/c))
  (-tupler-proj-fns tupler))

(define/own-contract (tupler-make-fn tupler)
  (->i ([t tupler?])
    [_ (t)
      (and/c (procedure-arity-includes/c #/tupler-length t)
      #/unconstrained-domain-> #/tupler-pred?-fn t)])
  (-tupler-make-fn tupler))

(define/own-contract
  (tupler-from-pred-and-ref-and-make length pred?-fn ref-fn make-fn)
  (->i
    (
      [length natural?]
      [pred?-fn (-> any/c boolean?)]
      [ref-fn (length pred?-fn)
        (-> pred?-fn (and/c natural? (</c length)) any/c)]
      [make-fn (pred?-fn) (unconstrained-domain-> pred?-fn)])
    [_ tupler?])
  (-tupler
    length
    (fn v #/pred?-fn v)
    (fn self i #/ref-fn self i)
    (build-list length #/fn i
      (fn self #/ref-fn self i))
    (lambda args #/apply make-fn args)))

(define/own-contract
  (tupler-from-pred-and-projs-and-make pred?-fn proj-fns make-fn)
  (->i
    (
      [pred?-fn (-> any/c boolean?)]
      [proj-fns (pred?-fn) (listof #/-> pred?-fn any/c)]
      [make-fn (pred?-fn) (unconstrained-domain-> pred?-fn)])
    [_ tupler?])
  (w- vec-proj-fns (list->vector proj-fns)
  #/-tupler
    (vector-length vec-proj-fns)
    (fn v #/pred?-fn v)
    (fn self i
      ((vector-ref vec-proj-fns i) self))
    (for/list ([proj-fn (in-list proj-fns)])
      (fn self #/proj-fn self))
    (lambda args #/apply make-fn args)))


(define/own-contract
  (tupler-for-simple-make-struct-type
    length reflection-name inspector props)
  (->i
    (
      [length natural?]
      [reflection-name symbol?]
      [inspector (or/c inspector? #f 'prefab)]
      [props (inspector)
        (listof
          (mat inspector 'prefab none/c
          #/cons/c struct-type-property? any/c))])
    [_ tupler?])
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


(begin-for-syntax
  
  ; TODO: Once we have the interace for defining structure type
  ; property expanders ironed out a bit better, see if we should
  ; export the ways to define and call them so other people can define
  ; their own. For now, we just keep all the expanders within this
  ; module rather than making them user-definable.
  
  (define-values
    (
      prop:struct-property-expander
      struct-property-expander?
      struct-property-expander-ref)
    (make-struct-type-property 'struct-property-expander #f))
  
  (struct dedicated-struct-property-expander (impl)
    #:property prop:struct-property-expander
    (fn self len reflection-name-id tupler-id stx
      (dissect self (dedicated-struct-property-expander impl)
      #/impl len reflection-name-id tupler-id stx)))
  
  (define (make-struct-property-expander impl)
    (dedicated-struct-property-expander impl))
  
  (define-splicing-syntax-class
    (props-and-gens len reflection-name-id tupler-id)
    #:attributes ([expansion 1])
    (pattern #/~or*
      (~seq
        (#:prop ~!
          (~var prop-key
            (expr/c #'struct-type-property?
              #:name "one of the structure type properties"))
          (~var prop-val
            (expr/c #'any/c
              #:name "one of the associated structure type property values")))
        (~bind #/ [expansion 1]
          (syntax->list #'(#:property prop-key.c prop-val.c))))
      (~seq
        (#:gen ~! gen-name:id methods ...)
        (~bind #/ [expansion 1]
          (syntax->list #'(#:methods gen-name [methods ...]))))
      (~seq
        (~and (op:id ~! . _) call)
        (~fail #:unless
          (struct-property-expander? #/syntax-local-value #'op)
          "expected the name of a structure property expander")
        (~parse
          (
            (~var repeat
              (props-and-gens len reflection-name-id tupler-id))
            ...)
          (w- op (syntax-local-value #'op)
          #/w- expand (struct-property-expander-ref op)
          #/expand op len reflection-name-id tupler-id #'call))
        (~bind #/ [expansion 1]
          (syntax->list #'(repeat.expansion ... ...))))))
  
  )

(define-for-syntax
  (expand-tupler-for-simple-struct/derived orig-stx args)
  (syntax-protect
  #/syntax-parse args #:context orig-stx #/
    (reflection-name inspector len:nat args ...)
    
    #:declare reflection-name
    (expr/c #'symbol? #:context orig-stx
      #:name "reflection-name argument")
    
    #:declare inspector
    (expr/c #'(or/c inspector? #f 'prefab) #:context orig-stx
      #:name "inspector argument")
    
    #:declare args
    (props-and-gens (syntax-e #'len)
      #'reflection-name-result
      #'tupler)
    
    ; NOTE: We can't use `struct-accessor-by-name` to simplify this
    ; `inst-field` code because we're using `#:omit-define-syntaxes`.
    #:with ((field inst-field) ...)
    (build-list (syntax-e #'len) #/fn i
      (for/list ([format-string (in-list (list "~a" "inst-~a"))])
        (datum->syntax #'anything
          (string->symbol #/format format-string i))))
    
    #`(let ()
        (define reflection-name-result reflection-name.c)
        (define inspector-result inspector.c)
        (define tupler
          (let ()
            (define-struct/derived #,orig-stx inst (field ...)
              #:constructor-name inst
              #:reflection-name reflection-name-result
              #:inspector inspector-result
              #:omit-define-syntaxes
              args.expansion ... ...)
            (tupler-from-pred-and-projs-and-make
              inst?
              (list inst-field ...)
              (procedure-rename inst reflection-name-result))))
        tupler)))

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
        #/dissect (tupler-proj-fns tupler-val) (list proj-fn ...)
        #/values
          (procedure-rename (tupler-pred?-fn tupler-val) 'pred?)
          (procedure-rename proj-fn 'proj)
          ...))))


(define-for-syntax
  (expand-define-value-imitation-simple-struct/derived orig-stx args)
  (syntax-protect
  #/syntax-parse args #:context orig-stx #/
    ((pred?:id proj:id ...) tupler:id reflection-name inspector
      arg ...)
    #`(begin
        (define tupler
          (tupler-for-simple-struct/derived #,orig-stx
            reflection-name inspector
            #,(length #/syntax->list #'(proj ...))
            arg ...))
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
        (define make
          (procedure-rename (tupler-make-fn tupler-result) 'name))
        
        (define-match-expander name
          (fn stx
            ; TODO: We should really use a syntax class for match
            ; patterns rather than `expr` here, but it doesn't look
            ; like one exists yet.
            (syntax-protect
            #/syntax-parse stx #/ (_ (~var arg expr) ...)
              #'(app
                  (fn v #/let ()
                    (and (pred? v)
                    #/for/list ([proj (in-list projs)])
                      (proj v)))
                #/list arg ...)))
          (fn stx
            (syntax-protect #/syntax-parse stx
              
              ; NOTE: We allow the expander to expand into a reference
              ; to a construction-only procedure version of itself
              ; when it's used directly as an identifier. That way,
              ; we have leeway to upgrade constructor-like functions
              ; into match expanders, and this constructor-like match
              ; expander looks like the result of an upgrade like
              ; that.
              ;
              [_:id #'make]
              
              [(_ arg ...) #'(make arg ...)]))))))



(define-for-syntax
  (expand-define-syntax-and-value-imitation-simple-struct/derived
    orig-stx args)
  (syntax-protect
  #/syntax-parse args #:context orig-stx #/
    ((pred?:id proj:id ...) make:id tupler:id arg ...)
    #`(begin
        (define-value-imitation-simple-struct/derived #,orig-stx
          (pred? proj ...)
          tupler arg ...)
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

(define-for-syntax
  (expand-define-imitation-simple-struct/derived orig-stx args)
  (syntax-protect
  #/syntax-parse args #:context orig-stx #/
    ((pred?:id proj:id ...) make:id arg ...)
    #`(define-syntax-and-value-imitation-simple-struct/derived
        #,orig-stx (pred? proj ...) make tupler arg ...)))

(define-syntax (define-imitation-simple-struct stx)
  (syntax-parse stx #/ (_ make arg ...)
  #/expand-define-imitation-simple-struct/derived
    stx
    #'(make arg ...)))

(define-syntax (define-imitation-simple-struct/derived stx)
  (syntax-parse stx #/ (_ orig-stx make arg ...)
  #/expand-define-imitation-simple-struct/derived
    #'orig-stx
    #'(make arg ...)))

(define-syntax auto-write
  (make-struct-property-expander
  #/fn len reflection-name-id tupler-id stx
    ; TODO: See if we can use `syntax-protect` here.
    (syntax-parse stx #/ (_)
      #`[
          (#:prop prop:custom-print-quotable 'never)
          (#:gen gen:custom-write
            (define write-proc
              (make-constructor-style-printer
                (fn self #,reflection-name-id)
                (fn self
                  (w- pred? (tupler-pred?-fn #,tupler-id)
                  #/w- projs (tupler-proj-fns #,tupler-id)
                  #/expect (pred? self) #t
                    (raise-arguments-error 'write-proc
                      "expected self to be an instance of the structure type where this gen:custom-write behavior was installed"
                      "self" self)
                  #/for/list ([proj (in-list projs)])
                    (proj self))))))])))

(define-syntax auto-equal
  (make-struct-property-expander
  #/fn len reflection-name-id tupler-id stx
    ; TODO: See if we can use `syntax-protect` here.
    (syntax-parse stx #/ (_)
      #`[
          (#:gen gen:equal+hash
            
            (define (equal-proc a b recursive-equal?)
              (w- pred? (tupler-pred?-fn #,tupler-id)
              #/w- projs (tupler-proj-fns #,tupler-id)
              #/expect (pred? a) #t
                (raise-arguments-error 'equal-proc
                  "expected the first argument to be an instance of the structure type where this gen:equal+hash behavior was installed"
                  "a" a
                  "b" b)
              #/expect (pred? b) #t
                (raise-arguments-error 'equal-proc
                  "expected the second argument to be an instance of the structure type where this gen:equal+hash behavior was installed"
                  "a" a
                  "b" b)
              #/for/and ([proj (in-list projs)])
                (recursive-equal? (proj a) (proj b))))
            
            (define (hash-proc self recursive-equal-hash-code)
              (w- pred? (tupler-pred?-fn #,tupler-id)
              #/w- projs (tupler-proj-fns #,tupler-id)
              #/expect (pred? self) #t
                (raise-arguments-error 'hash-proc
                  "expected self to be an instance of the structure type where this gen:equal+hash behavior was installed"
                  "self" self)
              #/recursive-equal-hash-code
                (map (fn proj #/proj self) projs)))
            
            (define
              (hash2-proc self recursive-equal-secondary-hash-code)
              (w- pred? (tupler-pred?-fn #,tupler-id)
              #/w- projs (tupler-proj-fns #,tupler-id)
              #/expect (pred? self) #t
                (raise-arguments-error 'hash-proc
                  "expected self to be an instance of the structure type where this gen:equal+hash behavior was installed"
                  "self" self)
              #/recursive-equal-secondary-hash-code
                (map (fn proj #/proj self) projs)))
            
            )])))


(define-syntax (define-imitation-simple-generics stx)
  (syntax-protect
  #/syntax-parse stx #/
    (_ inst?:id inst-impl?:id
      (#:method method:id
        (~and ()
          (~parse (arg-before) #/generate-temporaries #'(before)))
        ...
        (~and (#:this)
          (~parse (arg-this) #/generate-temporaries #'(this)))
        (~and ()
          (~parse (arg-after) #/generate-temporaries #'(after)))
        ...)
      ...
      prop-inst:id build-inst-impl:id
      prop-reflection-name inst-impl-reflection-name supers)
    
    #:declare prop-reflection-name
    (expr/c #'symbol? #:name "prop-reflection-name argument")
    
    #:declare inst-impl-reflection-name
    (expr/c #'symbol? #:name "inst-impl-reflection-name argument")
    
    #:declare supers
    (expr/c #'(listof (cons/c struct-type-property? (-> any/c any/c)))
      #:name "supers argument")
    
    #:with (method-impl ...) (generate-temporaries #'(method ...))
    
    #'(define-values
        (inst? inst-impl? method ... prop-inst build-inst-impl)
        (let ()
          (define prop-reflection-name-result prop-reflection-name.c)
          (define inst-impl-reflection-name-result
            inst-impl-reflection-name.c)
          (define supers-result supers.c)
          (define-value-imitation-simple-struct
            (inst-impl? method-impl ...)
            tupler
            inst-impl-reflection-name-result (current-inspector))
          (define-values (prop-inst inst? prop-ref)
            (make-struct-type-property prop-reflection-name-result
              (fn impl struct-info #/begin0 impl
                (unless (inst-impl? impl)
                  (raise-arguments-error
                    (string->symbol
                      (format "prop:~a" prop-reflection-name-result))
                    "expected an instance of the specific structure type used to represent instances of this property"
                    
                    "impl" impl
                    
                    "expected-structure-type"
                    inst-impl-reflection-name-result)))
              supers-result))
          (define (method arg-before ... arg-this arg-after ...)
            ( (method-impl (prop-ref arg-this))
              arg-before ... arg-this arg-after ...))
          ...
          (define make-inst-impl (tupler-make-fn tupler))
          (define (build-inst-impl method-impl ...)
            (make-inst-impl method-impl ...))
          (values
            inst? inst-impl? method ... prop-inst build-inst-impl)))))
