#lang parendown racket/base

; lathe-comforts/match
;
; Utilities for match expanders.

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


(require #/for-syntax racket/base)

(require #/for-syntax #/only-in syntax/parse
  expr expr/c id syntax-parse)

(require #/for-syntax #/only-in lathe-comforts fn)

; NOTE: The Racket documentation says `get/build-late-neg-projection`
; is in `racket/contract/combinator`, but it isn't. It's in
; `racket/contract/base`. Since it's also in `racket/contract` and the
; documentation correctly says it is, we require it from there.
(require #/only-in racket/contract get/build-late-neg-projection)
(require #/only-in racket/contract/base
  contract? contract-name flat-contract?)
(require #/only-in racket/contract/combinator
  blame-add-context contract-first-order-passes? make-contract
  make-flat-contract raise-blame-error)
(require #/only-in racket/match define-match-expander)

(require #/only-in lathe-comforts dissect dissectfn expect fn mat w-)

(provide define-match-expander-attenuated match/c)



(define-syntax (define-match-expander-attenuated stx)
  (syntax-protect
  #/syntax-parse stx #/ (_ new-name:id old-name:id arg/c ...)
    
    #:declare arg/c (expr/c #'contract? #:name "an argument contract")
    
    #:with (arg/c-result ...) (generate-temporaries #'(arg/c ...))
    #:with (arg ...) (generate-temporaries #'(arg/c ...))
    #:with (arg.c ...)
    (for/list ([var (in-list (syntax->list #'(arg ...)))])
      (datum->syntax var
        (string->symbol (format "~a.c" (syntax-e var)))))
    #:with (arg-result ...) (generate-temporaries #'(arg/c ...))
    
    #'(begin
        
        (define arg/c-result arg/c.c)
        ...
        
        (define-match-expander new-name
          (fn stx
            ; TODO: We should really use a syntax class for match
            ; patterns rather than `expr` here, but it doesn't look
            ; like one exists yet.
            (syntax-protect #/syntax-parse stx #/ (_ arg ...)
              (~@ #:declare arg expr) ...
              #'(old-name arg ...)))
          (fn stx
            (syntax-protect #/syntax-parse stx
              
              ; NOTE: We intentionally do not allow the syntax to be
              ; used as a plain identifier. Even if we did, it
              ; wouldn't have the argument contracts enforced.
              ;
              ; TODO: Is there an easy way to enforce the argument
              ; contracts *and* have the procedure's name match this
              ; match expander's name *and* have contract violations
              ; reported in terms of the client's code locations
              ; (as with `contract-out`)? It looks like we'd have to
              ; dig into undocumented details about how
              ; `provide/contract-original-contract` is used. The
              ; documentation of this syntax is probably easier if it
              ; doesn't need to support plain identifier use anyway.
              ;
;              [_:id #'old-name]
              
              [ (_ arg ...)
                ; TODO: See if we can give this `expr/c` a better
                ; `#:name`.
                (~@ #:declare arg
                  (expr/c #'arg/c-result #:name "an argument"))
                ...
                #'(let ([arg-result arg.c] ...)
                    (old-name arg-result ...))]))))))


(define (match/c-impl foo-name foo->maybe-list list->foo args)
  (w- arg/cs
    (for/list ([arg (in-list args)])
      (dissect arg (list arg/c arg-blame-message)
        arg/c))
  #/w- name
    (list* 'match/c foo-name
    #/for/list ([arg/c (in-list arg/cs)])
      (contract-name arg/c))
  #/w- first-order
    (fn v
      (w- v-list (foo->maybe-list v)
      #/and v-list
      #/for/and
        ([arg/c (in-list arg/cs)] [v-arg (in-list v-list)])
        (contract-first-order-passes? arg/c v-arg)))
  #/if
    (for/and ([arg/c (in-list arg/cs)])
      (flat-contract? arg/c))
    (make-flat-contract #:name name #:first-order first-order)
  #/make-contract #:name name #:first-order first-order
    
    #:late-neg-projection
    (fn blame
      (w- arg-projections
        (for/list ([arg (in-list args)])
          (dissect arg (list arg/c arg-blame-message)
          #/(get/build-late-neg-projection arg/c)
            (blame-add-context blame arg-blame-message)))
      #/fn v missing-party
        (w- v-list (foo->maybe-list v)
        #/mat v-list #f
          (raise-blame-error blame #:missing-party missing-party v
            '(expected: "~e" given: "~e")
            name v)
        #/list->foo
          (for/list
            (
              [arg-late-neg-projection (in-list arg-projections)]
              [v-arg (in-list v-list)])
            (arg-late-neg-projection v-arg missing-party)))))))

(define-syntax (match/c stx)
  (syntax-protect #/syntax-parse stx #/ (_ name:id arg/c ...)
    ; TODO: See how well this `#:name` helps. It seems pretty vague.
    #:declare arg/c (expr/c #'contract? #:name "one of the arguments")
    #:with (arg ...) (generate-temporaries #'(arg/c ...))
    #`(match/c-impl
        'name
        (fn v
          (expect v (name arg ...) #f
          #/list arg ...))
        (dissectfn (list arg ...)
          (name arg ...))
        (list
          #,@(for/list
               (
                 [i (in-naturals)]
                 [arg/c (in-list (syntax->list #'(arg/c.c ...)))])
               #`(list #,arg/c #,(format "position ~a of" i)))))))
