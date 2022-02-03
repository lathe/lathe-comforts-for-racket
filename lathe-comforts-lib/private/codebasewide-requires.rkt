#lang parendown/slash reprovide

(for-syntax /combine-in/fallback
  (combine-in
    (only-in racket/format ~a)
    (only-in racket/list append* check-duplicates last range)
    (only-in racket/provide-transform
      make-provide-pre-transformer pre-expand-export)
    (only-in racket/struct-info extract-struct-info struct-info?)
    (only-in racket/syntax syntax-local-eval)
    (only-in racket/unit rename)
    (only-in syntax/contract wrap-expr/c)
    (only-in syntax/parse
      ...+ ~! ~and ~bind ~fail ~literal ~not ~optional ~or ~or* ~parse ~seq ~var attribute define-splicing-syntax-class define-syntax-class expr expr/c id keyword nat pattern syntax-parse this-syntax))
  racket/base)

(only-in racket/contract/base
  -> ->i </c and/c any any/c chaperone-contract? cons/c contract? contract-name contract-out flat-contract? get/build-late-neg-projection listof none/c or/c procedure-arity-includes/c recontract-out recursive-contract rename-contract unconstrained-domain->)
(only-in racket/contract/combinator
  blame-add-context coerce-chaperone-contract coerce-contract coerce-flat-contract contract-first-order-passes? make-chaperone-contract make-contract make-flat-contract raise-blame-error)
(only-in racket/contract/region invariant-assertion)
(only-in racket/list append-map range)
(only-in racket/match
  define-match-expander match match/derived match-lambda)
(only-in racket/math natural?)
(only-in racket/struct make-constructor-style-printer)
(only-in syntax/parse/define
  define-syntax-parser define-syntax-parse-rule)

; TODO SHIM: See if we should add `lathe-comforts/private`'s imports from
; `racket/parse`. to the shim. Our other modules don't depend on
; `syntax/parse`'s parsing framework at run time.
