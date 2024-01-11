#lang parendown/slash racket/base

; lathe-comforts/data-structure/finger-tree
;
; Finger trees, a catenable deque data structure with a pure API.

;   Copyright 2023-2024 The Lathe Authors
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


;(require /only-in racket/fixnum most-positive-fixnum)

(require lathe-comforts)
(require lathe-comforts/list)
(require lathe-comforts/match)
(require lathe-comforts/maybe)
(require lathe-comforts/struct)
(require lathe-comforts/trivial)

(require lathe-comforts/private/shim)
(init-shim)


(provide /own-contract-out
  summary-sys?
  summary-sys-impl?
  summary-sys-element/c
  summary-sys-summary/c
  summary-sys-element-summarize
  summary-sys-summary-zero
  summary-sys-summary-plus-two
  prop:summary-sys
  make-summary-sys-impl-from-plus-two
  summary-sys-summary-plus
  finger-tree?
  finger-tree-summary-sys
  finger-tree-push-first
  finger-tree-push-last
  bisection?
  bisection-before
  bisection-after)
(provide
  bisection)
(provide /own-contract-out
  finger-tree-pop-first-maybe
  finger-tree-pop-last-maybe
  finger-tree-first-maybe
  finger-tree-last-maybe
  finger-tree-append
  counting-summary-sys?)
(provide
  counting-summary-sys)
(provide /own-contract-out
  trivial-summary-sys?)
(provide
  trivial-summary-sys)
(provide /own-contract-out
  multiple-summary-sys?
  multiple-summary-sys-summary-sys-list)
(provide
  multiple-summary-sys)
(provide /own-contract-out
  finger-tree
  finger-tree->list
  finger-tree-length
  finger-tree-summarize
  finger-tree-ref-maybe
  finger-tree-split-at-maybe
  finger-tree-with-ordered-summaries-split)


; TODO: This was a small sketch at a way to implement counts that
; would increment in worst-case constant time, rather than the
; worst-caselogarithmic time and amortized constant time we would
; probably get from Racket numbers. Ah well, pushing elements onto our
; finger tree also takes worst-case logarithmic time and amortized
; constant time, so we've just used Racket numbers after all.
;
; The general approach we were trying to take here was to have a
; little-endian number where the big digits are lazy, just like our
; finger trees' `middle` is lazy now. As we just noted a second ago,
; this would still give us worst-case logarithmic time for
; incrementing and decrementing. We tried to sneak around that by
; keeping some nodes in between digits to represent carries that
; haven't been propagated yet, but since a sequence of increments
; would just bunch those nodes up, we would basically be counting in
; unary.
;
; I mean, unary *is* a way to get constant-time incrementing and
; decrementing. Maybe I really just want an efficient unary
; representation (e.g. a tally system that does bigint incrementing
; for a while until it hits a large but modest limit, say 1kb of
; bigint bits, then conses on another 1kb and spends a while filling
; that bigint up too).
;
; Perhaps what we could aim to accomplish by using lazy little-endian
; naturals like these is not to make incrementing or deprecating much
; cheaper, but to make *comparisons* take merely linear time in the
; number of bits of the smaller number (the smaller number usually
; being only a few bits long at most for our purposes).
;
; Anyway, the number of elements we have in any given finger tree is
; constrained by the size of memory, so this stuff isn't the kind of
; thing most programmers would even think twice about.
;
#|
(define fsize-threshold (most-positive-fixnum))
(struct fsize-zero ())
(struct fsize-fx-plus (addend original-promise))
(struct fsize-shift (quotient-promise))

(define (fsize+nat fs n)
  (mat n 0 fs
  /match fs
    [ (fsize-zero)
      (define-values (q r)
        (quotient/remainder n /add1 fsize-threshold))
      (fsize-fx-plus r
        (delay/strict /fsize/shift /delay /fsize+nat (fsize-zero) q))]
    [(fsize-fx-plus addend original-promise)
      (define-values (q r)
        (quotient/remainder (+ addend n) /add1 fsize-threshold))
      (

(define (nat->fsize n)
  (mat n 0 (fsize-zero)
  /begin
    (define-values (q r) (quotient/remainder n /add1 fsize-threshold))
  /mat q 0 (fsize-fx-plus r /delay/stricq)
  /fsize-shifting-add r (delay/strict /nat->fsize q)))

(define (fsize-add1 fs)
  (match fs
    [ (fsize-small n)
      (if (= fsize-threshold n)
        (fsize-shifting-add 0 /delay/strict /nat->fsize 1)
        (fsize-small /add1 n))]
    [ (fize-unshifting-add margin original-promise)
      (if (= fsize-threshold margin)
        (match (force original-promise)
          [(fsize-small original-n) (fsize-shifting-add original-n /delay/strict /nat->fsize 1)
        (fsize-shifting-add 0 /delay/strict /nat->fsize 1)
        (fsize-small (add1 margin original-promise))]
      (define-values (q r) (
    
  (dissect fs (fsize remainder quotient-promise-maybe)
  /if (= remainder fsize-threshold)
    (fsize 0
      (expect quotient-promise-maybe (just quotient-promise)
        (just /delay/strict /nat->fsize 1)
        (just /delay /fsize-add1 /force quotient-promise)))
  /fsize (add1 remainder) quotient-promise-maybe))
|#

(define-imitation-simple-generics summary-sys? summary-sys-impl?
  (#:method summary-sys-element/c (#:this))
  (#:method summary-sys-summary/c (#:this))
  (#:method summary-sys-element-summarize (#:this) ())
  (#:method summary-sys-summary-zero (#:this))
  (#:method summary-sys-summary-plus-two (#:this) () ())
  prop:summary-sys make-summary-sys-impl-from-plus-two
  'summary-sys 'summary-sys-impl (list))
(ascribe-own-contract summary-sys? (-> any/c boolean?))
(ascribe-own-contract summary-sys-impl? (-> any/c boolean?))
(ascribe-own-contract summary-sys-element/c
  (-> summary-sys? contract?))
(ascribe-own-contract summary-sys-summary/c
  (-> summary-sys? contract?))
(ascribe-own-contract summary-sys-element-summarize
  (->i ([ss summary-sys?] [e (ss) (summary-sys-element/c ss)])
    [_ (ss) (summary-sys-summary/c ss)]))
(ascribe-own-contract summary-sys-summary-zero
  (->i ([ss summary-sys?]) [_ (ss) (summary-sys-summary/c ss)]))
(ascribe-own-contract summary-sys-summary-plus-two
  (->i
    (
      [ss summary-sys?]
      [a (ss) (summary-sys-summary/c ss)]
      [b (ss) (summary-sys-summary/c ss)])
    [_ (ss) (summary-sys-summary/c ss)]))
(ascribe-own-contract prop:summary-sys
  (struct-type-property/c summary-sys-impl?))
(ascribe-own-contract make-summary-sys-impl-from-plus-two
  (->
    #;element/c
    (-> summary-sys? contract?)
    #;summary/c
    (-> summary-sys? contract?)
    #;element-summarize
    (->i ([ss summary-sys?] [e (ss) (summary-sys-element/c ss)])
      [_ (ss) (summary-sys-summary/c ss)])
    #;summary-zero
    (->i ([ss summary-sys?]) [_ (ss) (summary-sys-summary/c ss)])
    #;summary-plus-two
    (->i
      (
        [ss summary-sys?]
        [a (ss) (summary-sys-summary/c ss)]
        [b (ss) (summary-sys-summary/c ss)])
      [_ (ss) (summary-sys-summary/c ss)])
    summary-sys-impl?))

; TODO: See if we want to use this.
#;
(struct makeshift-summary-sys-rep
  (
    element/c
    summary/c
    element-summarize
    summary-zero
    summary-plus-two)
  
  #:methods gen:equal-mode+hash
  [
    (define (equal-mode-proc a b recur now?)
      (dissect a
        (makeshift-summary-sys-rep
          a-element/c
          a-summary/c
          a-element-summarize
          a-summary-zero
          a-summary-plus-two)
      /dissect b
        (makeshift-summary-sys-rep
          b-element/c
          b-summary/c
          b-element-summarize
          b-summary-zero
          b-summary-plus-two)
      /and
        (equal-always? a-element/c b-element/c)
        (equal-always? a-summary/c b-summary/c)
        (equal-always? a-element-summarize b-element-summarize)
        (equal-always? a-summary-zero b-summary-zero)
        (equal-always? a-summary-plus-two b-summary-plus-two)))
    
    (define (hash-mode-proc v recur now?)
      (dissect v
        (makeshift-summary-sys-rep
          element/c
          summary/c
          element-summarize
          summary-zero
          summary-plus-two)
      /hash-code-combine
        (eq-hash-code struct:makeshift-summary-sys-rep)
        (equal-always-hash-code element/c)
        (equal-always-hash-code summary/c)
        (equal-always-hash-code element-summarize)
        (equal-always-hash-code summary-zero)
        (equal-always-hash-code summary-plus-two)))
    
    ]
  
  #:property prop:summary-sys
  (make-summary-sys-impl-from-plus-two
    (fn ss
      ((makeshift-summary-sys-rep-element/c ss)))
    (fn ss
      ((makeshift-summary-sys-rep-summary/c ss)))
    (fn ss e
      ((makeshift-summary-sys-rep-element-summarize ss) e))
    (fn ss
      ((makeshift-summary-sys-rep-summary-zero ss)))
    (fn ss a b
      ((makeshift-summary-sys-rep-summary-plus-two ss) a b))))

; TODO: See if we want to use this. If we do, use keyword arguments
; instead of positional ones.
#;
(define
  (makeshift-summary-sys
    element/c
    summary/c
    element-summarize
    summary-zero
    summary-plus-two)
  (makeshift-summary-sys-rep
    element/c
    summary/c
    element-summarize
    summary-zero
    summary-plus-two))

(define/own-contract (summary-sys-summary-plus ss . sums)
  (->i ([ss summary-sys?])
    #:rest [sums (ss) (listof /summary-sys-summary/c ss)]
    [_ (ss) (summary-sys-summary/c ss)])
  (expect sums (cons sum sums) (summary-sys-summary-zero ss)
  /list-foldl sum sums /fn state sum
    (summary-sys-summary-plus-two ss state sum)))

(struct summarized (summary element))

(struct ftree-0 ())
(struct ftree-1 (element))
(struct ftree-2+ (first-finger middle-nodes-promise last-finger))
(struct node-2 (element-0 element-1))
(struct node-3 (element-0 element-1 element-2))
(struct finger-1 (element-0))
(struct finger-2 (element-0 element-1))
(struct finger-3 (element-0 element-1 element-2))
(struct finger-4 (element-0 element-1 element-2 element-3))
(define-imitation-simple-struct
  (finger-tree? finger-tree-summary-sys finger-tree-ftree)
  finger-tree-rep
  'finger-tree (current-inspector)
  
  (#:gen gen:equal-mode+hash
    
    (define (equal-mode-proc a b recur now?)
      (dissect a (finger-tree-rep a-summary-sys a-ftree)
      /dissect b (finger-tree-rep b-summary-sys b-ftree)
      /and
        (equal-always? a-summary-sys b-summary-sys)
        (equal-always?/recur
          (finger-tree->list a)
          (finger-tree->list b)
          recur)))
    
    (define (hash-mode-proc v recur now?)
      (dissect v (finger-tree-rep summary-sys ftree)
      /hash-code-combine
        (eq-hash-code finger-tree?)
        (equal-always-hash-code summary-sys)
        (equal-always-hash-code/recur (finger-tree->list v) recur)))
    
    )
  
  (#:gen gen:custom-write
    
    (define write-proc
      (make-constructor-style-printer
        (fn this 'finger-tree)
        (fn this
          (dissect this (finger-tree-rep ss ftree)
          /w- this-list (finger-tree->list this)
          /dissect ss
            (multiple-summary-sys /list (counting-summary-sys) ss)
            `(
               ,@(if (equal-always? (trivial-summary-sys) ss)
                   '()
                   `(#:summary-sys ,ss))
               ,@this-list)))))
    
    ))
(ascribe-own-contract finger-tree? (-> any/c boolean?))
(ascribe-own-contract finger-tree-summary-sys
  (-> finger-tree? summary-sys?))

(define (summarized-node-2 ss a b)
  (dissect a (summarized a-sum a)
  /dissect b (summarized b-sum b)
  /summarized (summary-sys-summary-plus ss a-sum b-sum) /node-2 a b))
(define (summarized-node-3 ss a b c)
  (dissect a (summarized a-sum a)
  /dissect b (summarized b-sum b)
  /dissect c (summarized c-sum c)
  /summarized (summary-sys-summary-plus ss a-sum b-sum c-sum)
    (node-3 a b c)))

; TODO: It turns out we don't need this. See if we want to delete it.
#;
(define (node/c element/c)
  (w- c (coerce-contract 'node/c element/c)
  /rename-contract
    (or/c
      (match/c node-2 element/c element/c)
      (match/c node-3 element/c element/c element/c)))
    `(node/c ,(contract-name element/c)))

; TODO: It turns out we didn't use this. In order to avoid the cost of
; traversing several wrappers of this on every call, we just built our
; `middle` `ftree`s directly using `summarized-node-2` and
; `summarized-node-3`. See if we want to delete this.
#;
(struct node-summary-sys (element-ss)
  #:property prop:summary-sys
  (make-summary-sys-impl-from-plus-two
    #;element/c
    (fn ss
      (node/c /summary-sys-element/c element-ss))
    #;summary/c
    (fn ss
      (summary-sys-summary/c element-ss))
    #;element-summarize
    (fn ss e
      (dissect ss (node-summary-sys element-ss)
      /match e
        [ (node-2 (summarized a-sum a) (summarized b-sum b))
          (summary-sys-summary-plus element-ss a-sum b-sum)]
        [
          (node-3
            (summarized a-sum a)
            (summarized b-sum b)
            (summarized c-sum c))
          (summary-sys-summary-plus element-ss a-sum b-sum c-sum)]))
    #;summary-zero
    (fn ss
      (dissect ss (node-summary-sys element-ss)
      /summary-sys-summary-zero element-ss))
    #;summary-plus-two
    (fn ss a b
      (dissect ss (node-summary-sys element-ss)
      /summary-sys-summary-plus-two element-ss a b))))

(define (in-finger-tree ft body)
  (dissect ft (finger-tree-rep ss ft)
  /finger-tree-rep ss /body ss ft))

(define (node->list n)
  (match n
    [(node-2 a b) (list a b)]
    [(node-3 a b c) (list a b c)]))

(define (finger->list f)
  (match f
    [(finger-1 a) (list a)]
    [(finger-2 a b) (list a b)]
    [(finger-3 a b c) (list a b c)]
    [(finger-4 a b c d) (list a b c d)]))

(define (list->finger f)
  (match f
    [(list a) (finger-1 a)]
    [(list a b) (finger-2 a b)]
    [(list a b c) (finger-3 a b c)]
    [(list a b c d) (finger-4 a b c d)]))

(define (ftree-push-first ss e ft)
  (match ft
    [(ftree-0) (ftree-1 e)]
    [ (ftree-1 a)
      (ftree-2+ (finger-1 e) (delay/strict /ftree-0) (finger-1 a))]
    [ (ftree-2+ first middle last)
      (match first
        [ (finger-4 a b c d)
          (ftree-2+
            (finger-2 e a)
            (delay/strict
              (ftree-push-first ss
                (summarized-node-3 ss b c d)
                (force middle)))
            last)]
        [ first
          (ftree-2+
            (list->finger (cons e (finger->list first)))
            middle
            last)])]))

(define/own-contract (finger-tree-push-first e ft)
  ; TODO PRECISE: See if we want to check that `e` is a
  ; `summary-sys-element/c`.
  (-> any/c finger-tree? finger-tree?)
  (in-finger-tree ft /fn ss ft
  /ftree-push-first ss
    (summarized (summary-sys-element-summarize e) e)
    ft))

(define (ftree-push-last ss ft e)
  (match ft
    [(ftree-0) (ftree-1 e)]
    [ (ftree-1 a)
      (ftree-2+ (finger-1 a) (delay/strict /ftree-0) (finger-1 e))]
    [ (ftree-2+ first middle last)
      (match last
        [ (finger-4 a b c d)
          (ftree-2+
            first
            (delay/strict
              (ftree-push-last ss
                (force middle)
                (summarized-node-3 ss a b c)))
            (finger-2 d e))]
        [ last
          (ftree-2+
            first
            middle
            (list->finger (append (finger->list last) (list e))))])]))

(define/own-contract (finger-tree-push-last ft e)
  ; TODO PRECISE: See if we want to check that `e` is a
  ; `summary-sys-element/c`.
  (-> finger-tree? any/c finger-tree?)
  (in-finger-tree ft /fn ss ft
  /ftree-push-last ss
    ft
    (summarized (summary-sys-element-summarize e) e)))

(define-imitation-simple-struct
  (bisection? bisection-before bisection-after)
  bisection
  'bisection (current-inspector) (auto-write) (auto-equal))
(ascribe-own-contract bisection? (-> any/c boolean?))
(ascribe-own-contract bisection-before (-> bisection? any/c))
(ascribe-own-contract bisection-after (-> bisection any/c))

(define-imitation-simple-struct
  (trisection? trisection-before trisection-during trisection-after)
  trisection
  'trisection (current-inspector) (auto-write) (auto-equal))

(define (finger->ftree f)
  (match f
    [(finger-1 a) (ftree-1 a)]
    [ (finger-2 a b)
      (ftree-2+ (finger-1 a) (delay/strict /ftree-0) (finger-1 b))]
    [ (finger-3 a b c)
      (ftree-2+ (finger-1 a b) (delay/strict /ftree-0) (finger-1 c))]
    [ (finger-4 a b c d)
      (ftree-2+ (finger-1 a b) (delay/strict /ftree-0) (finger-1 c d))]
    ))

(define (node-ftree-and-finger->ftree ss middle last)
  (mat (ftree-pop-first ss middle) (just /bisection node middle)
    (ftree-2+
      (list->finger /node->list node)
      (delay/strict middle)
      last)
    (finger->ftree last)))

(define
  (short-list-and-node-ftree-promise-and-finger->ftree
    ss first middle last)
  (mat first (list)
    (node-ftree-and-finger->ftree ss (force middle) last)
    (ftree-2+ (list->finger first) middle last)))

(define (ftree-pop-first ss ft)
  (match ft
    [(ftree-0) (nothing)]
    [(ftree-1 a) (just /bisection a (ftree-0))]
    [ (ftree-2+ first middle last)
      (dissect (finger->list first) (cons a first)
      /just /bisection
        a
        (short-list-and-node-ftree-promise-and-finger->ftree ss
          first middle last))]))

(define/own-contract (finger-tree-pop-first-maybe ft)
  (-> finger-tree? /maybe/c /match/c bisection any/c finger-tree?)
  (dissect ft (finger-tree-rep ss ft)
  /maybe-if (ftree-pop-first ss ft)
    (dissectfn (bisection (summarized a-sum a) rest)
      (bisection a rest))))

(define (finger-and-node-ftree->ftree ss first middle)
  (mat (ftree-pop-last ss middle) (just /bisection middle node)
    (ftree-2+
      first
      (delay/strict middle)
      (list->finger /node->list node))
    (finger->ftree first)))

(define
  (finger-and-node-ftree-promise-and-short-list->ftree
    ss first middle last)
  (mat last (list)
    (finger-and-node-ftree->ftree ss first (force middle))
    (ftree-2+ first middle (list->finger last))))

(define (ftree-pop-last ss ft)
  (match ft
    [(ftree-0) (nothing)]
    [(ftree-1 a) (just /bisection (ftree-0) a)]
    [ (ftree-2+ first middle last)
      (dissect (finger->list last) (list last ... a)
      /just /bisection
        (finger-and-node-ftree-promise-and-short-list->ftree ss
          first middle last)
        a)]))

(define/own-contract (finger-tree-pop-last-maybe ft)
  (-> finger-tree? /maybe/c /match/c bisection finger-tree? any/c)
  (dissect ft (finger-tree-rep ss ft)
  /maybe-if (ftree-pop-last ss ft)
    (dissectfn (bisection rest (summarized a-sum a))
      (bisection rest a))))

(define (ftree-first-maybe ss ft)
  (match ft
    [(ftree-0) (nothing)]
    [(ftree-1 a) (just a)]
    [ (ftree-2+ first-finger middle last-finger)
      (just /first /finger->list first-finger)]))

(define/own-contract (finger-tree-first-maybe ft)
  (-> finger-tree? maybe?)
  (dissect ft (finger-tree-rep ss ft)
  /maybe-if (ftree-pop-first ss ft) /dissectfn (summarized a-sum a)
    a))

(define (ftree-last-maybe ss ft)
  (match ft
    [(ftree-0) (nothing)]
    [(ftree-1 a) (just a)]
    [ (ftree-2+ first-finger middle last-finger)
      (just /last /finger->list last-finger)]))

(define/own-contract (finger-tree-last-maybe ft)
  (-> finger-tree? maybe?)
  (dissect ft (finger-tree-rep ss ft)
  /maybe-if (ftree-pop-last ss ft) /dissectfn (summarized a-sum a)
    a))

(define (ftree-append ss ft-a ft-b)
  (match ft-a
    [(ftree-0) ft-b]
    [(ftree-1 a) (ftree-push-first ss a ft-b)]
  / (ftree-2+ first a-middle a-last)
  /match ft-b
    [(ftree-0) ft-a]
    [(ftree-1 b) (ftree-push-last ss ft-a b)]
  / (ftree-2+ b-first b-middle last)
  /ftree-2+
    first
    (delay /match
      (append (finger->list a-last) (finger->list b-first))
      [ (list a b)
        (ftree-append ss
          a-middle
          (ftree-push-first ss (summarized-node-2 ss a b) b-middle))]
      [ (list a b c)
        (ftree-append ss
          a-middle
          (ftree-push-first ss
            (summarized-node-3 ss a b c)
            b-middle))]
      [ (list a b c d)
        (ftree-append ss
          (ftree-push-last ss a-middle (summarized-node-2 ss a b))
          (ftree-push-first ss (summarized-node-2 ss c d) b-middle))]
      [ (list a b c d e)
        (ftree-append ss
          (ftree-push-last ss a-middle (summarized-node-2 ss a b))
          (ftree-push-first ss
            (summarized-node-3 ss c d e)
            b-middle))])
    last))

(define/own-contract (finger-tree-append ft-a ft-b)
  ; TODO PRECISE: See if we want to check that `ft-a` and `ft-b` have
  ; the same `finger-tree-summary-sys` as part of the contract.
  (-> finger-tree? finger-tree? finger-tree?)
  (in-finger-tree ft-a /fn a-ss ft-a
  /dissect ft-b (finger-tree-rep b-ss ft-b)
  /expect (equal-always? a-ss b-ss) #t
    ; TODO: Improve this error.
    (error "expected two finger trees with the same summary system")
  /ftree-append a-ss ft-a ft-b))

(define (finger-summarize ss finger)
  (apply summary-sys-summary-plus ss
    (list-map finger /dissectfn (summarized sum element) sum)))

(define (ftree-summarize ss ft)
  (match ft
    [(ftree-0) (summary-sys-summary-zero ss)]
    [(ftree-1 /summarized a-sum a) a-sum]
    [ (ftree-2+ first middle last)
      (summary-sys-summary-plus ss
        (finger-summarize ss first)
        (ftree-summarize ss /force middle)
        (finger-summarize ss last))]))

(define-imitation-simple-struct
  (counting-summary-sys?) counting-summary-sys
  'counting-summary-sys (current-inspector) (auto-write) (auto-equal)
  (#:prop prop:summary-sys /make-summary-sys-impl-from-plus-two
    #;element/c
    (fn ss any/c)
    #;summary/c
    (fn ss natural?)
    #;element-summarize
    (fn ss e 1)
    #;summary-zero
    (fn ss 0)
    #;summary-plus-two
    (fn ss a b /+ a b)))
(ascribe-own-contract counting-summary-sys? (-> any/c boolean?))

(define-imitation-simple-struct
  (trivial-summary-sys?) trivial-summary-sys
  'trivial-summary-sys (current-inspector) (auto-write) (auto-equal)
  (#:prop prop:summary-sys /make-summary-sys-impl-from-plus-two
    #;element/c
    (fn ss any/c)
    #;summary/c
    (fn ss trivial?)
    #;element-summarize
    (fn ss e /trivial)
    #;summary-zero
    (fn ss /trivial)
    #;summary-plus-two
    (fn ss a b /trivial)))
(ascribe-own-contract trivial-summary-sys? (-> any/c boolean?))

(define-imitation-simple-struct
  (multiple-summary-sys? multiple-summary-sys-summary-sys-list)
  unguarded-multiple-summary-sys
  'multiple-summary-sys (current-inspector) (auto-write) (auto-equal)
  (#:prop prop:summary-sys /make-summary-sys-impl-from-plus-two
    #;element/c
    (dissectfn (multiple-summary-sys ss-list)
      (list-foldl any/c ss-list /fn element/c ss
        (and/c element/c (summary-sys-element/c ss))))
    #;summary/c
    (dissectfn (multiple-summary-sys ss-list)
      (apply list/c
        (list-map ss-list /fn ss /summary-sys-element/c ss)))
    #;element-summarize
    (fn ss e
      (dissect ss (multiple-summary-sys ss-list)
        (list-map ss-list /fn ss
          (summary-sys-element-summarize ss e))))
    #;summary-zero
    (fn ss e
      (dissect ss (multiple-summary-sys ss-list)
        (list-map ss-list /fn ss /summary-sys-summary-zero ss)))
    #;summary-plus-two
    (fn ss a b
      (dissect ss (multiple-summary-sys ss-list)
        (for/list
          ([ss (in-list ss-list)] [a (in-list a)] [b (in-list b)])
          (summary-sys-summary-plus-two ss a b))))))
(ascribe-own-contract multiple-summary-sys? (-> any/c boolean?))
(ascribe-own-contract multiple-summary-sys-summary-sys-list
  (-> multiple-summary-sys? /listof summary-sys?))
(define-match-expander-attenuated
  attenuated-multiple-summary-sys
  unguarded-multiple-summary-sys
  [summary-sys-list (listof summary-sys?)]
  #t)
(define-match-expander-from-match-and-make
  multiple-summary-sys
  unguarded-multiple-summary-sys
  attenuated-multiple-summary-sys
  attenuated-multiple-summary-sys)

(define (list->ftree ss elements)
  (list-foldl (ftree-0) elements /fn ft e /ftree-push-last ss ft e))

(define/own-contract
  (finger-tree #:summary-sys [ss (trivial-summary-sys)] . elements)
  (->i () (#:summary-sys [ss summary-sys?])
    #:rest
    [elements (ss)
      (if (unsupplied-arg? ss)
        any/c
        (listof /summary-sys-element/c ss))]
    [_ finger-tree?])
  (finger-tree-rep
    (multiple-summary-sys /list (counting-summary-sys) ss)
    (list->ftree ss
      (list-map elements /fn e
        (summarized (summary-sys-element-summarize ss e) e)))))

(define (finger-tree-summarize-internal ft)
  (dissect ft (finger-tree-rep ss ft)
  /ftree-summarize ss ft))

(define/own-contract (finger-tree->list ft)
  (-> finger-tree? list?)
  (w-loop next result (list) ft ft
    (expect (finger-tree-pop-last-maybe ft)
      (just /bisection ft popped)
      result
    /next (cons popped result) ft)))

(define/own-contract (finger-tree-length ft)
  (-> finger-tree? natural?)
  (dissect (finger-tree-summarize-internal ft) (list n summary)
    n))

(define/own-contract (finger-tree-summarize ft)
  ; TODO PRECISE: See if we want to promise that the result is a
  ; `summary-sys-summary/c`.
  (-> finger-tree? any/c)
  (dissect (finger-tree-summarize-internal ft) (list n summary)
    summary))

; TODO: Change the `egress` type to be written `(ie essence manner)`.
; Boolie, andy, ory

(define (egress-veer apology)
  (list #f apology))

(define (egress-done result)
  (list #t result))

(define (egress-veer-apology-maybe e)
  (match e
    [(list #f apology) (just apology)]
    [(list #t result) (nothing)]))

(define (egress-catch-bind e on-apology then)
  (match e
    [(list #f apology) (on-apology apology)]
    [(list #t result) (then result)]))

(define (egress-divert-bind e apology-divert then)
  (egress-catch-bind e
    (fn apology /egress-veer /apology-divert apology)
    then))

(define (egress-catch e on-apology)
  (egress-catch-bind e on-apology /fn result /egress-done result))

(define (egress-divert e apology-divert)
  (egress-divert-bind e apology-divert /fn result
  /egress-done result))

(define (egress-bind e then)
  (egress-divert-bind e (fn apology apology) then))

(define
  (list-of-measured-ref/remainder lst i element-ref/remainder)
  (expect lst (cons elem lst) (egress-done i)
  /egress-bind (element-ref/remainder elem i) /fn i
  /list-of-measured-ref/remainder lst i element-ref/remainder))

(define (finger-ref/remainder f i element-ref/remainder)
  (list-of-measured-ref/remainder (finger->list f) i
    element-ref/remainder))

(define (node-ref/remainder node i element-ref/remainder)
  (list-of-measured-ref/remainder (node->list node) i
    element-ref/remainder))

(define (summarized-ref/remainder s i element-ref/remainder)
  (dissect s (summarized (list n sum) e)
  /w- new-i (- i n)
  /if (negative? new-i)
    (element-ref/remainder e i)
    (egress-done new-i)))

(define (ftree-ref/remainder ft i element-ref/remainder)
  (match ft
    [(ftree-0) (egress-done i)]
    [(ftree-1 a) (element-ref/remainder a i)]
    [ (ftree-2+ first middle last)
      (egress-bind (finger-ref/remainder first i) /fn i
      /egress-bind
        (ftree-ref/remainder (force middle) i /fn node i
          (summarized-ref/remainder node i /fn node i
            (node-ref/remainder node i element-ref/remainder)))
      /fn i
      /finger-ref/remainder last i)]))

(define (finger-tree-ref/remainder ft i)
  (dissect ft (finger-tree-rep ss ft)
  /ftree-ref/remainder ft i /fn element i
    (summarized-ref/remainder element i /fn element i
      (egress-veer element))))

(define/own-contract (finger-tree-ref-maybe ft i)
  (-> finger-tree? natural? maybe?)
  (egress-veer-apology-maybe /finger-tree-ref/remainder ft i))

(define
  (list-of-measured-split-at/remainder
    lst i element-split-at/remainder)
  (w-loop next rev-before (list) after lst i i
    (expect after (cons elem after) (egress-done i)
    /egress-divert-bind (element-split-at/remainder elem i)
      (fn elem-pieces /trisection rev-before elem-pieces after)
    /fn i
    /next (cons elem rev-before) after i)))

(define (summarized-split-at/remainder s i element-split-at/remainder)
  (dissect s (summarized (list n sum) e)
  /w- new-i (- i n)
  /if (negative? new-i)
    (element-split-at/remainder e i)
    (egress-done new-i)))

(define (ftree-split-at/remainder ss ft i element-split-at/remainder)
  (match ft
    [(ftree-0) (egress-done i)]
    [ (ftree-1 a)
      (egress-divert (element-split-at/remainder a i) /fn a-pieces
        (trisection (ftree-0) a-pieces (ftree-0)))]
    [ (ftree-2+ first middle last)
      (egress-divert-bind
        (list-of-measured-split-at/remainder (finger->list first) i
          element-split-at/remainder)
        (dissectfn (trisection rev-before element-pieces after)
          (trisection
            (list->ftree ss /reverse rev-before)
            element-pieces
            (short-list-and-node-ftree-promise-and-finger->ftree ss
              after middle last)))
      /fn i
      /egress-divert-bind
        (ftree-split-at/remainder ss (force middle) i /fn node i
          (summarized-split-at/remainder node i /fn node i
            (list-of-measured-split-at/remainder (node->list node) i
              element-split-at/remainder)))
        (dissectfn
          (trisection
            before
            (trisection rev-node-before element-pieces node-after)
            after)
          (trisection
            (finger-and-node-ftree-promise-and-short-list->ftree ss
              first (delay/strict before) (reverse rev-node-before))
            element-pieces
            (short-list-and-node-ftree-promise-and-finger->ftree ss
              node-after (delay/strict after) last)))
      /fn i
      /egress-divert
        (list-of-measured-split-at/remainder (finger->list last) i
          element-split-at/remainder)
        (dissectfn (trisection rev-before element-pieces after)
          (trisection
            (short-list-and-node-ftree-promise-and-finger->ftree ss
              first middle (reverse rev-before))
            element-pieces
            (list->ftree ss after))))]))

(define (finger-tree-split-at/remainder ft i)
  (dissect ft (finger-tree-rep ss ft)
  /egress-divert
    (ftree-split-at/remainder ft i /fn element i
      (summarized-split-at/remainder element i /fn element i
        (egress-veer element)))
    (dissectfn (trisection before element after)
      (bisection
        (finger-tree-rep ss before)
        (finger-tree-push-first element /finger-tree-rep ss after)))))

(define/own-contract (finger-tree-split-at-maybe ft i)
  (-> finger-tree? natural?
    (maybe/c /match/c bisection finger-tree? finger-tree?))
  (w- ss (finger-tree-summary-sys ft)
  /egress-veer-apology-maybe
    (egress-bind (finger-tree-ref/remainder ft i) /fn i
    ; If we've tried to split right at the end of the finger tree,
    ; `finger-tree-split-at/remainder` counts that as a failure, but
    ; `finger-tree-split-at-maybe` does not.
    /mat i 0 (bisection ft (finger-tree #:summary-sys ss))
    /egress-done i)))


(define (list-with-ordered-summaries-split lst element-split)
  (w-loop next rev-before (list) after lst
    (expect after (cons elem after)
      (trisection rev-before (bisection (nothing) (nothing)) (list))
    /dissect (element-split elem) (bisection e-before e-after)
    /expect e-after (nothing)
      (trisection rev-before (bisection e-before e-after) after)
    /next (cons elem rev-before) after)))

(define
  (ftree-with-ordered-summaries-split
    ss ft summarized-split element-split)
  (w- summarized-element-split
    (fn a /summarized-split a element-split)
  /match ft
    [ (ftree-0)
      (trisection
        (ftree-0)
        (bisection (nothing) (nothing))
        (ftree-0))]
    [ (ftree-1 a)
      (trisection (ftree-0) (summarized-element-split a) (ftree-0))]
    [ (ftree-2+ first middle last)
      (dissect
        (list-with-ordered-summaries-split (finger->list first)
          summarized-element-split)
        (trisection
          rev-first-before
          (bisection first-element-before first-element-after)
          first-after)
      /expect (bisection first-element-after first-after)
        (bisection (nothing) (list))
        (trisection
          (list->ftree ss /reverse rev-first-before)
          (bisection first-element-before first-element-after)
          (short-list-and-node-ftree-promise-and-finger->ftree ss
            first-after middle last))
      /dissect
        (list-with-ordered-summaries-split (finger->list last)
          summarized-element-split)
        (trisection
          rev-last-before
          (bisection last-element-before last-element-after)
          last-after)
      /expect (bisection rev-last-before last-element-before)
        (bisection (list) (nothing))
        (trisection
          (finger-and-node-ftree-promise-and-short-list->ftree ss
            first middle rev-last-before)
          (bisection last-element-before last-element-after)
          (list->ftree ss last-after))
      /dissect
        (ftree-with-ordered-summaries-split ss (force middle)
          summarized-split
          (fn sum node
            ; NOTE: We ignore `sum` here because the elements of a
            ; node are already `summarized` values in their own right.
            (list-with-ordered-summaries-split (node->list node)
              summarized-element-split)))
        (trisection
          middle-before
          (trisection rev-node-before element-pieces node-after)
          middle-after)
        (trisection
          (finger-and-node-ftree-promise-and-short-list->ftree ss
            first
            (delay/strict middle-before)
            (reverse rev-node-before))
          element-pieces
          (short-list-and-node-ftree-promise-and-finger->ftree ss
            node-after
            (delay/strict middle-after)
            last)))]))

(define/own-contract
  (finger-tree-with-ordered-summaries-split ft summary<i?)
  (->i
    (
      [ft finger-tree?]
      [ summary<i? (ft)
        (-> (summary-sys-summary/c /finger-tree-summary-sys ft)
          boolean?)])
    [_ (match/c bisection finger-tree? finger-tree?)])
  (dissect ft (finger-tree-rep ss ft)
  /dissect
    (ftree-with-ordered-summaries-split ss ft
      
      ; The `summary<i?` predicate gives us a notion that our
      ; summarized leaves and nodes are separated into a set that are
      ; early enough and a set that aren't. Some nodes are parents to
      ; nodes or leaves of both sets. We consider these nodes *not* to
      ; be among the set of nodes that are early enough, and we expect
      ; them not to pass the predicate. This means we drill down to
      ; leafward nodes and leaves whenever we find ourselves at our
      ; current level's first node that doesn't pass the predicate.
      ;
      (fn s element-split
        (dissect s (summarized sum e)
        /if (summary<i? sum)
          (bisection (just s) (nothing))
          (element-split sum e)))
      
      ; Once we get all the way down to a leaf, it doesn't have any
      ; leafward nodes to check, so the whole thing is just in the
      ; "not early enough" set.
      ;
      (fn sum e
        (bisection (nothing) (just /summarized sum e))))
    (trisection before (bisection e-before e-after) after)
  /bisection
    (finger-tree-rep ss
      (expect e-before (just e-before) before
        (ftree-push-first ss e-before before)))
    (finger-tree-rep ss
      (expect e-after (just e-after) after
        (ftree-push-last ss after e-after)))))
