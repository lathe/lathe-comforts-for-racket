#lang parendown racket/base

; lathe-comforts/list
;
; Utilities for lists and natural numbers.

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


(require #/only-in racket/contract/base -> any any/c contract-out)
(require #/only-in racket/list append-map range)
(require #/only-in racket/math natural?)

(require #/only-in lathe-comforts expect fn w-loop)
(require #/only-in lathe-comforts/maybe just maybe/c nothing)

(provide #/contract-out
  
  
  ; Utilities for natural numbers
  
  [nat->maybe (-> natural? #/maybe/c natural?)]
  
  
  ; Utilities for lists
  
  [list-foldl (-> any/c list? (-> any/c any/c any/c) any/c)]
  [list-foldr (-> list? any/c (-> any/c any/c any/c) any/c)]
  
  [list-bind (-> list? (-> any/c list?) list?)]
  
  [list-map (-> list? (-> any/c any/c) list?)]
  [list-any (-> list? (-> any/c any) any)]
  [list-all (-> list? (-> any/c any) any)]
  [list-each (-> list? (-> any/c any) void?)]
  
  [list-kv-map (-> list? (-> natural? any/c any/c) list?)]
  [list-kv-any (-> list? (-> natural? any/c any) any)]
  [list-kv-all (-> list? (-> natural? any/c any) any)]
  [list-kv-each (-> list? (-> natural? any/c any) void?)]
  
  [list-zip-map (-> list? list? (-> any/c any/c any/c) list?)]
  [list-zip-any (-> list? list? (-> any/c any/c any) any)]
  [list-zip-all (-> list? list? (-> any/c any/c any) any)]
  [list-zip-each (-> list? list? (-> any/c any/c any) void?)]
  
  
  ; Utilities for natural numbers and lists together
  [list-length<=nat? (-> list? natural? boolean?)]
  [nat<=list-length? (-> natural? list? boolean?)]
  
  [list-length=nat? (-> list? natural? boolean?)]
  
  [list-length<nat? (-> list? natural? boolean?)]
  [nat<list-length? (-> natural? list? boolean?)]
  
  
)



; ===== Utilities for natural numbers ================================

(define (nat->maybe n)
  (if (= n 0)
    (nothing)
    (just #/sub1 n)))


; ===== Utilities for lists ==========================================

(define (list-foldl state lst func)
  (foldl (fn elem state #/func state elem) state lst))

(define (list-foldr lst state func)
  (foldr (fn elem state #/func elem state) state lst))

(define (list-bind lst func)
  (append-map func lst))

(define (list-map lst func)
  (map func lst))

(define (list-any lst func)
  (ormap func lst))

(define (list-all lst func)
  (andmap func lst))

(define (list-each lst body)
  (for-each body lst))

(define (list-kv-map lst func)
  (map func (range #/length lst) lst))

(define (list-kv-any lst func)
  (expect lst (cons v lst) #f
  #/w-loop next k 0 v v lst lst
    
    ; If this is the last element of the list, we do a tail call. This
    ; is like the way `ormap` does a tail call. We could use `ormap`
    ; itself with `(range #/length lst)`, but this way we only compute
    ; the indexes we need.
    (expect lst (cons new-v lst) (func k v)
    
    #/or (func k v)
    #/next (add1 k) new-v lst)))

(define (list-kv-all lst func)
  (expect lst (cons v lst) #t
  #/w-loop next k 0 v v lst lst
    
    ; If this is the last element of the list, we do a tail call. This
    ; is like the way `andmap` does a tail call. We could use `andmap`
    ; itself with `(range #/length lst)`, but this way we only compute
    ; the indexes we need.
    (expect lst (cons new-v lst) (func k v)
    
    #/and (func k v)
    #/next (add1 k) new-v lst)))

(define (list-kv-each lst body)
  (for-each body (range #/length lst) lst))

(define (list-zip-map a b func)
  (map func a b))

(define (list-zip-any a b func)
  (ormap func a b))

(define (list-zip-all a b func)
  (andmap func a b))

(define (list-zip-each a b body)
  (for-each body a b))


; ===== Utilities for natural numbers and lists together =============

(define (list-length<=nat? lst n)
  (expect lst (cons _ lst) #t
  #/expect (nat->maybe n) (just n) #f
  #/list-length<=nat? lst n))

(define (nat<=list-length? n lst)
  (expect (nat->maybe n) (just n) #t
  #/expect lst (cons _ lst) #f
  #/nat<=list-length? n lst))

(define (list-length=nat? lst n)
  (expect lst (cons _ lst) (= 0 n)
  #/expect (nat->maybe n) (just n) #f
  #/list-length=nat? lst n))

(define (list-length<nat? lst n)
  (not #/nat<=list-length? n lst))

(define (nat<list-length? n lst)
  (not #/list-length<=nat? lst n))
