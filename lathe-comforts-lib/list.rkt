#lang parendown racket/base

; lathe-comforts/list
;
; Utilities for lists and natural numbers.

;   Copyright 2017-2018, 2022, 2024 The Lathe Authors
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

(require #/only-in lathe-comforts expect fn w-loop)
(require #/only-in lathe-comforts/maybe just maybe? maybe/c nothing)

(provide #/own-contract-out
  
  
  ; Utilities for natural numbers
  
  nat->maybe
  
  
  ; Utilities for lists
  
  list-foldl
  list-foldr
  
  list-bind
  
  list-map
  list-any
  list-all
  list-each
  
  list-kv-map
  list-kv-any
  list-kv-all
  list-kv-each
  
  list-zip-map
  list-zip-any
  list-zip-all
  list-zip-each
  
  
  ; Utilities for natural numbers and lists together
  list-length<=nat?
  nat<=list-length?
  
  list-length=nat?
  
  list-length<nat?
  nat<list-length?
  
  list-ref-maybe
  
  
  )



; ===== Utilities for natural numbers ================================

(define/own-contract (nat->maybe n)
  (-> natural? #/maybe/c natural?)
  (if (= n 0)
    (nothing)
    (just #/sub1 n)))


; ===== Utilities for lists ==========================================

(define/own-contract (list-foldl state lst func)
  (-> any/c list? (-> any/c any/c any/c) any/c)
  (foldl (fn elem state #/func state elem) state lst))

(define/own-contract (list-foldr lst state func)
  (-> list? any/c (-> any/c any/c any/c) any/c)
  (foldr (fn elem state #/func elem state) state lst))

(define/own-contract (list-bind lst func)
  (-> list? (-> any/c list?) list?)
  (append-map func lst))

(define/own-contract (list-map lst func)
  (-> list? (-> any/c any/c) list?)
  (map func lst))

(define/own-contract (list-any lst func)
  (-> list? (-> any/c any) any)
  (ormap func lst))

(define/own-contract (list-all lst func)
  (-> list? (-> any/c any) any)
  (andmap func lst))

(define/own-contract (list-each lst body)
  (-> list? (-> any/c any) void?)
  (for-each body lst))

(define/own-contract (list-kv-map lst func)
  (-> list? (-> natural? any/c any/c) list?)
  (map func (range #/length lst) lst))

(define/own-contract (list-kv-any lst func)
  (-> list? (-> natural? any/c any) any)
  (expect lst (cons v lst) #f
  #/w-loop next k 0 v v lst lst
    
    ; If this is the last element of the list, we do a tail call. This
    ; is like the way `ormap` does a tail call. We could use `ormap`
    ; itself with `(range #/length lst)`, but this way we only compute
    ; the indexes we need.
    (expect lst (cons new-v lst) (func k v)
    
    #/or (func k v)
    #/next (add1 k) new-v lst)))

(define/own-contract (list-kv-all lst func)
  (-> list? (-> natural? any/c any) any)
  (expect lst (cons v lst) #t
  #/w-loop next k 0 v v lst lst
    
    ; If this is the last element of the list, we do a tail call. This
    ; is like the way `andmap` does a tail call. We could use `andmap`
    ; itself with `(range #/length lst)`, but this way we only compute
    ; the indexes we need.
    (expect lst (cons new-v lst) (func k v)
    
    #/and (func k v)
    #/next (add1 k) new-v lst)))

(define/own-contract (list-kv-each lst body)
  (-> list? (-> natural? any/c any) void?)
  (for-each body (range #/length lst) lst))

(define/own-contract (list-zip-map a b func)
  (-> list? list? (-> any/c any/c any/c) list?)
  (map func a b))

(define/own-contract (list-zip-any a b func)
  (-> list? list? (-> any/c any/c any) any)
  (ormap func a b))

(define/own-contract (list-zip-all a b func)
  (-> list? list? (-> any/c any/c any) any)
  (andmap func a b))

(define/own-contract (list-zip-each a b body)
  (-> list? list? (-> any/c any/c any) void?)
  (for-each body a b))


; ===== Utilities for natural numbers and lists together =============

(define/own-contract (list-length<=nat? lst n)
  (-> list? natural? boolean?)
  (expect lst (cons _ lst) #t
  #/expect (nat->maybe n) (just n) #f
  #/list-length<=nat? lst n))

(define/own-contract (nat<=list-length? n lst)
  (-> natural? list? boolean?)
  (expect (nat->maybe n) (just n) #t
  #/expect lst (cons _ lst) #f
  #/nat<=list-length? n lst))

(define/own-contract (list-length=nat? lst n)
  (-> list? natural? boolean?)
  (expect lst (cons _ lst) (= 0 n)
  #/expect (nat->maybe n) (just n) #f
  #/list-length=nat? lst n))

(define/own-contract (list-length<nat? lst n)
  (-> list? natural? boolean?)
  (not #/nat<=list-length? n lst))

(define/own-contract (nat<list-length? n lst)
  (-> natural? list? boolean?)
  (not #/list-length<=nat? lst n))


(define/own-contract (list-ref-maybe lst i)
  (-> list? natural? maybe?)
  (expect lst (cons v lst) (nothing)
  #/expect (nat->maybe i) (just i) (just v)
  #/list-ref-maybe lst i))
