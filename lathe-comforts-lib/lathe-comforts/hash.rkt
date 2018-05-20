#lang parendown racket/base

; lathe-comforts/hash
;
; Utilities for hash tables.

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


(require #/only-in racket/contract/base
  -> ->i any any/c list/c listof)
(require #/only-in racket/contract/region define/contract)

(require #/only-in lathe-comforts
  dissect dissectfn expect fn mat w- w-loop)
(require #/only-in lathe-comforts/maybe
  just maybe? maybe/c maybe-map nothing)
(require #/only-in lathe-comforts/list
  list-any list-bind list-foldl list-map)

; TODO: Document each of these exports.
(provide
  make-similar-hash
  hash-comparison-same?
  hash-keys-same?
  hash-ref-maybe
  hash-set-maybe
  hash-kv-map-sorted
  hash-kv-map-maybe-kv
  hash-kv-map-kv
  hash-kv-map-maybe
  hash-kv-map
  hash-kv-any
  hash-kv-all
  hash-kv-each
  hash-v-map-maybe
  hash-v-map
  hash-v-any
  hash-v-all
  hash-v-each
)



(define/contract (make-similar-hash example assocs)
  (-> hash? (listof pair?) hash?)
  (list-foldl (hash-copy-clear example) assocs #/fn result entry
    (dissect entry (cons k v)
    #/hash-set result k v)))

(define/contract (hash-comparison-same? a b)
  (-> hash? hash? boolean?)
  (list-any (list hash-equal? hash-eqv? hash-eq?) #/fn check
    (and (check a) (check b))))

(define/contract (hash-keys-same? a b)
  (->i ([a hash?] [b hash?])
    ; The two hash tables must have the same comparison functions.
    #:pre [a b] (hash-comparison-same? a b)
    [_ boolean?])
  (and (= (hash-count a) (hash-count b)) #/hash-keys-subset? a b))

(define/contract (hash-ref-maybe hash key)
  (-> hash? any/c maybe?)
  ; NOTE: We don't implement this using `hash-has-key?` followed by
  ; `hash-ref` because that could be prone to a race condition.
  (w- dummy (list #/list)
  #/w- result (hash-ref hash key #/fn dummy)
  #/if (eq? dummy result)
    (nothing)
    (just result)))

(define/contract (hash-set-maybe hash key maybe-value)
  (-> hash? any/c maybe? hash?)
  (expect maybe-value (just value)
    (hash-remove hash key)
    (hash-set hash key value)))

(define/contract (hash-kv-map-sorted key<? hash func)
  (-> (-> any/c any/c boolean?) hash? (-> any/c any/c any/c) list?)
  (list-map (sort (hash->list hash) key<? #:key car)
  #/dissectfn (cons k v)
    (func k v)))

(define/contract (hash-kv-map-maybe-kv hash func)
  (-> hash? (-> any/c any/c #/maybe/c #/list/c any/c any/c) hash?)
  (make-similar-hash hash #/list-bind (hash->list hash)
  #/dissectfn (cons k v)
    (expect (func k v) (just #/list k v) (list)
    #/list #/cons k v)))

(define/contract (hash-kv-map-kv hash func)
  (-> hash? (-> any/c any/c #/list/c any/c any/c) hash?)
  (hash-kv-map-maybe-kv hash #/fn k v #/just #/func k v))

(define/contract (hash-kv-map-maybe hash func)
  (-> hash? (-> any/c any/c maybe?) hash?)
  (hash-kv-map-maybe-kv hash #/fn k v
    (maybe-map (func k v) #/fn v #/list k v)))

(define/contract (hash-kv-map hash func)
  (-> hash? (-> any/c any/c any/c) hash?)
  (hash-kv-map-maybe hash #/fn k v #/just #/func k v))

(define/contract (hash-kv-any hash func)
  (-> hash? (-> any/c any/c any) any)
  
  ; NOTE: We go to all this trouble with `hash-iterate-first`,
  ; `hash-iterate-pair`, and `hash-iterate-next` just so that when we
  ; exit early, we avoid the cost of a full `hash->list`.
  (w- cursor (hash-iterate-first hash)
  #/mat cursor #f #f
  #/w-loop next cursor cursor
    (dissect (hash-iterate-pair hash cursor) (cons k v)
    #/w- cursor (hash-iterate-next hash cursor)
    
    ; If this is the last entry of the hash, we do a tail call. This
    ; is like the way `andmap` does a tail call for the last element
    ; of a list.
    #/mat cursor #f (func k v)
    
    #/or (func k v)
    #/next cursor)))

(define/contract (hash-kv-all hash func)
  (-> hash? (-> any/c any/c any) any)
  
  ; NOTE: We go to all this trouble with `hash-iterate-first`,
  ; `hash-iterate-pair`, and `hash-iterate-next` just so that when we
  ; exit early, we avoid the cost of a full `hash->list`.
  (w- cursor (hash-iterate-first hash)
  #/mat cursor #f #t
  #/w-loop next cursor cursor
    (dissect (hash-iterate-pair hash cursor) (cons k v)
    #/w- cursor (hash-iterate-next hash cursor)
    
    ; If this is the last entry of the hash, we do a tail call. This
    ; is like the way `andmap` does a tail call for the last element
    ; of a list.
    #/mat cursor #f (func k v)
    
    #/and (func k v)
    #/next cursor)))

(define/contract (hash-kv-each hash body)
  (-> hash? (-> any/c any/c any) void?)
  (hash-for-each hash #/fn k v
    (body k v)))

(define/contract (hash-v-map-maybe hash func)
  (-> hash? (-> any/c maybe?) hash?)
  (hash-kv-map-maybe hash #/fn k v #/func v))

(define/contract (hash-v-map hash func)
  (-> hash? (-> any/c any/c) hash?)
  (hash-kv-map hash #/fn k v #/func v))

(define/contract (hash-v-any hash func)
  (-> hash? (-> any/c any) any)
  (hash-kv-any hash #/fn k v #/func v))

(define/contract (hash-v-all hash func)
  (-> hash? (-> any/c any) any)
  (hash-kv-all hash #/fn k v #/func v))

(define/contract (hash-v-each hash body)
  (-> hash? (-> any/c any) void?)
  (hash-kv-each hash #/fn k v
    (body v)))
