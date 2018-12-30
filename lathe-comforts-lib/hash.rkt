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
  -> ->i any any/c contract-out listof)

(require #/only-in lathe-comforts
  dissect dissectfn expect fn mat w- w-loop)
(require #/only-in lathe-comforts/maybe just maybe? maybe-map nothing)
(require #/only-in lathe-comforts/list
  list-any list-bind list-foldl list-map)

(provide #/contract-out
  [make-similar-hash (-> hash? (listof pair?) hash?)]
  [hash-comparison-same? (-> hash? hash? boolean?)]
  [hash-keys-same?
    (->i ([a hash?] [b hash?])
      ; The two hash tables must have the same comparison function.
      #:pre [a b] (hash-comparison-same? a b)
      [_ boolean?])]
  [hash-ref-maybe (-> hash? any/c maybe?)]
  [hash-set-maybe (-> hash? any/c maybe? hash?)]
  [hash-kv-map-sorted
    (-> (-> any/c any/c boolean?) hash? (-> any/c any/c any/c) list?)]
  [hash-kv-bind (-> hash? (-> any/c any/c hash?) hash?)]
  [hash-kv-map-maybe (-> hash? (-> any/c any/c maybe?) hash?)]
  [hash-kv-map (-> hash? (-> any/c any/c any/c) hash?)]
  [hash-kv-any (-> hash? (-> any/c any/c boolean?) boolean?)]
  [hash-kv-all (-> hash? (-> any/c any/c boolean?) boolean?)]
  [hash-kv-each (-> hash? (-> any/c any/c any) void?)]
  [hash-v-map-maybe (-> hash? (-> any/c maybe?) hash?)]
  [hash-v-map (-> hash? (-> any/c any/c) hash?)]
  [hash-v-any (-> hash? (-> any/c boolean?) boolean?)]
  [hash-v-all (-> hash? (-> any/c boolean?) boolean?)]
  [hash-v-each (-> hash? (-> any/c any) void?)]
)



(define (make-similar-hash example assocs)
  (list-foldl (hash-copy-clear example) assocs #/fn result entry
    (dissect entry (cons k v)
    #/hash-set result k v)))

(define (hash-comparison-same? a b)
  (list-any (list hash-equal? hash-eqv? hash-eq?) #/fn check
    (and (check a) (check b))))

(define (hash-keys-same? a b)
  (and (= (hash-count a) (hash-count b)) #/hash-keys-subset? a b))

(define (hash-ref-maybe hash key)
  ; NOTE: We don't implement this using `hash-has-key?` followed by
  ; `hash-ref` because that could be prone to a race condition.
  (w- dummy (list #/list)
  #/w- result (hash-ref hash key #/fn dummy)
  #/if (eq? dummy result)
    (nothing)
    (just result)))

(define (hash-set-maybe hash key maybe-value)
  (expect maybe-value (just value)
    (hash-remove hash key)
    (hash-set hash key value)))

(define (hash-kv-map-sorted key<? hash func)
  (list-map (sort (hash->list hash) key<? #:key car)
  #/dissectfn (cons k v)
    (func k v)))

(define (hash-kv-bind hash func)
  (make-similar-hash hash
  #/list-bind (hash->list hash) #/dissectfn (cons k v)
    (hash->list #/func k v)))

(define (hash-kv-map-maybe hash func)
  (hash-kv-bind hash #/fn k v
    (expect (func k v) (just v) (hash)
    #/hash k v)))

(define (hash-kv-map hash func)
  (hash-kv-map-maybe hash #/fn k v #/just #/func k v))

; NOTE: We only return booleans from this so as not to reveal the
; iteration order.
(define (hash-kv-any hash func)
  ; NOTE: We go to all this trouble with `hash-iterate-first`,
  ; `hash-iterate-pair`, and `hash-iterate-next` just so that when we
  ; exit early, we avoid the cost of a full `hash->list`.
  (w-loop next cursor (hash-iterate-first hash)
    (mat cursor #f #f
    #/dissect (hash-iterate-pair hash cursor) (cons k v)
    #/or (func k v)
    #/next #/hash-iterate-next hash cursor)))

; NOTE: We only return booleans from this so as not to reveal the
; iteration order.
(define (hash-kv-all hash func)
  (not #/hash-kv-any hash #/fn k v #/not #/func k v))

(define (hash-kv-each hash body)
  (hash-for-each hash #/fn k v
    (body k v)))

(define (hash-v-map-maybe hash func)
  (hash-kv-map-maybe hash #/fn k v #/func v))

(define (hash-v-map hash func)
  (hash-kv-map hash #/fn k v #/func v))

; NOTE: We only return booleans from this so as not to reveal the
; iteration order.
(define (hash-v-any hash func)
  (hash-kv-any hash #/fn k v #/func v))

; NOTE: We only return booleans from this so as not to reveal the
; iteration order.
(define (hash-v-all hash func)
  (hash-kv-all hash #/fn k v #/func v))

(define (hash-v-each hash body)
  (hash-kv-each hash #/fn k v
    (body v)))
