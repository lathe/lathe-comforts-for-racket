#lang info

(define collection "lathe-comforts")

(define deps (list "base"))
(define build-deps
  (list
    "at-exp-lib"
    "lathe-comforts-lib"
    "parendown-doc"
    "parendown-lib"
    "racket-doc"
    "reprovide-lang-lib"
    "scribble-lib"))

(define scribblings
  (list (list "scribblings/lathe-comforts.scrbl" (list 'multi-page))))
