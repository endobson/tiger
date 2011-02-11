#lang typed/racket/base


(require "core-ast.rkt" racket/list)

(provide
 environment
 environment-ids
 environment-types

 global-environment)


(struct: environment
 ((ids : (HashTable Symbol Symbol))
  (types : (HashTable Symbol Symbol))) #:transparent)





(: global-environment environment)
(define global-environment
 (environment
  (make-immutable-hash
   '((print . print)
     (flush . flush)
     (getchar . getchar)
     (ord . ord)
     (chr . chr)
     (size . size)
     (substring . substring)
     (concat . concat)
     (not . not)
     (exit . exit)))
  (make-immutable-hash
   '((int . int) (string . string)))))



