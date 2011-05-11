#lang typed/racket/base


(require "unique.rkt")

(provide global-id-names global-type-names)




(: names->map ((Listof Symbol) -> (HashTable Symbol unique)))
(define (names->map names)
 (make-immutable-hash
  (map (inst cons Symbol unique) names (map gen-uniq names))))


(: global-id-names (HashTable Symbol unique))
(define global-id-names
 (names->map
   '(print
     flush
     getchar
     ord
     chr
     size
     substring
     concat
     not
     exit
     int
     string
     itoa
     atoi
     readline
     )))



(: global-type-names (HashTable Symbol unique))
(define global-type-names
 (names->map
   '(int string)))

