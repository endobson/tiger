#lang typed/racket/base


(require "core-ast.rkt" racket/list)

(provide
 environment
 environment-ids
 environment-types

 type-environment
 type-environment-ids
 type-environment-types

 global-environment
 global-type-environment)


(struct: environment
 ((ids : (HashTable Symbol Symbol))
  (types : (HashTable Symbol Symbol))))


(struct: type-environment
 ((ids : (HashTable Symbol resolved-type))
  (types : (HashTable Symbol resolved-type))))




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

(: global-type-environment type-environment)
(define global-type-environment
 (type-environment
  (make-immutable-hash
   (list
    (cons 'print (function-type (list (string-type)) (unit-type)))
    (cons 'flush (function-type empty  (unit-type)))
    (cons 'getchar (function-type (list) (string-type)))
    (cons 'ord (function-type (list (string-type)) (int-type)))
    (cons 'chr (function-type (list (int-type)) (string-type)))
    (cons 'size (function-type (list (string-type)) (int-type)))
    (cons 'substring (function-type (list (string-type) (int-type) (int-type)) (string-type)))
    (cons 'concat (function-type (list (string-type) (string-type)) (string-type)))
    (cons 'not (function-type (list (int-type)) (int-type)))
    (cons 'exit (function-type (list (int-type)) (unit-type)))))
  (make-immutable-hash
   (list
    (cons 'int (int-type))
    (cons 'string (string-type))))))
