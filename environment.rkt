#lang typed/racket/base


(require "core-ast.rkt" racket/list)

(provide
 environment
 environment-ids
 environment-types

 type-environment
 type-environment-ids
 type-environment-types

 resolve-type

 global-environment
 global-type-environment)


(struct: environment
 ((ids : (HashTable Symbol Symbol))
  (types : (HashTable Symbol Symbol))) #:transparent)


(struct: type-environment
 ((ids : (HashTable Symbol resolved-type))
  (types : (HashTable Symbol resolved-value-type))) #:transparent)


(: resolve-type
 (case-lambda
  (value-type type-environment -> resolved-value-type)
  (type type-environment -> resolved-type)))
(define (resolve-type type env)
 (if (type-reference? type)
     (let ((sym (type-reference-name type)))
      (hash-ref (type-environment-types env) sym
       (lambda ()
        (error 'resolve-type "Unbound type name ~a in ~a" sym env))))
     type))




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
 (let ((mtr (lambda: ((lst : (Listof Symbol))) (map type-reference lst)))
       (unit (unit-type))
       (string (type-reference 'string))
       (int (type-reference 'int)))
  (type-environment
   (make-immutable-hash
    (list
     (cons 'print (function-type (mtr '(string)) unit))
     (cons 'flush (function-type (mtr '())  unit))
     (cons 'getchar (function-type (mtr '()) string))
     (cons 'ord (function-type (mtr '(string)) int))
     (cons 'chr (function-type (mtr '(int)) string))
     (cons 'size (function-type (mtr '(string)) int))
     (cons 'substring (function-type (mtr '(string int int)) string))
     (cons 'concat (function-type (mtr '(string string)) string))
     (cons 'not (function-type (mtr '(int)) int))
     (cons 'exit (function-type (mtr '(int)) unit))))
   (make-immutable-hash
    (list
     (cons 'int (int-type))
     (cons 'string (string-type)))))))
