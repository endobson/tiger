#lang typed/racket/base

(require "primop.rkt" "types.rkt" "unique.rkt")

(provide (all-defined-out) unique)

(define-type function-environment (HashTable unique lifted-function))


(define-type expression
 (U bind
    bind-rec
    sequence
    conditional
    identifier
    primop-expr))



(struct: lifted-program
 ((functions : function-environment)
  (expr : expression)) #:transparent)


(struct: identifier  ((name : unique)) #:transparent)
(struct: primop-expr ((rator : primop) (args : (Listof expression))) #:transparent)
(struct: conditional ((condition : expression) (t-branch : expression) (f-branch : expression) (type : type)) #:transparent)
(struct: bind        ((name : unique) (type : type) (expr : expression) (body : expression)) #:transparent)
(struct: bind-rec    ((functions : (Listof (Pair unique create-closure))) (body : expression)) #:transparent)
(struct: sequence    ((first : expression) (next : expression)) #:transparent)


(struct: lifted-function
 ((name : unique)
  (type : function-type)
  (args : (Listof unique))
  (closed-variables : (Listof unique))
  (closed-variable-types : (Listof type))
  (body : expression)) #:transparent)

(define-struct: create-closure ((function : unique) (closed-variables : (Listof unique))) #:transparent)


