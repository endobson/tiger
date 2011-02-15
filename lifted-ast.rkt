#lang typed/racket/base

(require "primop.rkt" "types.rkt")

(provide (all-defined-out))

(define-type function-environment (HashTable Symbol lifted-function))


(define-type expression
 (U bind
    bind-rec
    sequence
    conditional
    identifier
    primop-expr))



(struct: lifted-program
 ((functions : function-environment)
  (expr : expression)))


(struct: identifier  ((name : Symbol)))
(struct: primop-expr ((rator : primop) (args : (Listof expression))))
(struct: conditional ((condition : expression) (t-branch : expression) (f-branch : expression)))
(struct: bind        ((name : Symbol) (type : type) (expr : expression) (body : expression)))
(struct: bind-rec    ((functions : (Listof (Pair Symbol create-closure))) (body : expression)))
(struct: sequence    ((first : expression) (next : expression)))


(struct: lifted-function
 ((type : function-type)
  (args : (Listof Symbol))
  (closed-variables : (Listof Symbol))
  (closed-variable-types : (Listof type))
  (body : expression)))

(define-struct: create-closure ((function : Symbol) (closed-variables : (Listof Symbol))))


