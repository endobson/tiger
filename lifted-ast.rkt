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
  (expr : expression)) #:transparent)


(struct: identifier  ((name : Symbol)) #:transparent)
(struct: primop-expr ((rator : primop) (args : (Listof expression))) #:transparent)
(struct: conditional ((condition : expression) (t-branch : expression) (f-branch : expression) (type : type)) #:transparent)
(struct: bind        ((name : Symbol) (type : type) (expr : expression) (body : expression)) #:transparent)
(struct: bind-rec    ((functions : (Listof (Pair Symbol create-closure))) (body : expression)) #:transparent)
(struct: sequence    ((first : expression) (next : expression)) #:transparent)


(struct: lifted-function
 ((type : function-type)
  (args : (Listof Symbol))
  (closed-variables : (Listof Symbol))
  (closed-variable-types : (Listof type))
  (body : expression)) #:transparent)

(define-struct: create-closure ((function : Symbol) (closed-variables : (Listof Symbol))) #:transparent)


