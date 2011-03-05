#lang typed/racket/base

(provide (all-defined-out) unique)
(require "primop.rkt" "types.rkt" "unique.rkt")



(define-type function-environment (HashTable unique function))

(struct: lifted-program
 ((functions : function-environment)
  (expr : expression)) #:transparent)









(define-type expression (U return conditional bind-primop bind-rec))

(struct: return ((name : unique)) #:transparent)
(struct: conditional
 ((condition : unique)
  (t-branch : expression)
  (f-branch : expression)
  (type : type)) #:transparent)
(struct: bind-primop
 ((name : unique)
  (type : type)
  (op : primop)
  (args : (Listof unique))
  (body : expression)) #:transparent)

(struct: bind-rec    ((functions : (Listof (Pair unique create-closure))) (body : expression)) #:transparent)


(struct: function
 ((name : unique)
  (full-type : function-type)
  (args : (Listof unique))
  (closed-variables : (Listof unique))
  (closed-variable-types : (Listof type))
  (body : expression)) #:transparent)

(define-struct: create-closure ((function : unique) (closed-variables : (Listof unique))) #:transparent)

(: function->function-type (function -> function-type))
(define (function->function-type fun)
 (function-full-type fun))


