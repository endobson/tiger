#lang typed/racket/base

(provide (all-defined-out) unique)
(require "primop.rkt" "types.rkt" "unique.rkt")


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
(struct: bind-rec ((functions : (Listof (Pair unique function))) (body : expression)) #:transparent)

(struct: function ((name : unique) (args : (Listof (Pair unique type))) (return-type : type) (body : expression)) #:transparent)

(: function->function-type (function -> function-type))
(define (function->function-type fun)
 (make-function-type (map (inst cdr unique type) (function-args fun)) (function-return-type fun)))
