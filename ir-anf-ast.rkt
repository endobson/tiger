#lang typed/racket/base

(provide (all-defined-out))
(require "primop.rkt" "types.rkt")


(define-type expression (U return conditional bind-primop bind-rec))

(struct: return ((name : Symbol)) #:transparent)
(struct: conditional
 ((conditon : Symbol)
  (t-branch : expression)
  (f-branch : expression)
  (type : type)) #:transparent)
(struct: bind-primop
 ((name : Symbol)
  (type : type)
  (op : primop)
  (args : (Listof Symbol))
  (body : expression)) #:transparent)
(struct: bind-rec ((functions : (Listof (Pair Symbol function))) (body : expression)) #:transparent)

(struct: function ((name : Symbol) (args : (Listof (Pair Symbol type))) (return-type : type) (body : expression)) #:transparent)

(: function->function-type (function -> function-type))
(define (function->function-type fun)
 (make-function-type (map (inst cdr Symbol type) (function-args fun)) (function-return-type fun)))
