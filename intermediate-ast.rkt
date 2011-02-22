#lang typed/racket/base

(require "primop.rkt")
(require "types.rkt")

(provide (all-defined-out))

(define-type expression
 (U identifier primop-expr conditional bind bind-rec sequence while-loop for-loop break assignment))

(struct: identifier ((name : Symbol)) #:transparent)
(struct: primop-expr ((rator : primop) (args : (Listof expression))) #:transparent)
(struct: conditional ((condition : expression) (t-branch : expression) (f-branch : expression) (type : type)) #:transparent)
(struct: bind ((name : Symbol) (type : type) (expr : expression) (body : expression)) #:transparent)
(struct: bind-rec ((functions : (Listof (Pair Symbol function))) (body : expression)) #:transparent)
(struct: sequence ((first : expression) (next : expression)) #:transparent)

(struct: while-loop ((guard : expression) (body : expression)) #:transparent)
(struct: for-loop ((id : Symbol) (init : expression) (final : expression) (body : expression)) #:transparent)
(struct: break () #:transparent)
(struct: assignment ((name : Symbol) (val : expression)) #:transparent)


(struct: function ((args : (Listof (Pair Symbol type))) (return-type : type) (body : expression)) #:transparent)


(: function->function-type (function -> function-type))
(define (function->function-type fun)
 (make-function-type (map (inst cdr Symbol type) (function-args fun)) (function-return-type fun)))






