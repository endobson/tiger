#lang typed/racket/base

(require "primop.rkt")
(require "types.rkt")

(provide (all-defined-out))

(define-type expression
 (U identifier primop-expr conditional bind bind-rec sequence while-loop for-loop break assignment))

(struct: identifier ((name : Symbol)))
(struct: primop-expr ((rator : primop) (args : (Listof expression))))
(struct: conditional ((condition : expression) (t-branch : expression) (f-branch : expression)))
(struct: bind ((name : Symbol) (type : type) (expr : expression) (body : expression)))
(struct: bind-rec ((functions : (Listof (Pair Symbol function))) (body : expression)))
(struct: sequence ((first : expression) (next : expression)))

(struct: while-loop ((guard : expression) (body : expression)))
(struct: for-loop ((id : Symbol) (init : expression) (final : expression) (body : expression)))
(struct: break ())
(struct: assignment ((name : Symbol) (val : expression)))


(struct: function ((args : (Listof (Pair Symbol type))) (return-type : type) (body : expression)))






