#lang typed/racket/base

(require "primop.rkt")

(define-type expression
 (U primop-expr conditional bind bind-rec sequence))

(struct: primop-expr ((rator : primop) (args : (Listof expression))))
(struct: conditional ((condition : expression) (t-branch : expression) (f-branch : expression)))
(struct: bind ((name : Symbol) (expr : expression) (body : expression)))
(struct: bind-rec ((functions : (Listof (Pair Symbol function))) (body : expression)))
(struct: sequence ((first : expression) (next : expression)))

(define-struct: while-loop ((guard : expression) (body : expression)))
(define-struct: for-loop ((id : Symbol) (init : expression) (final : expression) (body : expression)))
(define-struct: break ())


(struct: function ((arg-names : Symbol) (body : expression)))






