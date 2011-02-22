#lang typed/racket/base


(provide (all-defined-out))
(require "primop.rkt" "types.rkt")

(define-type expression
 (U primop-expr conditional bind bind-rec sequence identifier))

(struct: identifier ((name : Symbol)) #:transparent)
(struct: primop-expr ((rator : primop) (args : (Listof expression))) #:transparent)
(struct: conditional ((condition : expression) (t-branch : expression) (f-branch : expression) (type : type)) #:transparent)
(struct: bind ((name : Symbol) (type : type) (expr : expression) (body : expression)) #:transparent)
(struct: bind-rec ((functions : (Listof (Pair Symbol function))) (body : expression)) #:transparent)
(struct: sequence ((first : expression) (next : expression)) #:transparent)


(struct: function ((args : (Listof (Pair Symbol type))) (return-type : type) (body : expression)) #:transparent)





