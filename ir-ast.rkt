#lang typed/racket/base


(provide (all-defined-out) unique)
(require "primop.rkt" "types.rkt" "unique.rkt")

(define-type expression
 (U primop-expr conditional bind bind-rec sequence identifier))

(struct: identifier ((name : unique)) #:transparent)
(struct: primop-expr ((op : primop) (args : (Listof expression))) #:transparent)
(struct: conditional ((condition : expression) (t-branch : expression) (f-branch : expression) (type : type)) #:transparent)
(struct: bind ((name : unique) (type : type) (expr : expression) (body : expression)) #:transparent)
(struct: bind-rec ((functions : (Listof (Pair unique function))) (body : expression)) #:transparent)
(struct: sequence ((first : expression) (next : expression)) #:transparent)


(struct: function ((name : unique) (args : (Listof (Pair unique type))) (return-type : type) (body : expression)) #:transparent)

(: function->function-type (function -> function-type))
(define (function->function-type fun)
 (make-function-type (map (inst cdr unique type) (function-args fun)) (function-return-type fun)))




