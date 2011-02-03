#lang typed/racket/base

(require "core-ast.rkt")

(provide (all-defined-out))

(define-type type-environment (HashTable Symbol resolved-value-type))
(define-type function-environment (HashTable Symbol lifted-function))


(define-type expression
 (U bind
    bind-rec
    lvalue
    sequence
    assignment
    if-then-else
    Constant
    negation
    function-call
    math
    create-record
    create-array
    while-loop
    break))


(define-type lvalue (U identifier field-ref array-ref))



(struct: lifted-program
 ((types : type-environment)
  (functions : function-environment)
  (expr : expression)))

(struct: lifted-function
 ((type : function-type)
  (args : (Listof Symbol))
  (closed-variables : (Listof Symbol))
  (closed-variable-types : (Listof value-type))
  (body : expression)))



(define-struct: identifier ((symbol : Symbol)))
(define-struct: field-ref ((base : lvalue) (field : Symbol)))
(define-struct: array-ref ((base : lvalue) (index : expression)))

(define-struct: bind ((symbol : Symbol) (value : expression) (body : expression)))
(define-struct: bind-rec ((bindings : (Listof (Pair Symbol create-closure))) (body : expression)))

(define-struct: create-closure ((function : Symbol) (closed-variables : (Listof Symbol))))

(define-struct: sequence ((exprs : (Listof expression))))
(define-struct: assignment ((value : lvalue)  (expr : expression)))
(define-struct: if-then-else
                 ((cond : expression)
                  (true : expression)
                  (false : expression)))



(define-struct: function-call ((fun : expression) (args : (Listof expression))))

(define-struct: negation ((expr : expression)))
(define-struct: math ((operator : (U '+ '* '/ '- '= '<> '< '> '<= '>= '& '\| )) (left : expression) (right : expression)))

(define-struct: create-record ((type : type) (fields : (Listof (Pair Symbol expression)))))
(define-struct: create-array ((type : type) (size : expression) (value : expression)))

(define-struct: while-loop ((guard : expression) (body : expression)))
(define-struct: break ())


