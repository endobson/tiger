#lang typed/racket/base

(require "core-ast.rkt")

(provide (all-defined-out))



(define-type expression
 (U binder
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
    for-loop
    break))


(define-type lvalue (U identifier field-ref array-ref))
(define-type declaration 
 (U type-declaration
    function-declaration
    untyped-variable-declaration
    variable-declaration))



(define-struct: identifier ((symbol : Symbol)))
(define-struct: field-ref ((base : lvalue) (field : Symbol)))
(define-struct: array-ref ((base : lvalue) (index : expression)))

(define-struct: binder ((declarations : (Listof declaration)) (body : expression)))

(define-struct: sequence ((exprs : (Listof expression))))
(define-struct: assignment ((value : lvalue)  (expr : expression)))
(define-struct: if-then-else
                 ((cond : expression)
                  (true : expression)
                  (false : (U #f expression))))



(define-struct: function-call ((fun : expression) (args : (Listof expression))))

(define-struct: negation ((expr : expression)))
(define-struct: math ((operator : (U '+ '* '/ '- '= '<> '< '> '<= '>= '& '\| )) (left : expression) (right : expression)))

(define-struct: create-record ((type : type-reference) (fields : (Listof (Pair Symbol expression)))))
(define-struct: create-array ((type : type-reference) (size : expression) (value : expression)))

(define-struct: while-loop ((guard : expression) (body : expression)))
(define-struct: for-loop ((id : Symbol) (init : expression) (final : expression) (body : expression)))
(define-struct: break ())

(define-struct: type-declaration ((name : Symbol) (type : value-type)))
(define-struct: function-declaration
 ((name : Symbol)
  (args : (Listof (Pair Symbol type-reference)))
  (return-type : (U type-reference unit-type))
  (body : expression)))
(define-struct: variable-declaration ((name : Symbol) (type : type-reference) (value : expression)))
(define-struct: untyped-variable-declaration ((name : Symbol) (value : expression)))




(define-predicate expression? expression)
(define-predicate declaration? declaration)


