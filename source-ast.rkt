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
    equality
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



(define-struct: identifier ((symbol : Symbol)) #:transparent)
(define-struct: field-ref ((base : lvalue) (field : Symbol) (type : (Option type-reference))) #:transparent)
(define-struct: array-ref ((base : lvalue) (index : expression) (type : (Option type-reference))) #:transparent)

(define-struct: binder ((declarations : (Listof declaration)) (body : expression)) #:transparent)

(define-struct: sequence ((exprs : (Listof expression))) #:transparent)
(define-struct: assignment ((value : lvalue)  (expr : expression)) #:transparent)
(define-struct: if-then-else
                 ((cond : expression)
                  (true : expression)
                  (false : (U #f expression))
                  (type : (U #f 'nil 'unit type-reference))) #:transparent)



(define-struct: function-call ((fun : expression) (args : (Listof expression)) (type : (Option type-reference))) #:transparent)

(define-struct: negation ((expr : expression)) #:transparent)
(define-struct: equality ((operator : (U  '= '<> )) (left : expression) (right : expression) (type : (Option type-reference))) #:transparent)
(define-struct: math ((operator : (U '+ '* '/ '- '< '> '<= '>= '& '\| )) (left : expression) (right : expression)) #:transparent)

(define-struct: create-record ((type : type-reference) (fields : (Listof (Pair Symbol expression)))) #:transparent)
(define-struct: create-array ((type : type-reference) (size : expression) (value : expression)) #:transparent)

(define-struct: while-loop ((guard : expression) (body : expression)) #:transparent)
(define-struct: for-loop ((id : Symbol) (init : expression) (final : expression) (body : expression)) #:transparent)
(define-struct: break () #:transparent)

(define-struct: type-declaration ((name : Symbol) (type : (U type-reference compound-type))) #:transparent)
(define-struct: function-declaration
 ((name : Symbol)
  (args : (Listof (Pair Symbol type-reference)))
  (return-type : (Option type-reference))
  (body : expression)) #:transparent)
(define-struct: variable-declaration ((name : Symbol) (type : type-reference) (value : expression)) #:transparent)
(define-struct: untyped-variable-declaration ((name : Symbol) (value : expression)) #:transparent)




(define-predicate expression? expression)
(define-predicate declaration? declaration)


