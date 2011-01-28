#lang typed/racket/base

(provide (all-defined-out))


(define-type expression
 (U lvalue binder sequence
    assignment if-then-else
    integer-literal
    string-literal
    nil
    negation
    function-call
    arithmetic
    comparison
    boolean
    create-record
    create-array
    while-loop
    for-loop
    break))

(define-type lvalue (U identifier field-ref array-ref))
(define-type declaration (U type-declaration function-declaration variable-declaration))

(define-struct: identifier ((symbol : Symbol)))
(define-struct: field-ref ((base : lvalue) (field : Symbol)))
(define-struct: array-ref ((base : lvalue) (index : expression)))

(define-struct: binder ((declarations : (Listof declaration)) (body : expression)))

(define-struct: sequence ((exprs : (Listof expression))))
(define-struct: assignment ((value : lvalue)  (expr : expression)))
(define-struct: if-then-else ((cond : expression)
                              (true : expression)
                              (false : (U #f expression))))

(define-struct: integer-literal ((value : Integer)))
(define-struct: string-literal ((value : Integer)))
(define-struct: nil ())

(define-struct: negation ((expr : expression)))

(define-struct: function-call ((fun : expression) (args : (Listof expression))))

(define-struct: arithmetic ((operator : (U '+ '* '/ '-)) (left : expression) (right : expression)))
(define-struct: comparison ((operator : (U '= '<> '< '> '<= '>=)) (left : expression) (right : expression)))
(define-struct: boolean ((operator : (U 'and 'or)) (left : expression) (right : expression)))
(define-struct: create-record ((type : type) (fields : (Listof (Pair Symbol expression)))))
(define-struct: create-array ((type : type) (size : expression) (value : expression)))
(define-struct: while-loop ((guard : expression) (body : expression)))
(define-struct: for-loop ((id : Symbol) (init : expression) (final : expression) (body : expression)))
(define-struct: break ())

(define-struct: type-declaration ((name : Symbol) (type : type)))
(define-struct: function-declaration ((name : Symbol) (args : (Listof (Pair Symbol type))) (return-type : type) (body : expression)))
(define-struct: variable-declaration ((name : Symbol) (type : type) (value : expression)))
(define-struct: untyped-variable-declaration ((name : Symbol) (value : expression)))

(define-type type (U value-type unit-type))
(define-type value-type (U Symbol int-type string-type array-type record-type function-type))

(define-struct: int-type ())
(define-struct: string-type ())
(define-struct: unit-type ())

(define-struct: array-type ((elem-type : value-type)))
(define-struct: record-type ((fields : (Listof (Pair Symbol value-type)))))
(define-struct: function-type ((args : (Listof value-type)) (return : type)))






