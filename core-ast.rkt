#lang typed/racket/base

(provide (all-defined-out))


(define-struct: int-type () #:transparent)
(define-struct: string-type () #:transparent)
(define-struct: unit-type () #:transparent)


(define-type type (U value-type unit-type))
(define-type value-type (U type-reference resolved-value-type))
(define-type resolved-value-type (U int-type string-type array-type record-type function-type))
(define-type resolved-type (U Primitive-Type array-type record-type function-type))


(define-struct: type-reference ((name : Symbol)))
(define-struct: array-type ((elem-type : value-type)) #:transparent)
(define-struct: record-type ((fields : (Listof (Pair Symbol value-type)))))
(define-struct: function-type ((args : (Listof value-type)) (return : type)) #:transparent)


(define-struct: integer-literal ((value : Integer)))
(define-struct: string-literal ((value : String)))
(define-struct: nil ())

(define-type Constant (U integer-literal string-literal nil))
(define-predicate constant? Constant)



(define-type Primitive-Type (U int-type string-type unit-type))
(define-predicate primitive-type? Primitive-Type)

