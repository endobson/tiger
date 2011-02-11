#lang typed/racket/base

(provide (all-defined-out))




(define-type compound-type (U array-type record-type function-type))


(define-struct: type-reference ((name : Symbol)) #:transparent)
(define-struct: array-type ((elem-type : type-reference)))
(define-struct: record-type ((fields : (Listof (Pair Symbol type-reference)))))
(define-struct: function-type ((args : (Listof type-reference)) (return : (Option type-reference))) #:transparent)


(define-struct: integer-literal ((value : Integer)))
(define-struct: string-literal ((value : String)))
(define-struct: nil ())

(define-type Constant (U integer-literal string-literal nil))
(define-predicate constant? Constant)




