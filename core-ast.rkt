#lang typed/racket/base

(provide (all-defined-out))




(define-type compound-type (U array-type record-type function-type))


(define-struct: type-reference ((name : Symbol)) #:transparent)
(define-struct: array-type ((elem-type : type-reference)))
(define-struct: record-type ((fields : (Listof (Pair Symbol type-reference)))))
(define-struct: function-type ((args : (Listof (U compound-type type-reference))) (return : (U #f compound-type type-reference))) #:transparent)


(define-struct: integer-literal ((value : Integer)))
(define-struct: string-literal ((value : String)))
(define-struct: nil ((type : (Option type-reference))))

(define-type Constant (U integer-literal string-literal nil))
(define-predicate constant? Constant)
(define-predicate compound-type? compound-type)




