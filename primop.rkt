#lang typed/racket/base

(provide (all-defined-out))

(require "types.rkt")


(define-type primop
 (U math-primop
    unit-primop
    call-closure-primop
    integer-constant-primop
    string-constant-primop
    nil-primop
    runtime-primop
    array-set!-primop
    array-ref-primop
    field-ref-primop
    field-set!-primop
    create-array-primop
    create-record-primop

    
    
    ))

(define-struct: math-primop ((symbol : (U '+ '- '* '/ '= '<> '<= '>= '< '> '& '\|))) #:transparent)
(define-struct: unit-primop () #:transparent)
(define-struct: call-closure-primop () #:transparent)
(define-struct: integer-constant-primop ((val : Integer)) #:transparent)
(define-struct: string-constant-primop ((val : String)) #:transparent)
(define-struct: nil-primop () #:transparent)
(define-struct: runtime-primop ((type : type) (name : Symbol)) #:transparent)

(define-struct: array-set!-primop ((type : array-type)) #:transparent)
(define-struct: array-ref-primop ((type : array-type)) #:transparent)
(define-struct: field-ref-primop ((type : record-type) (field : Symbol)) #:transparent)
(define-struct: field-set!-primop ((type : record-type) (field : Symbol)) #:transparent)

(define-struct: create-record-primop ((type : record-type)) #:transparent)
(define-struct: create-array-primop ((type : array-type)) #:transparent)
