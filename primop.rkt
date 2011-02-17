#lang typed/racket/base

(provide (except-out (all-defined-out) make-runtime-primop))

(require "types.rkt" "external-functions.rkt")


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
    equality-primop
    
    
    ))

(define-struct: math-primop ((symbol : (U '+ '- '* '/ '<= '>= '< '> '& '\|))) #:transparent)
(define-struct: equality-primop ((equality : Boolean) (type : type)) #:transparent)
(define-struct: unit-primop () #:transparent)
(define-struct: call-closure-primop () #:transparent)
(define-struct: integer-constant-primop ((val : Integer)) #:transparent)
(define-struct: string-constant-primop ((val : String)) #:transparent)
(define-struct: nil-primop ((type : type)) #:transparent)
(define-struct: runtime-primop ((type : type) (name : Symbol)) #:transparent)

(define-struct: array-set!-primop ((type : array-type)) #:transparent)
(define-struct: array-ref-primop ((type : array-type)) #:transparent)
(define-struct: field-ref-primop ((type : record-type) (field : Symbol)) #:transparent)
(define-struct: field-set!-primop ((type : record-type) (field : Symbol)) #:transparent)

(define-struct: create-record-primop ((type : record-type)) #:transparent)
(define-struct: create-array-primop ((type : array-type)) #:transparent)


(: runtime-primop-database (HashTable Symbol runtime-primop))
(define runtime-primop-database
 (make-immutable-hash
  (hash-map external-function-database
   (lambda: ((name : Symbol) (type : function-type)) (cons name (runtime-primop type name))))))
 
