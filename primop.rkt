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

(struct: math-primop ((symbol : (U '+ '- '* '/ '= '<> '<= '>= '< '> '& '\|))))
(struct: unit-primop ())
(struct: call-closure-primop ())
(struct: integer-constant-primop ((val : Integer)))
(struct: string-constant-primop ((val : String)))
(struct: nil-primop ())
(struct: runtime-primop ((type : type) (name : Symbol)))

(struct: array-set!-primop ((type : array-type)))
(struct: array-ref-primop ((type : array-type)))
(struct: field-ref-primop ((type : record-type) (field : Symbol)))
(struct: field-set!-primop ((type : record-type) (field : Symbol)))

(struct: create-record-primop ((type : record-type)))
(struct: create-array-primop ((type : array-type)))
