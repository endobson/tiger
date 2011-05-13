#lang typed/racket/base

(provide (except-out (all-defined-out) make-runtime-primop))

(require racket/match racket/list)
(require "types.rkt" "external-functions.rkt" "unique.rkt")


(define-type primop
 (U math-primop
    unit-primop
    undefined-primop
    call-closure-primop
    call-known-function-primop
    call-known-runtime-primop
    integer-constant-primop
    string-constant-primop
    nil-primop
    runtime-primop
    box-set!-primop
    array-set!-primop
    field-set!-primop
    box-ref-primop
    array-ref-primop
    field-ref-primop
    create-box-primop
    create-array-primop
    create-record-primop
    equality-primop
    comparison-primop
    
    
    ))

(define-struct: math-primop ((symbol : (U '+ '- '* '/ '& '\|))) #:transparent)
(define-struct: equality-primop ((equality : Boolean) (type : type)) #:transparent)
(define-struct: comparison-primop ((symbol : (U '< '> '<= '>=)) (type : (U Int-Type String-Type))) #:transparent)
(define-struct: unit-primop () #:transparent)
(define-struct: undefined-primop ((type : type)) #:transparent)
(define-struct: call-closure-primop ((type : function-type)) #:transparent)
(define-struct: call-known-function-primop ((type : function-type) (name : unique)) #:transparent)
(define-struct: call-known-runtime-primop ((type : function-type) (name : Symbol)) #:transparent)
(define-struct: integer-constant-primop ((val : Integer)) #:transparent)
(define-struct: string-constant-primop ((val : String)) #:transparent)
(define-struct: nil-primop ((type : type)) #:transparent)
(define-struct: runtime-primop ((type : function-type) (name : Symbol)) #:transparent)

(define-struct: box-set!-primop ((type : box-type)) #:transparent)
(define-struct: box-ref-primop ((type : box-type)) #:transparent)
(define-struct: array-set!-primop ((type : array-type)) #:transparent)
(define-struct: array-ref-primop ((type : array-type)) #:transparent)


(define-struct: field-ref-primop ((type : record-type) (field : Symbol)) #:transparent)
(define-struct: field-set!-primop ((type : record-type) (field : Symbol)) #:transparent)

(define-struct: create-box-primop ((type : box-type)) #:transparent)
(define-struct: create-record-primop ((type : record-type)) #:transparent)
(define-struct: create-array-primop ((type : array-type)) #:transparent)


(: runtime-primop-database (HashTable Symbol runtime-primop))
(define runtime-primop-database
 (make-immutable-hash
  (hash-map external-function-database
   (lambda: ((name : Symbol) (type : function-type)) (cons name (runtime-primop type name))))))
 

(: primop-name (primop -> Symbol))
(define (primop-name op)
 (match op
  ((integer-constant-primop n) (string->symbol (string-append "int-" (number->string n) "-val")))
  ((string-constant-primop str) (string->symbol (string-append "string-" (format "~s" str) "-val")))
  ((unit-primop) 'unit)
  ((undefined-primop ty) 'undef)
  ((nil-primop ty) 'nil)
  ((runtime-primop ty name) name)
  ((math-primop op)
   (case op
    ((+) 'plus)
    ((-) 'minus)
    ((*) 'times)
    ((/) 'divide)
    ((&) 'and)
    ((\|) 'or)))
  ((comparison-primop op ty)
   (case op
    ((<=) 'less-then-equal)
    ((>=) 'greater-then-equal)
    ((>) 'greater)
    ((<) 'less)))
  ((equality-primop eql type) (if eql 'equal 'not-equal))
  ((call-closure-primop ty) 'call)
  ((create-box-primop ty) 'create-box)
  ((create-array-primop ty) 'create-array)
  ((create-record-primop ty) 'create-record)
  ((box-ref-primop ty) 'box-ref)
  ((array-ref-primop ty) 'array-ref)
  ((field-ref-primop ty name) (string->symbol (string-append "field-" (symbol->string name) "-ref")))
  ((box-set!-primop ty) 'box-set!)
  ((array-set!-primop ty) 'array-set!)
  ((field-set!-primop ty name) (string->symbol (string-append "field-" (symbol->string name) "-ref")))
  (else (error 'primop-name "Missing Case"))))

    



(: primop-arg-types (primop -> (Listof type)))
(define (primop-arg-types op)
 (match op
  ((integer-constant-primop n) empty)
  ((string-constant-primop str) empty)
  ((unit-primop) empty)
  ((nil-primop ty) empty)
  ((undefined-primop ty) empty)
  ((runtime-primop ty name) empty)
  ((math-primop sym)
   (list int-type int-type))
  ((equality-primop eql ty) (list ty ty))
  ((comparison-primop sym ty) (list ty ty))
  ((call-closure-primop ty)
   (cons ty (function-type-arg-types ty)))
  ((call-known-function-primop ty name)
   (cons ty (function-type-arg-types ty)))
  ((call-known-runtime-primop ty name)
   (cons ty (function-type-arg-types ty)))
  ((create-box-primop ty) 
   (list (box-type-elem-type ty)))
  ((create-array-primop ty) 
   (list int-type (array-type-elem-type ty)))
  ((create-record-primop ty)
   (map (inst cdr Symbol type) (record-type-fields ty)))
  ((box-ref-primop ty)
   (list ty))
  ((array-ref-primop ty)
   (list ty int-type))
  ((field-ref-primop ty name)
   (list ty))
  ((box-set!-primop ty)
   (list ty (box-type-elem-type ty)))
  ((array-set!-primop ty)
   (list ty int-type (array-type-elem-type ty)))
  ((field-set!-primop ty name)
   (list ty (record-type-field-type ty name)))
  (else (error 'primop-arg-types "Not yet implemented ~a" op))))



