#lang typed/racket/base

(require "primop.rkt")
(require "types.rkt")
(require racket/list racket/match)

(provide (all-defined-out))

(define-type expression
 (U identifier primop-expr conditional bind bind-rec while-loop for-loop break assignment))

(struct: identifier ((name : Symbol)) #:transparent)
(struct: primop-expr ((rator : primop) (args : (Listof expression))) #:transparent)
(struct: conditional ((condition : expression) (t-branch : expression) (f-branch : expression) (type : type)) #:transparent)
(struct: bind ((name : Symbol) (type : type) (expr : expression) (body : expression)) #:transparent)
(struct: bind-rec ((functions : (Listof (Pair Symbol function))) (body : expression)) #:transparent)

(struct: while-loop ((guard : expression) (body : expression)) #:transparent)
(struct: for-loop ((id : Symbol) (init : expression) (final : expression) (body : expression)) #:transparent)
(struct: break () #:transparent)
(struct: assignment ((name : Symbol) (val : expression)) #:transparent)


(struct: function ((args : (Listof (Pair Symbol type))) (return-type : type) (body : expression)) #:transparent)


(: function->function-type (function -> function-type))
(define (function->function-type fun)
 (make-function-type (map (inst cdr Symbol type) (function-args fun)) (function-return-type fun)))





(: type-of 
 (case-lambda
  (expression -> type)
  (expression (HashTable Symbol type) -> type)))
(define type-of
 (case-lambda:
  (((expr : expression)) (type-of expr (ann (make-immutable-hash empty) (HashTable Symbol type))))
  (((expr : expression) (env : (HashTable Symbol type)))
   (match expr 
    ((identifier id) (hash-ref env id (lambda () (error 'type-of "Unbound identifier ~a in ~a" id env))))
    ((primop-expr op args) 
     (match op
      ((call-closure-primop ty) (function-type-return-type ty))
      ((math-primop sym) int-type)
      ((equality-primop eql type) int-type)
      ((unit-primop) unit-type)
      ((integer-constant-primop val) int-type)
      ((string-constant-primop val) string-type) 
      ((nil-primop ty) ty)
      ((runtime-primop ty name) ty) 
      ((box-set!-primop ty) unit-type)
      ((array-set!-primop ty) unit-type) 
      ((field-set!-primop ty field) unit-type)
  
      ((box-ref-primop ty) (box-type-elem-type ty)) 
      ((array-ref-primop ty) (array-type-elem-type ty))
      ((field-ref-primop ty field) (record-type-field-type ty field))
      
      ((create-box-primop ty) ty)
      ((create-record-primop ty) ty)
      ((create-array-primop ty) ty) 
  
      (else (error 'type-of "Not yet implemented primop ~a" op))))
    ((conditional c t f ty) ty)
    ((bind name ty expr body) 
     (type-of body (hash-set env name ty)))
    ((bind-rec funs body)
     (type-of body
      (foldr
       (lambda: ((p : (Pair Symbol function))
                 (env : (HashTable Symbol type)))
        (hash-set env (car p) (function->function-type (cdr p)))) env funs)))
    ((while-loop g body) unit-type)
    ((for-loop id init final body) unit-type)
    ((break) (error 'type-of "Cannot take type of break"))
    ((assignment name val) unit-type)
    (else (error 'type-of "Not yet implemented ~a" expr))))))


