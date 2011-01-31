#lang racket/base

(require racket/contract)

(provide/contract
 (struct integer-literal ((value integer?)))
 (struct string-literal ((value string?)))
 (create-record 
  (->i
   ((type record-type?)
    (fields (type)
     (apply list/c
      (for/list ((field (record-type-fields type)))
       (cons/c (car field)
        (typed-expression?
         (resolve-type (cdr field) (record-type-env type))))))))
   any/c))

 (create-array
  (->i
   ((type array-type?)
    (size integer-typed?)
    (value (type) (typed-expression?
                   (resolve-type (array-type-elem-type type)
                                 (array-type-env typ)))))
   any/c))

 (negation (-> integer-typed? any/c))
 (arithmetic (-> (oneof/c '+ '* '/ '-) integer-typed? integer-typed? any/c))
 (comparison (-> (oneof/c '= '<> '< '> '<= '>=) integer-typed? integer-typed? any/c))
 (boolean (-> (oneof/c 'and 'or) integer-typed? integer-typed? any/c))

 (while-loop (-> integer-typed? unit-typed?))
 (for-loop (-> symbol? integer-typed? integer-typed? unit-typed?))
 
 (binder  (-> (listof declaration?) expression? any/c))
 (sequence (-> (listof expression?) any/c))
 (if-then-else
  (->i
   ((cond integer-typed?)
    (true (false) (typed-expression? (expression-type false)))
    (false expression?))
   any/c))

 (function-declaration
  (->i
   ((name symbol?)
    (args (listof (cons/c symbol? value-type?)))
    (return-type type)
    (body (return-type) (typed-expression? return-type)))
   any/c))
 
 (variable-declaration
  (->i
   ((name symbol?)
    (type value-type?)
    (value (type) (typed-expression? type)))))

 (type-declaration
  (-> symbol? value-type?))


 )






(define ((typed-expression? type) expr)
 (cond
  ((record-type? type) 
   ((record-typed? type) expr))
  ((array-type? type) 
   ((array-typed? type) expr))
  ((integer-type? type)
   (integer-typed? expr))
  ((string-type? type)
   (string-typed? expr))
  ((unit-type? type)
   (unit-typed? expr))))

(define (record-typed? type expr)
 #f)

(define (array-typed? type expr)
 #f)

(define (integer-typed? expr)
 #f)

(define (string-typed? expr)
 #f)

(define (unit-typed? expr)
 #f)

(define (resolve-type type env)
 (cond
  ((type-reference? type)
   (unbox (hash-ref (type-environment-hash env) (type-reference-name env))))
  (else type)))


(struct type-environment (hash))

(struct integer-literal ((value)))
(struct string-literal ((value)))
(struct nil ())



(struct record-type ((fields env)))
(struct integer-type () #:transparent)
(struct string-type () #:transparent)
(struct unit-type () #:transparent)
(struct type-reference (name) #:transparent)
(struct array-type  ((elem-type env)))


(struct create-record (type fields))
(struct create-array  (type size value expression))


(struct negation (expr))
(struct arithmetic (operator left right)) ;(U '+ '* '/ '-)
(struct comparison (operator left right)) ;(U '= '<> '< '> '<= '>=) 
(struct boolean (operator left right)) ;(U 'and 'or)


(struct while-loop (guard body))
(struct for-loop (id init final body))
(struct break ())

(struct sequence (exprs))

(struct function-declaration (name args return-type body))
(struct variable-declaration (name type value))
(struct type-declaration (name type))


