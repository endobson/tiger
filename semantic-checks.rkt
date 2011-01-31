#lang typed/racket/base

(require racket/match racket/list)

(require "source-ast.rkt" "core-ast.rkt")
(provide rename-variables type-check)




(: rename-variables (expression -> expression))


(struct: environment
 ((ids : (HashTable Symbol Symbol))
  (types : (HashTable Symbol Symbol))))


(define (rename-variables prog)
 (define-type updater
  (case-lambda
   (lvalue -> lvalue)
   (declaration -> declaration)
   (value-type -> value-type)
   (type -> type)
   (expression -> expression)))
 (: rename (environment -> updater))
 (define (rename env)
  (: recur updater)
  (define (recur prog)
   (match prog
    ((identifier sym) (identifier (lookup-identifier sym env)))
    ((field-ref base field) (field-ref (recur base) field))
    ((array-ref base index) (array-ref (recur base) (recur index)))
    ((binder declarations body)
     (let ((env (extend-environment declarations env)))
      (binder (map (rename env) declarations) ((rename env) body))))
    ((sequence exprs) (sequence (map recur exprs)))
    ((assignment value expr)
     (assignment (recur value) (recur expr)))
    ((if-then-else c t f)
     (if-then-else (recur c) (recur t) (and f (recur f))))
    ((integer-literal v) (integer-literal v))
    ((string-literal s) (string-literal s))
    ((nil) (nil))
    ((negation expr) (negation (recur expr)))
    ((function-call fun args)
     (function-call (recur fun) (map recur args)))
    ((arithmetic op left right)
     (arithmetic op (recur left) (recur right)))
    ((comparison op left right)
     (comparison op (recur left) (recur right)))
    ((create-record type fields)
     (create-record type
       (map (inst cons Symbol expression)
            (map (inst car Symbol expression) fields)
            (map recur (map (inst cdr Symbol expression) fields)))))
    ((create-array type size value)
     (create-array type (recur size) (recur value)))
    ((while-loop guard body)
     (while-loop (recur guard) (recur body)))
    ((for-loop id init final body)
     (let* ((env (add-identifier id env))
            (recur (rename env)))
      (for-loop (lookup-identifier id env) (recur init) (recur final) (recur body))))
    ((break) (break))
    ((type-declaration name type) (type-declaration name type))
    ((function-declaration name args return-type body)
     (let ((env (add-identifiers (map (inst car Symbol type) args) env)))
      (: lookup-id (Symbol -> Symbol))
      (define (lookup-id name) (lookup-identifier name env))
      (function-declaration
       (lookup-id name)
       (map (inst cons Symbol type)
        (map lookup-id (map (inst car Symbol type) args))
        (map (inst cdr Symbol type) args))
       return-type
       ((rename env) body))))
    ((variable-declaration sym type value)
     (variable-declaration (lookup-identifier sym env) type (recur value)))
    ((untyped-variable-declaration sym value)
     (untyped-variable-declaration (lookup-identifier sym env) (recur value)))
    ((int-type) (int-type))
    ((string-type) (string-type))
    ((unit-type) (unit-type))
    ((array-type elem-type) (array-type (recur elem-type)))
    ((record-type fields)
     (record-type 
      (map (inst cons Symbol value-type)
       (map (inst car Symbol value-type) fields)
       (map recur (map (inst cdr Symbol value-type) fields)))))
    ((function-type args return)
     (function-type (map recur args) (recur return)))
    ((type-reference name)
     (type-reference (lookup-type name env)))
    ))
       
  recur)
 
 (: lookup-identifier (Symbol environment -> Symbol))
 (define (lookup-identifier sym env)
  (hash-ref (environment-ids env) sym
   (lambda ()
    (error 'lookup-identifier "Unbound Identifier ~a" sym))))


 (: lookup-type (Symbol environment -> Symbol))
 (define (lookup-type sym env)
  (hash-ref (environment-types env) sym
   (lambda ()
    (error 'lookup-identifier "Unbound Identifier ~a" sym))))


 (: add-identifier (Symbol environment -> environment))
 (define (add-identifier sym env)
  (match env
   ((environment id type)
    (environment (hash-set id sym (gensym sym)) type))))


 (: add-type (Symbol environment -> environment))
 (define (add-type sym env)
  (match env
   ((environment id type)
    (environment id (hash-set type sym (gensym sym))))))



 (: add-identifiers ((Listof Symbol) environment -> environment))
 (define (add-identifiers syms env)
  (for/fold: : environment
   ((env : environment env))
   ((sym : Symbol syms))
   (add-identifier sym env)))

 (: extend-environment ((Listof declaration) environment -> environment))
 (define (extend-environment decs env)
  (for/fold: : environment
    ((env : environment env))
    ((dec : declaration decs))
   (match dec
    ((function-declaration name args type body)
     (add-identifier name env))
    ((variable-declaration name type value)
     (add-identifier name env))
    ((untyped-variable-declaration name value)
     (add-identifier name env))
    ((type-declaration name type) (add-type name env)))))



 ((rename
   (environment
    (make-immutable-hash empty)
    (make-immutable-hash empty)))
  prog))




(: type-check (expression -> expression))


(struct: type-environment
 ((ids : (HashTable Symbol type))
  (types : (HashTable Symbol type))))


(define (type-check prog)
 (: rename (type-environment -> (type -> updater)))
 (define (rename env)
  (: assert-type (resolved-type -> updater))
  (define (assert-type type)
   (: recur updater)
   (define (recur prog)
    (match prog
     ((identifier sym) 
      (if (type=? type (lookup-identifier-type sym env))
          (identifier sym)
          (error 'type-error)))
     ((field-ref base field)
      (if 
       (field-ref (recur base) field))
     ((array-ref base index) (array-ref (recur base) (recur index)))
     ((binder declarations body)
      (let ((env (extend-environment declarations env)))
       (binder (map (rename env) declarations) ((rename env) body))))
     ((sequence exprs) (sequence (map recur exprs)))
     ((assignment value expr)
      (assignment (recur value) (recur expr)))
     ((if-then-else c t f)
      (if-then-else (recur c) (recur t) (and f (recur f))))
     ((integer-literal v) (integer-literal v))
     ((string-literal s) (string-literal s))
     ((nil) (nil))
     ((negation expr) (negation (recur expr)))
     ((function-call fun args)
      (function-call (recur fun) (map recur args)))
     ((arithmetic op left right)
      (arithmetic op (recur left) (recur right)))
     ((comparison op left right)
      (comparison op (recur left) (recur right)))
     ((create-record type fields)
      (create-record type
        (map (inst cons Symbol expression)
             (map (inst car Symbol expression) fields)
             (map recur (map (inst cdr Symbol expression) fields)))))
     ((create-array type size value)
      (create-array type (recur size) (recur value)))
     ((while-loop guard body)
      (while-loop (recur guard) (recur body)))
     ((for-loop id init final body)
      (let* ((env (add-identifier id env))
             (recur (rename env)))
       (for-loop (lookup-identifier id env) (recur init) (recur final) (recur body))))
     ((break) (break))
     ((type-declaration name type) (type-declaration name type))
     ((function-declaration name args return-type body)
      (let ((env (add-identifiers (map (inst car Symbol type) args) env)))
       (: lookup-id (Symbol -> Symbol))
       (define (lookup-id name) (lookup-identifier name env))
       (function-declaration
        (lookup-id name)
        (map (inst cons Symbol type)
         (map lookup-id (map (inst car Symbol type) args))
         (map (inst cdr Symbol type) args))
        return-type
        ((rename env) body))))
     ((variable-declaration sym type value)
      (variable-declaration (lookup-identifier sym env) type (recur value)))
     ((untyped-variable-declaration sym value)
      (error 'todo))
     ((int-type) (int-type))
     ((string-type) (string-type))
     ((unit-type) (unit-type))
     ((array-type elem-type) (array-type (recur elem-type)))
     ((record-type fields)
      (record-type 
       (map (inst cons Symbol value-type)
        (map (inst car Symbol value-type) fields)
        (map recur (map (inst cdr Symbol value-type) fields)))))
     ((function-type args return)
      (function-type (map recur args) (recur return)))
     ((type-reference name) (type-reference name))
     ))
        
   recur)
  assert-type)
 
 (: lookup-identifier-type (Symbol environment -> resolved-type))
 (define (lookup-identifier-type sym env)
  (hash-ref (environment-ids env) sym
   (lambda ()
    (error 'lookup-identifier "Unbound Identifier ~a" sym))))


 (: resolve-type (type environment -> resolved-type))
 (define (resolve-type sym env)
  (hash-ref (environment-types env) sym
   (lambda ()
    (error 'lookup-identifier "Unbound Identifier ~a" sym))))


 (: add-identifier (Symbol type environment -> environment))
 (define (add-identifier sym type env)
  (match env
   ((environment ids types)
    (environment (hash-set ids sym type) types))))


 (: add-type (Symbol type environment -> environment))
 (define (add-type sym type env)
  (match env
   ((environment ids types)
    (environment ids (hash-set types sym type)))))



 (: add-identifiers ((Listof (Pair Symbol type)) environment -> environment))
 (define (add-identifiers syms env)
  (for/fold: : environment
   ((env : environment env))
   ((sym : (Pair Symbol type) syms))
   (add-identifier (car sym) (cdr sym) env)))

 (: extend-environment ((Listof declaration) environment -> environment))
 (define (extend-environment decs env)
  (for/fold: : environment
    ((env : environment env))
    ((dec : declaration decs))
   (match dec
    ((function-declaration name args type body)
     (add-identifier name env))
    ((variable-declaration name type value)
     (add-identifier name env))
    ((untyped-variable-declaration name value)
     (add-identifier name env))
    ((type-declaration name type) (add-type name env)))))



 ((rename
   (environment
    (make-immutable-hash empty)
    (make-immutable-hash empty)))
  prog))





