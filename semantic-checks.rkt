#lang typed/racket/base

(require racket/match racket/list)

(require "source-ast.rkt" "core-ast.rkt")
(provide rename-variables  type-check)




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
    ((math op left right)
     (math op (recur left) (recur right)))
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













(: record-type-has-field? (record-type Symbol -> Boolean))
(define (record-type-has-field? type sym)
 (ormap (lambda: ((field : (Pair Symbol value-type)))
          (equal? sym (car field)))
  (record-type-fields type)))
 
 
(define (function-declaration->function-type dec)
 (error 'type-check "Not yet implemented"))


(: type-check (expression -> expression))


(struct: type-environment
 ((ids : (HashTable Symbol resolved-type))
  (types : (HashTable Symbol resolved-type))))


(define (type-check prog)
 (define-type pos-type (U 'nil resolved-type))
 (define-type updater
  (case-lambda
   (lvalue -> (values lvalue resolved-type))
   (declaration -> declaration)
   (expression -> (values expression pos-type))))


 (: rename (type-environment -> updater))
 (define (rename env)
  (: recur updater)
  (define (recur prog)
   (match prog
    ((identifier sym) 
     (values prog (lookup-identifier-type sym env)))
    ((field-ref base field)
     (let-values (((base type) (recur base)))
      (if (and (record-type? type) (record-type-has-field? type field))
          (values (field-ref base field) type)
          (error 'type-check "Expression ~a of type ~a has no field ~a" base type field))))
    ((array-ref base index)
     (let-values (((base a-type) (recur base)))
      (if (array-type? a-type)
       (let-values (((index i-type) (recur index)))
        (if (int-type? index)
            (values (array-ref base index) (array-type-elem-type a-type))
            (error 'type-check "Expression ~a is not of type int" index)))
       (error 'type-check "Expression ~a is not of an array type" base))))
    ((binder declarations body)
     (let ((env (extend-environment declarations env)))
      (let ((recur (rename env)))
       (let-values (((body type) (recur body)))
        (values (binder (map recur declarations) body) type)))))
    ((sequence exprs) 
     (: rec ((Listof expression) -> (values (Listof expression) pos-type)))
     (define (rec exprs)
      (if (empty? (rest exprs))
          (let-values (((expr type) (recur (first exprs))))
           (values (list expr) type))
          (let-values (((rec-expr _) (recur (first exprs)))
                       ((rec-exprs type) (rec (rest exprs))))
           (values (cons rec-expr rec-exprs) type))))
      
     (cond 
      ((empty? exprs) (values (sequence empty) (unit-type)))
      (else
       (let-values (((exprs type) (rec exprs)))
        (values (sequence exprs) type)))))
    ((assignment value expr)
     (let-values (((value v-type) (recur value))
                  ((expr e-type) (recur expr)))
      (if (equal? v-type e-type)
          (values (assignment value expr) e-type)
          (error 'type-check "Assignment to ~a of type ~a is of different type than ~a of type ~a"
           value v-type expr e-type))))
    ((if-then-else c t f)
     (let-values (((c c-type) (recur c)))
      (if (int-type? c)
       (let-values (((t t-type) (recur t))
                    ((f f-type) (recur f)))
        (if (equal? t-type f-type)
            (values (if-then-else c t f) t-type)
            (error 'type-check "The different branches of a conditional ~a and ~a have different types ~a and ~a"
              t f t-type f-type)))
       (error 'type-check "The condition of a conditional ~a had type ~a instead of int" c c-type))))
    ((integer-literal v) (values prog (int-type)))
    ((string-literal s) (values prog (string-type)))
    ((nil) (values prog 'nil))
    ((negation expr)
     (let-values (((expr type) (recur expr)))
      (if (int-type? type)
          (values (negation expr) type)
          (error 'type-check "The expression of a negation ~a had type ~a instead of int" expr type))))
    ((function-call fun args)
     (error 'type-check "Not yet implemented"))
    ((math op left right)
     (let-values (((left  l-type) (recur left))
                  ((right r-type) (recur right)))
      (if (int-type? l-type)
          (if (int-type? r-type)
              (values (math op left right) l-type)
              (error 'type-check "The right expression of the math operation ~a, ~a had type ~a instead of int" op right r-type))
          (error 'type-check "The left expression of the math operation ~a, ~a had type ~a instead of int" op left l-type))))
    ((create-record type fields)
     (error 'type-check "Not yet implemented"))
    ((create-array type size value)
     (let-values (((size s-type) (recur size))
                  ((value v-type) (recur value))
                  ((a-type) (resolve-type type env)))
      (if (array-type? a-type)
          (if (int-type? s-type)
              (if (equal? v-type (array-type-elem-type a-type))
                  (values (create-array type size value) a-type)
                  (error 'type-check "Inital value ~a in array creation has type ~a which does not match array type ~a"
                    value v-type a-type))
              (error 'type-check "Size for array ~a has type ~a instead of int" size s-type))
          (error 'type-check "Type for create array ~a is not an array type" a-type))))
    ((while-loop guard body)
     (let-values (((guard g-type) (recur guard))
                  ((body b-type) (recur body)))
      (if (int-type? g-type)
          (if (unit-type? b-type)
              (values (while-loop guard body) (unit-type))
              (error 'type-check "While loop body ~a has type ~a instead of unit-type" body b-type))
          (error 'type-check "While loop condition ~a has type ~a instead of int-type" guard g-type))))
    ((for-loop id init final body)
     (error 'type-check "Not yet implemented"))
    ((break) (values (break) (unit-type)))
    ((type-declaration name type) prog)
    ((function-declaration name args return-type body)
     (error 'type-check "Not yet implemented")
#;
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
     (error 'type-check "Not yet implemented"))
    ((untyped-variable-declaration sym value)
     (error 'type-check "Not yet implemented"))
    ))
       
  recur)
  
 
 (: lookup-identifier-type (Symbol type-environment -> resolved-type))
 (define (lookup-identifier-type sym env)
  (hash-ref (type-environment-ids env) sym
   (lambda ()
    (error 'lookup-identifier "Unbound Identifier ~a" sym))))

 (: resolve-type (type type-environment -> resolved-type))
 (define (resolve-type type env)
  (if (type-reference? type)
      (let ((sym (type-reference-name type)))
       (hash-ref (type-environment-ids env) sym
        (lambda ()
         (error 'lookup-identifier "Unbound Identifier ~a" sym))))
      type))

 (: add-identifier (Symbol type type-environment -> type-environment))
 (define (add-identifier sym type env)
  (match env
   ((type-environment ids types)
    (type-environment (hash-set ids sym (resolve-type type env)) types ))))


 (: add-type (Symbol type type-environment -> type-environment))
 (define (add-type sym type env)
  (match env
   ((type-environment ids types)
    (type-environment ids (hash-set types sym (resolve-type type env))))))



 (: add-identifiers ((Listof (Pair Symbol type)) type-environment -> type-environment))
 (define (add-identifiers syms env)
  (for/fold: : type-environment
   ((env : type-environment env))
   ((sym : (Pair Symbol type) syms))
   (add-identifier (car sym) (cdr sym) env)))

 (: extend-environment ((Listof declaration) type-environment -> type-environment))
 (define (extend-environment decs env)
  (for/fold: : type-environment
    ((env : type-environment env))
    ((dec : declaration decs))
   (match dec
    ((function-declaration name args type body)
     (add-identifier name (function-declaration->function-type dec) env))
    ((variable-declaration name type value)
     (add-identifier name type env))
    ((untyped-variable-declaration name value)
     (error 'type-check "Not yet implemented"))
    ((type-declaration name type) (add-type name type env)))))



 (let-values (((prog type)
    ((rename
      (type-environment
       (make-immutable-hash empty)
       (make-immutable-hash empty)))
     prog)))
  prog))




