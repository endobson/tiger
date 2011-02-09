#lang typed/racket/base

(require racket/match racket/list)
(require "source-ast.rkt" "core-ast.rkt" "environment.rkt")


(provide type-check)


(: record-type-has-field? (record-type Symbol -> Boolean))
(define (record-type-has-field? type sym)
 (ormap (lambda: ((field : (Pair Symbol value-type)))
          (equal? sym (car field)))
  (record-type-fields type)))



(: function-declaration->function-type (function-declaration -> function-type)) 
(define (function-declaration->function-type dec)
 (match dec
  ((function-declaration name args type body)
   (function-type (map (inst cdr Symbol value-type) args) type))))



(: span (All (a b) ((a -> Any : b) (Listof a) -> (values (Listof b) (Listof a)))))
(define (span f list)
 (if (empty? list) (values empty empty)
  (let ((elem (first list)))
   (if (f elem)
       (let-values (((f r) (span f (rest list))))
        (values (cons elem f) r))
       (values empty list)))))
   
(: map2 (All (a b c) ((a -> (values b c)) (Listof a) -> (values (Listof b) (Listof c)))))
(define (map2 f lst)
 (cond
  ((empty? lst) (values empty empty))
  (else
   (let-values (((b c) (f (first lst)))
                ((bs cs) (map2 f (rest lst))))
    (values (cons b bs) (cons c cs))))))




(: type-check (expression type-environment -> expression))
(define (type-check prog env)
 (define-type pos-type (U 'nil resolved-type))
 (define-type updater
  (case-lambda
   (lvalue -> (values lvalue resolved-type))
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
     (let-values (((declarations env) (extend-environment declarations env)))
      (let ((recur (rename env)))
       (let-values (((body type) (recur body)))
        (values (binder declarations body) type)))))
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
      (if (int-type? c-type)
       (let-values (((t t-type) (recur t))
                    ((f f-type) (if f (recur f) (values (sequence empty) (unit-type)))))
        (if (equal? t-type f-type)
            (values (if-then-else c t f) t-type)
            (error 'type-check "The different branches of a conditional ~a and ~a have different types ~a and ~a"
              t f t-type f-type)))
       (error 'type-check "The condition of a conditional ~a had type ~a instead of type int" c c-type))))
    ((integer-literal v) (values prog (int-type)))
    ((string-literal s) (values prog (string-type)))
    ((nil) (values prog 'nil))
    ((negation expr)
     (let-values (((expr type) (recur expr)))
      (if (int-type? type)
          (values (negation expr) type)
          (error 'type-check "The expression of a negation ~a had type ~a instead of int" expr type))))
    ((function-call fun args)
     (let-values (((fun fun-type) (recur fun)) ((args arg-types) (map2 recur args)))
      (if (function-type? fun-type)
       (if (equal? (map (resolve-type* env) (function-type-args fun-type))
                   arg-types)
           (values (function-call fun args) (resolve-type (function-type-return fun-type) env))
           (error 'type-check "Arguments ~a of types ~a did not match function argument types of ~a"
             args arg-types fun-type))
       (error 'type-check "Function expression ~a of type ~a is not of a function type" fun fun-type))))
    ((math op left right)
     (let-values (((left  l-type) (recur left))
                  ((right r-type) (recur right)))
      (if (int-type? l-type)
          (if (int-type? r-type)
              (values (math op left right) l-type)
              (error 'type-check "The right expression of the math operation ~a, ~a had type ~a instead of int" op right r-type))
          (error 'type-check "The left expression of the math operation ~a, ~a had type ~a instead of int" op left l-type))))
    ((create-record type fields)
     (error 'type-check "Not yet implemented create-record"))
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
     (error 'type-check "Not yet implemented for-loop"))
    ((break) (values (break) (unit-type)))
    ((type-declaration name type) prog)
    ((function-declaration name args return-type body)
     (error 'type-check "Not yet implemented function-declaration"))
    ((variable-declaration sym type value)
     (error 'type-check "Not yet implemented variable-declaration"))
    ((untyped-variable-declaration sym value)
     (let-values (((value type) (recur value)))
      (if (equal? 'nil type)
          (error 'type-check "Untyped variable declaration has nil-type, ~a" value)
          (if (unit-type? type)
              (error 'type-check "Untyped variable declaration has unit-type, ~a" value)
              (variable-declaration sym type value)))))
    ))
       
  recur)
  
 
 (: lookup-identifier-type (Symbol type-environment -> resolved-type))
 (define (lookup-identifier-type sym env)
  (hash-ref (type-environment-ids env) sym
   (lambda ()
    (error 'lookup-identifier "Unbound Identifier ~a" sym))))

 (: resolve-type* (type-environment -> (type -> resolved-type)))
 (define (resolve-type* env)
  (lambda: ((t : type)) (resolve-type t env)))

 (: add-identifier (Symbol type type-environment -> type-environment))
 (define (add-identifier sym type env)
  (match env
   ((type-environment ids types)
    (type-environment (hash-set ids sym (resolve-type type env)) types ))))


 (: add-type (Symbol value-type type-environment -> type-environment))
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

 (: extend-environment ((Listof declaration) type-environment -> (values (Listof declaration) type-environment)))
 (define (extend-environment decs env)
  (cond
   ((empty? decs) (values empty env))
   (else
    (let ((dec (first decs)))
     (match dec
      ((function-declaration name args type body)
       (let-values (((fun-decs decs) (span function-declaration? decs)))
        (let-values (((fun-decs env) (check-function-declarations fun-decs env)))
         (let-values (((decs env) (extend-environment decs env)))
          (values (append fun-decs decs) env)))))
      ((variable-declaration name type value)
       (let-values (((value v-type) ((rename env) value)))
        (if (equal? (resolve-type type env) v-type)
            (let-values (((decs env) (extend-environment (rest decs) (add-identifier name type env))))
             (values (cons (variable-declaration name type value) decs) env))
            (error 'type-check "Variable declaration type ~a does not match type of expression ~a" type v-type))))
      ((untyped-variable-declaration name value)
       (let-values (((value v-type) ((rename env) value)))
        (if (equal? 'nil v-type)
            (error 'type-check "Untyped variable declaration expression ~a has nil-type" value)
            (if (unit-type? v-type)
                (error 'type-check "Untyped variable declaration expression ~a has unit-type" value)
                (let-values (((decs env) (extend-environment (rest decs) (add-identifier name v-type env))))
                 (values (cons (variable-declaration name v-type value) decs) env))))))
      ((type-declaration name type)
       (let-values (((type-decs decs) (span type-declaration? decs)))
        (let-values (((type-decs env) (check-type-declarations type-decs env)))
         (let-values (((decs env) (extend-environment decs env)))
          (values (append type-decs decs) env))))))))))



 (: check-function-declarations ((Listof function-declaration) type-environment -> (values (Listof function-declaration) type-environment)))
 (define (check-function-declarations decs env)
  (let ((env (add-identifiers
         (map (inst cons Symbol type)
              (map function-declaration-name decs)
              (map function-declaration->function-type decs)) env)))
    (: fix-function-declaration (function-declaration -> function-declaration))
    (define (fix-function-declaration dec)
     (match dec
      ((function-declaration name args type body)
       (let-values (((body b-type) ((rename (add-identifiers args env)) body)))
        (if (equal? b-type (resolve-type type env))
            (function-declaration name args type body)
            (error 'type-check "Bad function declaration body, should have type ~a has type ~a" type b-type))))))

    (values (map fix-function-declaration decs) env)))



 (: check-type-declarations ((Listof type-declaration) type-environment -> (values (Listof type-declaration) type-environment)))
 (define (check-type-declarations types env)
  (define-type dag (HashTable Symbol (U Symbol #f)))
  (: compute-reference-dag ((Listof type-declaration) dag -> dag))
  (define (compute-reference-dag types dag)
   (cond
    ((empty? types) dag)
    (else
     (let ((ref (first types)) (types (rest types)))
      (match ref
       ((type-declaration name type)
        (compute-reference-dag types 
         (match type
          ((type-reference ref-name)
           (hash-set dag name ref-name))
          (else (hash-set dag name #f))))))))))

  (: cycle-exists? (Symbol dag -> Boolean))
  (define (cycle-exists? symbol a-dag)
   (: recur (Symbol dag (HashTable Symbol #t) -> Boolean))
   (define (recur symbol dag visited)
    (if (hash-has-key? visited symbol)
        #t
        (let ((sym (hash-ref dag symbol (lambda () #f))))
         (and sym
             (recur sym dag (hash-set visited symbol #t))))))
   (recur symbol a-dag (make-immutable-hash empty)))

  (: make-table ((Listof type-declaration) -> (HashTable Symbol type-declaration)))
  (define (make-table decls)
   (make-immutable-hash (map (inst cons Symbol type-declaration) (map type-declaration-name decls) decls)))


  (: make-ordered (dag (Listof type-declaration) (HashTable Symbol type-declaration) -> (Listof type-declaration)))
  (define (make-ordered dag types table)
   (: recur (type-declaration (Listof type-declaration) -> (Listof type-declaration)))
   (define (recur dec acc)
    (if (member dec acc)
        acc
        (cons dec
         (match dec
          ((type-declaration name ty)
           (let ((ref (hash-ref dag name (lambda () #f))))
            (if ref
                (let ((next-dec (hash-ref table ref (lambda () #f))))
                 (if next-dec (recur next-dec acc) acc))
                acc)))))))

    (foldr recur empty types))

  (: extend (type-declaration type-environment -> type-environment))
  (define (extend dec env)
   (match dec
    ((type-declaration name type)
     (add-type name (resolve-type type env) env))))

  (: normalize (type-environment -> (type-declaration -> type-declaration)))
  (define ((normalize env) dec)
   (match dec
    ((type-declaration name ty)
     (type-declaration name (resolve-type ty env)))))
   

  (: reference-dag dag)
  (: cycle Boolean)
  (: ordered (Listof type-declaration))
  (: fixed-decls (Listof type-declaration))
  (: final-env type-environment )
  (define reference-dag (compute-reference-dag types (make-immutable-hash empty)))
  (define cycle 
   (for/fold: : Boolean
    ((result : Boolean #f))
    ((dec : type-declaration types))
    (or result 
     (match dec
      ((type-declaration name ty)
       (cycle-exists? name reference-dag))))))
  (when cycle
   (error 'type-check "Cycle in type declarations ~a" types))
  (define ordered (reverse (make-ordered reference-dag types (make-table types))))
  (define final-env (foldl extend env ordered))
  (define fixed-decls (map (normalize final-env) ordered))
  (values fixed-decls final-env))




     






 (let-values (((prog type) ((rename env) prog)))
   prog))


