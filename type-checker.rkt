#lang typed/racket/base

(require racket/match racket/list)
(require "source-ast.rkt" "core-ast.rkt" "environment.rkt" "external-functions.rkt" "unique.rkt")
(require
 (only-in "types.rkt"
  string-type int-type unit-type 
  String-Type Int-Type Unit-Type
  string-type? int-type? unit-type?
  (function-type other:function-type)
  (function-type-arg-types other:function-type-arg-types)
  (function-type-return-type other:function-type-return-type)))


(provide type-check global-type-environment)


(define-type type (U primitive-type compound-type))
(define-type primitive-type (U Int-Type String-Type Unit-Type))
(define-type value-type (U String-Type Int-Type compound-type))


(: assert-unique (Any -> unique))
(define (assert-unique v)
 (unless (unique? v) (eprintf "Assert unique failed in typecheck with ~a~n" v))
 (assert v unique?))



(struct: type-environment
 ((ids : (HashTable unique value-type))
  (types : (HashTable unique value-type))) #:transparent)


(: global-type-environment type-environment)
(define global-type-environment
 (let* ((string (type-reference (hash-ref global-type-names 'string)))
        (int (type-reference (hash-ref global-type-names 'int)))
        (extract-name (lambda (ty)
         (cond ((int-type? ty) int) ((string-type? ty) string) (else (error 'extract-name)))))
        (extract-name2 (lambda (ty)
         (cond ((int-type? ty) int) ((string-type? ty) string) ((unit-type? ty) #f) (else (error 'extract-name))))))
  (type-environment
   (make-immutable-hash
    (hash-map external-function-database
     (lambda: ((key : Symbol) (val : other:function-type))
      (cons (hash-ref global-id-names key)
            (function-type (map extract-name (other:function-type-arg-types val))
                           (extract-name2 (other:function-type-return-type val))))))
    )
   (make-immutable-hash
    (list
     (cons (hash-ref global-type-names 'int) int-type)
     (cons (hash-ref global-type-names 'string) string-type))))))



(: unresolve-type (value-type type-environment -> type-reference))
(define (unresolve-type type type-env)
 (let/ec: escape : type-reference
  (hash-for-each (type-environment-types type-env)
   (lambda: ((name : unique) (atype : value-type))
    (when (type-equal? atype type type-env) (escape (type-reference name)))))
  (error 'unresolve-type "No name for type ~a in ~a" type type-env)))


(: resolve-type (type-reference type-environment -> value-type))
(define (resolve-type type env)
 (if (type-reference? type)
     (let ((sym (type-reference-name type)))
      (hash-ref (type-environment-types env) (assert-unique sym)
       (lambda ()
        (error 'resolve-type "Unbound type name ~a in ~a" sym env))))
     type))

(: sub-type? ((U type-reference type 'nil) (U type-reference type 'nil) type-environment -> Boolean))
(define (sub-type? t1 t2 env)
 (cond
  ((type-reference? t1) (sub-type? (resolve-type t1 env) t2 env))
  ((type-reference? t2) (sub-type? t1 (resolve-type t2 env) env))
  ((equal? 'nil t1) (record-type? t2))
  (else (type-equal? t1 t2 env))))

(: type-equal? ((U type-reference type 'nil) (U type-reference type 'nil) type-environment -> Boolean))
(define (type-equal? t1 t2 env)
 (cond
  ((type-reference? t1) (type-equal? (resolve-type t1 env) t2 env))
  ((type-reference? t2) (type-equal? t1 (resolve-type t2 env) env))
  ((equal? 'nil t1) (equal? 'nil t2))
  ((unit-type? t1) (unit-type? t2))
  ((int-type? t1) (int-type? t2))
  ((string-type? t1) (string-type? t2))
  ((array-type? t1) (equal? t1 t2))
  ((record-type? t1) (equal? t1 t2))
  ((function-type? t1)
   (and (function-type? t2)
    (match t1
     ((function-type args1 return1)
      (match t2
       ((function-type args2 return2)
        (and
         (if return1 (and return2 (type-equal? return1 return2 env))
             (not return2))
         (= (length args1) (length args2))
         (andmap (lambda: ((arg1 : (U type-reference type)) (arg2 : (U type type-reference))) (type-equal? arg1 arg2 env)) args1 args2))))))))))






(: record-type-has-field? (record-type Symbol -> Boolean))
(define (record-type-has-field? type sym)
 (ormap (lambda: ((field : (Pair Symbol type-reference)))
          (equal? sym (car field)))
  (record-type-fields type)))


(: record-field-type (record-type Symbol -> type-reference))
(define (record-field-type type sym)
 (or
  (ormap (lambda: ((field : (Pair Symbol type-reference)))
           (and (equal? sym (car field)) (cdr field)))
   (record-type-fields type))
  (error 'record-field-type "Record type ~a has no field of name ~a" type sym)))






(: function-declaration->function-type (function-declaration -> function-type)) 
(define (function-declaration->function-type dec)
 (match dec
  ((function-declaration name args type body)
   (function-type (map (inst cdr (U Symbol unique) type-reference) args) type))))



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
 (define-type pos-type (U 'nil type))
 (define-type updater
  (case-lambda
   (lvalue -> (values lvalue type))
   (expression -> (values expression pos-type))))


 (: type-check (type-environment -> updater))
 (define (type-check env)
  (: recur updater)
  (define (recur prog)
   (match prog
    ((identifier sym) 
     (values prog (lookup-identifier-type sym env)))
    ((field-ref base field ty)
     (let-values (((base type) (recur base)))
      (if (if ty (type-equal? ty type env) #t)
          (if (and (record-type? type) (record-type-has-field? type field) )
              (values (field-ref base field (unresolve-type type env)) (resolve-type (record-field-type type field) env))
              (error 'type-check "Expression ~a of type ~a has no field ~a" base type field))
          (error 'type-check "Annotated type ~a does not match actual type ~a" ty type))))
    ((array-ref base index ty)
     (let-values (((base a-type) (recur base)))
      (if (if ty (type-equal? ty a-type env) #t)
          (if (array-type? a-type)
           (let-values (((index i-type) (recur index)))
            (if (int-type? i-type)
                (values (array-ref base index (unresolve-type a-type env)) (resolve-type (array-type-elem-type a-type) env))
                (error 'type-check "Expression ~a is not of type int" index)))
           (error 'type-check "Expression ~a is not of an array type" base))
          (error 'type-check "Annotated type ~a does not match actual type ~a" ty a-type))))
    ((binder declarations body)
     (let-values (((declarations env) (extend-environment declarations env)))
      (let ((recur (type-check env)))
       (let-values (((body type) (recur body)))
        (values (binder declarations body) type)))))
    ((sequence exprs) 
     (: rec ((Listof expression) -> (values expression pos-type)))
     (define (rec exprs)
      (if (empty? (rest exprs))
          (let-values (((expr type) (recur (first exprs))))
           (values expr type))
          (let-values (((rec-expr1 type1) (recur (first exprs)))
                       ((rec-expr2 type2) (rec (rest exprs))))
           (cond
            ((unit-type? type1)
               (values (sequence (list rec-expr1 rec-expr2)) type2))
            ((equal? 'nil type1)
             (error 'type-check "Expression has nil-type and cannot be assigned a type" rec-expr1))
            (else
             (values (binder (list (variable-declaration (gen-uniq 'ignored) (unresolve-type type1 env) rec-expr1)) rec-expr2) type2))))))
     (cond 
      ((empty? exprs) (values (sequence empty) unit-type))
      (else (rec exprs))))
    ((assignment value expr)
     (let-values (((value v-type) (recur value))
                  ((expr e-type) (recur expr)))
      (if (type-equal? v-type e-type env)
          (values (assignment value expr) unit-type)
          (error 'type-check "Assignment to ~a of type ~a is of different type than ~a of type ~a"
           value v-type expr e-type))))
    ((if-then-else c t f ty)
     (if ty
      (error 'type-check "Already annotated conditional")
      (let-values (((c c-type) (recur c)))
       (if (int-type? c-type)
        (let-values (((t t-type) (recur t))
                     ((f f-type) (if f (recur f) (values (sequence empty) unit-type))))
         (: type-error (-> Nothing))
         (define (type-error)
          (error 'type-check "The different branches of a conditional ~a and ~a have different types ~a and ~a"
                  t f t-type f-type))
         (let-values (((r-type-ref r-type)
                (cond
                 ((equal? t-type 'nil)
                  (cond 
                   ((equal? f-type 'nil) (values 'nil 'nil))
                   ((unit-type? f-type) (type-error))
                   ((record-type? f-type) (values (unresolve-type f-type env) f-type))
                   (else (type-error))))
                 ((type-equal? t-type f-type env)
                  (if (unit-type? t-type) (values 'unit unit-type)
                      (values (unresolve-type t-type env) t-type)))
                 (else (type-error)))))
               (values (if-then-else c t f r-type-ref) r-type)))
        (error 'type-check "The condition of a conditional ~a had type ~a instead of type int" c c-type)))))
    ((integer-literal v) (values prog int-type))
    ((string-literal s) (values prog string-type))
    ((nil #f) (values prog 'nil))
    ((nil _) (error 'type-check "Already annotated nil ~a" prog))
    ((negation expr)
     (let-values (((expr type) (recur expr)))
      (if (int-type? type)
          (values (negation expr) type)
          (error 'type-check "The expression of a negation ~a had type ~a instead of int" expr type))))
    ((function-call fun args ty)
     (: maybe-resolve-type ((U type-reference compound-type) -> type))
     (define (maybe-resolve-type ty)
      (if (type-reference? ty) (resolve-type ty env) ty))
     (if ty
      (error 'type-check "Already annotated function call ~a" prog)
      (let-values (((fun fun-type) (recur fun)) ((args arg-types) (map2 recur args)))
       (match fun-type
        ((function-type fun-arg-types fun-return-type)
          (if (and (= (length fun-arg-types) (length arg-types))
                   (andmap (lambda: ((t1 : (U type-reference type)) (t2 : pos-type)) (sub-type? t2 t1 env))
                                       fun-arg-types
                                       arg-types))
              (let ((fun-type-name (gen-uniq 'fun-type)))
               (values (binder (list (type-declaration fun-type-name (function-type fun-arg-types fun-return-type)))
                               (function-call fun args (type-reference fun-type-name)))
                       (if fun-return-type
                           (maybe-resolve-type fun-return-type)
                           unit-type)))
              (error 'type-check "Arguments ~a of types ~a did not match function argument types of ~a"
                args arg-types fun-type)))
        (else (error 'type-check "Function expression ~a of type ~a is not of a function type" fun fun-type))))))
    ((math op left right)
     (let-values (((left  l-type) (recur left))
                  ((right r-type) (recur right)))
      (if (int-type? l-type)
          (if (int-type? r-type)
              (values (math op left right) l-type)
              (error 'type-check "The right expression of the math operation ~a, ~a had type ~a instead of int" op right r-type))
          (error 'type-check "The left expression of the math operation ~a, ~a had type ~a instead of int" op left l-type))))
    ((equality op left right type)
     (if type
         (error "Already annotated equality: ~a" prog)
         (let-values (((left l-type) (recur left))
                      ((right r-type) (recur right)))
          (let: ((res-type : value-type
                 (cond
                  ((and (equal? 'nil l-type) (equal? 'nil r-type))
                   (error 'type-check "Both sides of equality are nil ~a" prog))
                  ((and (equal? 'nil l-type) (record-type? r-type)) r-type)
                  ((and (equal? 'nil r-type) (record-type? l-type)) l-type)
                  ((or (equal? 'nil l-type) (equal? 'nil r-type))
                   (error 'type-check "One side of equality is nil and other isn't a record ~a" prog))
                  ((or (unit-type? l-type) (unit-type? r-type))
                   (error 'type-check "At least one side of equality is unit typed ~a" prog))
                  ((not (equal? l-type r-type))
                   (error 'type-check "Type ~a and ~a cannot be compared" l-type r-type))
                  (else r-type))))
            (values (equality op left right (unresolve-type res-type env)) int-type)))))
    ((create-record type fields)
     (: check-field ((Pair Symbol type-reference) (Pair Symbol expression) -> (Pair Symbol expression)))
     (define (check-field type-pair expr-pair)
      (let ((type-field-name (car type-pair))
            (expr-field-name (car expr-pair))
            (type-type (cdr type-pair))
            (expr-expr (cdr expr-pair)))
       (if (equal? type-field-name expr-field-name)
           (let-values (((expr e-type) (recur expr-expr)))
            (if (or (equal? 'nil e-type) (type-equal? e-type type-type env))
                (cons expr-field-name expr)
                (error 'type-check "The field expression ~a has type ~a instead of ~a" expr e-type type-type)))
           (error 'type-check "The field name is ~a and should be ~a" expr-field-name type-field-name))))

     (let ((r-type (resolve-type type env)))
      (match r-type
       ((record-type ty-fields) 
        (values (create-record type (map check-field ty-fields fields)) r-type)))))
    ((create-array type size value)
     (let-values (((size s-type) (recur size))
                  ((value v-type) (recur value))
                  ((a-type) (resolve-type type env)))
      (if (array-type? a-type)
          (if (int-type? s-type)
              (if (type-equal? v-type (array-type-elem-type a-type) env)
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
              (values (while-loop guard body) unit-type)
              (error 'type-check "While loop body ~a has type ~a instead of unit-type" body b-type))
          (error 'type-check "While loop condition ~a has type ~a instead of int-type" guard g-type))))
    ((for-loop id init final body)
     (let-values (((init i-type) (recur init))
                  ((final f-type) (recur final))
                  ((body b-type) ((type-check (add-identifier id int-type env)) body)))
      (if (int-type? i-type)
          (if (int-type? f-type)
              (if (unit-type? b-type)
                  (values (for-loop id init final body) unit-type)
                  (error 'type-check "Body of for loop ~a has type ~a instead of unit-type" body b-type))
              (error 'type-check "Final expression of for loop ~a has type ~a instead of int-type" final f-type ))
          (error 'type-check "Initial expression of for loop ~a has type ~a instead of unit-type" init i-type))))
    ((break) (values (break) unit-type))))
       
  recur)
  

 (define-type nil-updater 
  (case-lambda 
   (lvalue -> lvalue)
   (expression -> expression)
   (declaration -> declaration)))
 (: nil-annotate (type-environment -> nil-updater))
 (define (nil-annotate env)
  (: pos-fix (type-environment -> (expression type-reference -> expression)))
  (define ((pos-fix env) expr ref)
   (let ((ty (resolve-type ref env)))
    ((search (and (record-type? ty) ref) env) expr)))


  (: fix-declarations ((Listof declaration) type-environment ->
                       (values (Listof declaration) type-environment)))
  (define (fix-declarations decs env)
   (if (empty? decs) (values empty env)
    (let ((dec (first decs)))
     (match dec
      ((function-declaration name args type body)
       (let-values (((fun-decs decs) (span function-declaration? decs)))
        (let-values (((fun-decs env) (fix-function-declarations fun-decs env)))
         (let-values (((decs env) (fix-declarations decs env)))
          (values (append fun-decs decs) env)))))
      ((variable-declaration name type value)
       (let ((value ((search type env) value)))
        (let-values (((decs env) (fix-declarations (rest decs) (add-identifier name (resolve-type type env) env))))
         (values (cons (variable-declaration name type value) decs) env))))
      ((untyped-variable-declaration name value)
       (error 'nil-annotate "Unremoved untyped-variable-declaration"))
      ((type-declaration name type)
       (let-values (((decs env)
                     (fix-declarations (rest decs)
                                       (add-type name
                                                 (if (type-reference? type)
                                                     (resolve-type type env)
                                                     type)
                                                 env))))
        (values (cons dec decs) env)))))))

  (: fix-function-declarations ((Listof function-declaration) type-environment ->
                                (values (Listof function-declaration) type-environment)))
  (define (fix-function-declarations decs env)
   (let ((env (add-identifiers
          (map (inst cons (U Symbol unique) value-type)
               (map function-declaration-name decs)
               (map function-declaration->function-type decs)) env)))
     (: fix-function-declaration (function-declaration -> function-declaration))
     (define (fix-function-declaration dec)
      (match dec
       ((function-declaration name args type body)
        (let ((args-typed (map (lambda: ((pair : (Pair (U Symbol unique) type-reference))) (cons (car pair) (resolve-type (cdr pair) env))) args)))
         (let ((body ((search type (add-identifiers args-typed env)) body)))
          (function-declaration name args type body))))))

    (values (map fix-function-declaration decs) env)))

   

  (: search ((Option type-reference) type-environment -> nil-updater))
  (define (search ref env)
   (: recur nil-updater)
   (define (recur prog)
    (match prog
     ((identifier sym) prog)
     ((field-ref base field ty) (field-ref ((search ty env) base) field ty))
     ((array-ref base index ty) (array-ref ((search #f env) base) ((search #f env) index) ty))
     ((binder declarations body)
       (let-values (((decls env) (fix-declarations declarations env)))
        (binder decls ((search ref env) body))))
     ((sequence exprs)
      (sequence 
       (if ref
        (let ((rexprs (reverse exprs)))
         (reverse (cons (recur (first rexprs)) (map (search #f env) (rest rexprs)))))
        (map recur exprs))))
     ((assignment value expr)
      (let ((ty (type-of value env)))
       (assignment ((search #f env) value)
         ((search (and (record-type? ty) (unresolve-type ty env)) env) expr))))
     ((if-then-else c t f ty)
      (if-then-else ((search #f env) c) (recur t) (and f (recur f)) ty))
     ((integer-literal v) prog)
     ((string-literal s) prog)
     ((nil #f)
      (if ref
          (nil ref)
          (error 'nil-annotate "Nil with unknown type")))
     ((nil _)
      (error 'nil-annotate "Nil already has annotated type"))
     ((negation expr) (negation (recur expr)))
     ((function-call fun args ref)
      (if ref
       (let ((ty (resolve-type ref env)))
        (if (function-type? ty)
            (let ((arg-types (function-type-args ty)))
             (if (andmap type-reference? arg-types)
                 (function-call ((search #f env) fun)
                  (map (pos-fix env) args arg-types) ref)
                 (error 'nil-annotate "Unsimplified function-type")))
            (error 'nil-annotate "Tried to call an expression of non function type")))
       (error 'nil-annotate "Unannotated function call")))
     ((math op left right)
      (math op (recur left) (recur right)))
     ((equality op left right ty)
      (if ty
          (equality op ((search ty env) left) ((search ty env) right) ty)
          (error 'nil-annotate "Unannotated equality remains")))
     ((create-record type fields)
      (let ((rtype (resolve-type type env)))
       (if (record-type? rtype)
           (create-record type
            (map (inst cons Symbol expression)
             (map (inst car Symbol expression) fields)
             (map (pos-fix env)
              (map (inst cdr Symbol expression) fields)
              (map (inst cdr Symbol type-reference) (record-type-fields rtype)))))
           (error 'nil-annotate "Create record tries to create non record type"))))
     ((create-array type size value)
      (create-array type (recur size) (recur value)))
     ((while-loop guard body)
      (while-loop (recur guard) (recur body)))
     ((for-loop id init final body)
      (for-loop id (recur init) (recur final) (recur body)))
     ((break) prog)
     ((type-declaration name type) prog)
     ((function-declaration name args return-type body)
      (let ((args-typed (map (lambda: ((pair : (Pair (U Symbol unique) type-reference)))
                              (cons (car pair) (resolve-type (cdr pair) env))) args)))
       (function-declaration name args return-type
        (let ((env (add-identifiers args-typed env)))
         (if return-type
             ((pos-fix env) body return-type)
             ((search #f env) body))))))
     ((variable-declaration sym type value)
      (variable-declaration sym type 
       ((pos-fix env) value type)))
     ((untyped-variable-declaration sym value)
      (error 'nil-annotate "Remaining untyped-variable-declaration ~a" prog))))
   recur)

   
       
  (search #f env))







 (: type-of (expression type-environment -> pos-type))
 (define (type-of expr env)
  (match expr
   ((identifier sym) (lookup-identifier-type sym env))
   ((field-ref base field ty) (if ty (resolve-type ty env) (error 'type-of "Unannotated field-ref")))
   ((array-ref base index ty) (if ty (resolve-type ty env) (error 'type-of "Unannotated array-ref")))
   ((binder declarations body)
    (error 'type-of "Binder not implemented"))
   ((sequence exprs)
    (if (empty? exprs) unit-type (type-of (last exprs) env)))
   ((assignment value expr)
    (type-of value env))
   ((if-then-else c t f ty)
    (cond
     ((not ty) (type-of t env))
     ((equal? ty 'nil) 'nil)
     ((equal? ty 'unit) unit-type)
     (else (resolve-type ty env))))
   ((integer-literal v) int-type)
   ((string-literal s) string-type)
   ((nil #f) 'nil)
   ((nil _)
    (error 'type-of "Nil already has annotated type"))
   ((negation expr) int-type)
   ((function-call fun args ref)
    (if ref
     (let ((ty (resolve-type ref env)))
      (if (function-type? ty)
          (let ((ret-type (function-type-return ty)))
           (cond
            ((not ret-type) unit-type)
            ((type-reference? ret-type) (resolve-type ret-type env))
            (else ret-type)))
          (error 'type-of "Tried to call an expression of non function type")))
     (error 'type-of "Unannotated function call")))
   ((math op left right) int-type)
   ((create-record type fields) (resolve-type type env))
   ((create-array type size value) (resolve-type type env))
   ((while-loop guard body) unit-type)
   ((for-loop id init final body) unit-type)
   ((break) unit-type)))


 
 (: lookup-identifier-type ((U Symbol unique) type-environment -> value-type))
 (define (lookup-identifier-type sym env)
  (hash-ref (type-environment-ids env) (assert-unique sym)
   (lambda ()
    (error 'lookup-identifier "Unbound Identifier ~a in ~a" sym env))))

 (: resolve-type* (type-environment -> (type-reference -> value-type)))
 (define (resolve-type* env)
  (lambda: ((t : type-reference)) (resolve-type t env)))

 (: add-identifier ((U Symbol unique) value-type type-environment -> type-environment))
 (define (add-identifier sym type env)
  (match env
   ((type-environment ids types)
    (type-environment (hash-set ids (assert-unique sym) type) types ))))


 (: add-type ((U Symbol unique) value-type type-environment -> type-environment))
 (define (add-type sym type env)
  (match env
   ((type-environment ids types)
    (type-environment ids (hash-set types (assert-unique sym) type)))))



 (: add-identifiers ((Listof (Pair (U Symbol unique) value-type)) type-environment -> type-environment))
 (define (add-identifiers syms env)
  (for/fold: : type-environment
   ((env : type-environment env))
   ((sym : (Pair (U Symbol unique) value-type) syms))
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
       (let-values (((value v-type) ((type-check env) value)))
        (let ((r-type (resolve-type type env)))
        (if (sub-type? v-type r-type env)
            (let-values (((decs env) (extend-environment (rest decs) (add-identifier name r-type env))))
             (values (cons (variable-declaration name type value) decs) env))
            (error 'type-check "Variable declaration type ~a does not match type of expression ~a" type v-type)))))
      ((untyped-variable-declaration name value)
       (let-values (((value v-type) ((type-check env) value)))
        (if (equal? 'nil v-type)
            (error 'type-check "Untyped variable declaration expression ~a has nil-type" value)
            (if (unit-type? v-type)
                (error 'type-check "Untyped variable declaration expression ~a has unit-type" value)
                (let-values (((decs env) (extend-environment (rest decs) (add-identifier name v-type env))))
                 (values (cons (variable-declaration name (unresolve-type v-type env) value) decs) env))))))
      ((type-declaration name type)
       (let-values (((type-decs decs) (span type-declaration? decs)))
        (let-values (((type-decs env) (check-type-declarations type-decs env)))
         (let-values (((decs env) (extend-environment decs env)))
          (values (append type-decs decs) env))))))))))



 (: check-function-declarations ((Listof function-declaration) type-environment -> (values (Listof function-declaration) type-environment)))
 (define (check-function-declarations decs env)
  (let ((env (add-identifiers
         (map (inst cons (U Symbol unique) value-type)
              (map function-declaration-name decs)
              (map function-declaration->function-type decs)) env)))
    (: fix-function-declaration (function-declaration -> function-declaration))
    (define (fix-function-declaration dec)
     (match dec
      ((function-declaration name args type body)
       (let ((args-typed (map (lambda: ((pair : (Pair (U Symbol unique) type-reference))) (cons (car pair) (resolve-type (cdr pair) env))) args)))
        (let-values (((body b-type) ((type-check (add-identifiers args-typed env)) body)))
         (if (if type (type-equal? b-type type env)
                 (unit-type? b-type))
             (function-declaration name args type body)
             (error 'type-check "Bad function declaration body, should have type ~a has type ~a" type b-type)))))))

    (values (map fix-function-declaration decs) env)))



 (: check-type-declarations ((Listof type-declaration) type-environment -> (values (Listof type-declaration) type-environment)))
 (define (check-type-declarations types env)
  (define-type dag (HashTable unique (U unique #f)))

  (: simplify-function-types ((Listof type-declaration) -> (Listof type-declaration)))
  (define (simplify-function-types types)
   (if (empty? types) empty
    (let ((type (first types)) (types (rest types)))
     (match type
      ((type-declaration name ty)
       (match ty
        ((function-type args return-type)
         (let ((return-name (gen-uniq 'return))
               (arg-names (map (lambda (_) (gen-uniq 'arg)) args)))
          (: add-return ((Listof type-declaration) -> (Listof type-declaration)))
          (define (add-return decls)
           (if return-type (cons (type-declaration return-name return-type) decls) decls))
          (cons (type-declaration name (function-type (map type-reference arg-names)
                                                      (and return-type (type-reference return-name))))
           (simplify-function-types (add-return
            (append (map type-declaration arg-names args) types))))))
        (else (cons type (simplify-function-types types)))))))))


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
           (hash-set dag (assert-unique name) (assert-unique ref-name)))
          (else (hash-set dag (assert-unique name) #f))))))))))

  (: cycle-exists? (unique dag -> Boolean))
  (define (cycle-exists? symbol a-dag)
   (: recur (unique dag (HashTable unique #t) -> Boolean))
   (define (recur symbol dag visited)
    (if (hash-has-key? visited symbol)
        #t
        (let ((sym (hash-ref dag symbol (lambda () #f))))
         (and sym
             (recur sym dag (hash-set visited symbol #t))))))
   (recur symbol a-dag (make-immutable-hash empty)))

  (: make-table ((Listof type-declaration) -> (HashTable unique type-declaration)))
  (define (make-table decls)
   (make-immutable-hash (map (inst cons unique type-declaration) (map assert-unique (map type-declaration-name decls)) decls)))


  (: make-ordered (dag (Listof type-declaration) (HashTable unique type-declaration) -> (Listof type-declaration)))
  (define (make-ordered dag types table)
   (: recur (type-declaration (Listof type-declaration) -> (Listof type-declaration)))
   (define (recur dec acc)
    (if (member dec acc)
        acc
        (cons dec
         (match dec
          ((type-declaration name ty)
           (let ((ref (hash-ref dag (assert-unique name) (lambda () #f))))
            (if ref
                (let ((next-dec (hash-ref table ref (lambda () #f))))
                 (if next-dec (recur next-dec acc) acc))
                acc)))))))

    (foldr recur empty types))

  (: extend (type-declaration type-environment -> type-environment))
  (define (extend dec env)
   (match dec
    ((type-declaration name type)
     (add-type name (if (type-reference? type) (resolve-type type env) type) env))))

   

  (let* ((types (simplify-function-types types))
         (reference-dag (compute-reference-dag types (make-immutable-hash empty)))
         (cycle 
          (for/fold: : Boolean
           ((result : Boolean #f))
           ((dec : type-declaration types))
           (or result 
            (match dec
             ((type-declaration name ty)
              (cycle-exists? (assert-unique name) reference-dag)))))))
   (when cycle
    (error 'type-check "Cycle in type declarations ~a" types))
   (: ordered (Listof type-declaration))
   (: final-env type-environment )
   (define ordered (reverse (make-ordered reference-dag types (make-table types))))
   (define final-env (foldl extend env ordered))
   (values ordered final-env)))




     






 (let-values (((prog type) ((type-check env) prog)))
   ((nil-annotate env) prog)))



