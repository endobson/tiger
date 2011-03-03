#lang typed/racket/base





(require

         (prefix-in inter: "types.rkt")
         (prefix-in source: "source-ast.rkt")
         (prefix-in source: "core-ast.rkt")
         (prefix-in inter: "intermediate-ast.rkt")
         "environment.rkt" "unique.rkt"
         "primop.rkt" "external-functions.rkt")


(require racket/list racket/match)

(provide transform global-env global-type-env)

(: assert-unique (Any -> unique))
(define (assert-unique v)
 (assert v unique?))



(: span (All (a b) ((a -> Any : b) (Listof a) -> (values (Listof b) (Listof a)))))
(define (span f list)
 (if (empty? list) (values empty empty)
  (let ((elem (first list)))
   (if (f elem)
       (let-values (((f r) (span f (rest list))))
        (values (cons elem f) r))
       (values empty list)))))

(: global-type-env (HashTable unique inter:type))
(define global-type-env
 (make-immutable-hash
  (list (cons (hash-ref global-type-names 'int) inter:int-type) (cons (hash-ref global-type-names 'string) inter:string-type))))



(: exit-primop runtime-primop)
(define exit-primop
 (runtime-primop (inter:make-function-type (list inter:int-type) inter:unit-type) 'exit))

(: global-env (HashTable unique runtime-primop))
(define global-env
 (make-immutable-hash
  (hash-map external-function-database
   (lambda: ((name : Symbol) (type : inter:function-type))
    (cons (hash-ref global-id-names name) (runtime-primop type name))))))




(: transform  (source:expression (HashTable unique runtime-primop) (HashTable unique inter:type) -> inter:expression))
(define (transform prog global-env global-type-env)
 (: trans ((HashTable unique #t) (HashTable unique inter:type) -> (source:expression -> inter:expression)))
 (define (trans env type-env)
  (: recur (source:expression  -> inter:expression))
  (define (recur prog)
   (match prog
    ((source:identifier name) 
     (let ((name (assert-unique name)))
      (if (hash-has-key? env name) (inter:identifier name) 
       (inter:primop-expr
        (hash-ref global-env name
         (lambda () (error 'transform "Unbound identifier ~a in ~a and ~a" name env global-env)))
        empty))))
    ((source:math symbol left right)
     (inter:primop-expr (math-primop symbol) (list (recur left) (recur right))))
    ((source:equality symbol left right type)
     (if type
         (inter:primop-expr (equality-primop (equal? symbol '=) (lookup-type-reference type type-env)) (list (recur left) (recur right)))
         (error 'transform "Unannotated equality: ~a" prog)))
    ((source:negation expr)
     (inter:primop-expr (math-primop '-) (list (inter:primop-expr (integer-constant-primop 0) empty) (recur expr))))
    ((source:function-call fun args ref)
     (if ref
      (let ((ty (lookup-type-reference ref type-env)))
       (if (inter:function-type? ty)
        (inter:primop-expr (call-closure-primop ty)
         (map (lambda: ((expr : source:expression)) (recur expr)) (cons fun args)))
        (error 'transform "Annotated function-call with non function type ~a" prog)))
      (error 'transform "Unannotated function-call ~a" prog)))
    ((source:sequence exprs)
     (cond
      ((empty? exprs) (inter:primop-expr (unit-primop) empty))
      ((= (length exprs) 2)
       (inter:bind (gen-uniq 'ignored) inter:unit-type (recur (first exprs)) (recur (second exprs))))
      (else (error 'transform "Bad sequence ~a" prog))))
    ((source:assignment lvalue expr)
     (let ((val (recur expr)))
      (match lvalue
       ((source:identifier name)
        (inter:assignment (assert-unique name) val))
       ((source:field-ref base field type)
        (if type
            (let ((r-type (lookup-type-reference type type-env)))
             (if (inter:record-type? r-type)
                 (inter:primop-expr (field-set!-primop r-type field) (list (recur base) val))
                 (error 'transform "Field-reference of non record-type")))
            (error 'transform "Unelaborated field-ref")))
       ((source:array-ref base index type)
        (if type
            (let ((r-type (lookup-type-reference type type-env)))
             (if (inter:array-type? r-type)
                 (inter:primop-expr (array-set!-primop r-type) (list (recur base) (recur index) val))
                 (error 'transform "Array-reference of non array-type")))
            (error 'transform "Untyped array-ref"))))))
    ((source:field-ref base field type)
     (if type
         (let ((r-type (lookup-type-reference type type-env)))
          (if (inter:record-type? r-type)
              (inter:primop-expr (field-ref-primop r-type field) (list (recur base)))
              (error 'transform "Field-reference of non record-type")))
         (error 'transform "Untyped field-ref")))
    ((source:array-ref base index type)
     (if type
         (let ((r-type (lookup-type-reference type type-env)))
          (if (inter:array-type? r-type)
              (inter:primop-expr (array-ref-primop r-type) (list (recur base) (recur index)))
              (error 'transform "Array-reference of non array-type")))
         (error 'transform "Untyped array-ref")))
    ((source:if-then-else c t f ty)
     (if ty
         (if f
             (let ((r-type 
                    (cond 
                     ((equal? 'nil ty) (error 'transform "Nil typed if remains"))
                     ((equal? 'unit ty) inter:unit-type)
                     (else (lookup-type-reference ty type-env)))))
              (inter:conditional (recur c) (recur t) (recur f) r-type))
             (error 'transform "One armed if remains"))
         (error 'transform "Unannotated if remains")))
    ((source:create-record type fields)
     (let ((r-type (lookup-type-reference type type-env)))
      (if (inter:record-type? r-type)
          (inter:primop-expr (create-record-primop r-type)
           (map (lambda: ((expr : source:expression)) (recur expr))
            (map (inst cdr Symbol source:expression) fields)))
          (error 'transform "Creation of a record of a non record-type"))))
    ((source:create-array type size value)
     (let ((r-type (lookup-type-reference type type-env)))
      (if (inter:array-type? r-type)
          (inter:primop-expr (create-array-primop r-type)
           (list (recur size) (recur value)))
          (error 'transform "Creation of an array of a non array-type"))))
    ((source:binder decls expr)
     (if (empty? decls) (recur expr)
       (match (first decls)
        ((source:untyped-variable-declaration var body)
         (error 'transform "Untyped variable declaration remains"))
        ((source:variable-declaration var type body)
         (let ((var (assert-unique var)))
          (inter:bind var (lookup-type-reference type type-env) (recur body)
           ((trans (hash-set env var #t) type-env)  (source:binder (rest decls) expr)))))
        ((source:function-declaration name args type body)
         (let-values (((fun-decls decls) (span source:function-declaration? decls)))
          (let-values (((funs env) (transform-function-declarations fun-decls env type-env)))
           (inter:bind-rec funs ((trans env type-env) (source:binder decls expr))))))
        ((source:type-declaration name type)
         (let-values (((type-decls decls) (span source:type-declaration? decls)))
          ((trans env (extend-type-environment type-decls type-env)) (source:binder decls expr)))))))
    ((source:while-loop cond body)
     (inter:while-loop (recur cond) (recur body)))
    ((source:for-loop var init final body)
     (let ((var (assert-unique var)))
      (let ((env (hash-set env var #t)))
       (let ((recur (trans env type-env)))
        (inter:for-loop var (recur init) (recur final) (recur body))))))
    ((source:break) (inter:break))
    ((source:integer-literal num) (inter:primop-expr (integer-constant-primop num) empty))
    ((source:string-literal str) (inter:primop-expr (string-constant-primop str) empty))
    ((source:nil ref)
     (if ref
         (inter:primop-expr (nil-primop (lookup-type-reference ref type-env)) empty)
         (error 'transform "Untyped nil remains")))
    (else (error 'transform "Case ~a remains" prog))
    ))
  recur)
 
 (: lookup-type-reference (source:type-reference (HashTable unique inter:type) -> inter:type))
 (define (lookup-type-reference ref env) (lookup-type (assert-unique (source:type-reference-name ref)) env))

 (: lookup-type (unique (HashTable unique inter:type) -> inter:type))
 (define (lookup-type name env)
  (hash-ref env name
   (lambda () (error 'transform "Unbound type refernece ~a in ~a" name env))))
 
 (: transform-function-declarations
  ((Listof source:function-declaration) (HashTable unique #t) (HashTable unique inter:type) ->
   (values (Listof (Pair unique inter:function)) (HashTable unique #t))))
 (define (transform-function-declarations fun-decs env type-env)
  (: add-symbol (unique (HashTable unique #t) -> (HashTable unique #t)))
  (define (add-symbol sym env) (hash-set env sym #t))

  (let* ((function-names (map assert-unique (map source:function-declaration-name fun-decs)))
         (env (foldl add-symbol env function-names)))
   (: transform-function-declaration (source:function-declaration -> (Pair unique inter:function)))
   (define (transform-function-declaration dec)
    (match dec
     ((source:function-declaration name args return body)
      (let ((name (assert-unique name)))
       (let ((arg-names (map assert-unique (map (inst car (U Symbol unique) source:type-reference) args)))
             (arg-types (map (inst cdr (U Symbol unique) source:type-reference) args)))
        (let ((env (foldl add-symbol env arg-names)))
         (let ((body ((trans env type-env) body))
               (return (if return (lookup-type-reference return type-env) inter:unit-type))
               (arg-types (map (lambda: ((ref : source:type-reference)) (lookup-type-reference ref type-env)) arg-types)))
          (cons name (inter:function (map (inst cons unique inter:type) arg-names arg-types) return body)))))))))
   (values (map transform-function-declaration fun-decs) env)))
        


 (: extend-type-environment ((Listof source:type-declaration) (HashTable unique inter:type) -> (HashTable unique inter:type)))
 (define (extend-type-environment type-decs type-env)
  (: convert-type-declaration (source:type-declaration -> (Pair unique (U inter:proto-type inter:proto-ref-type))))
  (define (convert-type-declaration dec)
   (match dec
    ((source:type-declaration name ty)
     (cons (assert-unique name) (convert-type ty)))))
  (: convert-type (case-lambda
                   (source:compound-type  ->  inter:proto-type)
                   (source:type-reference -> inter:proto-ref-type)
                   ((U source:compound-type source:type-reference) -> (U inter:proto-type inter:proto-ref-type))))

  (define (convert-type ty)
   (match ty
    ((source:type-reference name) (assert-unique name))
    ((source:record-type fields)
     (inter:proto-record-type
      (map
       (lambda: ((pair : (Pair Symbol source:type-reference))) (cons (car pair) (assert-unique (source:type-reference-name (cdr pair)))))
       fields)))
    ((source:array-type ref) (inter:proto-array-type (assert-unique (source:type-reference-name ref))))
    ((source:function-type arg-types return-type)
     (if (andmap source:type-reference? arg-types)
         (inter:proto-function-type
          (map assert-unique (map source:type-reference-name arg-types))
          
          (and return-type (if (source:type-reference? return-type)
                               (assert-unique (source:type-reference-name return-type))
                               (error 'transform "Unsimplified function-type"))))
         (error 'transform "Unsimplified function-type")))))
         

  (inter:fix-proto-types (map convert-type-declaration type-decs) type-env))



 ((trans (make-immutable-hash empty) global-type-env) prog))



  


