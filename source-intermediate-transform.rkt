#lang typed/racket/base





(require

         (prefix-in inter: "types.rkt")
         (prefix-in source: "source-ast.rkt")
         (prefix-in source: "core-ast.rkt")
         (prefix-in inter: "intermediate-ast.rkt")
         "primop.rkt" "external-functions.rkt")


(require racket/list racket/match)


(provide transform global-env global-type-env)


(: span (All (a b) ((a -> Any : b) (Listof a) -> (values (Listof b) (Listof a)))))
(define (span f list)
 (if (empty? list) (values empty empty)
  (let ((elem (first list)))
   (if (f elem)
       (let-values (((f r) (span f (rest list))))
        (values (cons elem f) r))
       (values empty list)))))

(: global-type-env (HashTable Symbol inter:type))
(define global-type-env
 (make-immutable-hash
  (list (cons 'int inter:int-type) (cons 'string inter:string-type))))



(: exit-primop runtime-primop)
(define exit-primop
 (runtime-primop (inter:make-function-type (list inter:int-type) inter:unit-type) 'exit))

(: global-env (HashTable Symbol runtime-primop))
(define global-env
 (make-immutable-hash
  (hash-map external-function-database
   (lambda: ((name : Symbol) (type : inter:function-type))
    (cons name (runtime-primop type name))))))




(: transform  (source:expression (HashTable Symbol runtime-primop) (HashTable Symbol inter:type) -> inter:expression))
(define (transform prog global-env global-type-env)
 (: trans ((HashTable Symbol #t) (HashTable Symbol inter:type) -> (source:expression -> inter:expression)))
 (define (trans env type-env)
  (: recur (source:expression  -> inter:expression))
  (define (recur prog)
   (match prog
    ((source:identifier name) 
     (if (hash-has-key? env name) (inter:identifier name) 
      (inter:primop-expr
       (hash-ref global-env name
        (lambda () (error 'transform "Unbound identifier ~a in ~a and ~a" name env global-env)))
       empty)))
    ((source:math symbol left right)
     (inter:primop-expr (math-primop symbol) (list (recur left) (recur right))))
    ((source:equality symbol left right type)
     (if type
         (inter:primop-expr (equality-primop (equal? symbol '=) (lookup-type-reference type type-env)) (list (recur left) (recur right)))
         (error 'transform "Unannotated equality: ~a" prog)))
    ((source:negation expr)
     (inter:primop-expr (math-primop '-) (list (inter:primop-expr (integer-constant-primop 0) empty) (recur expr))))
    ((source:function-call fun args)
     (inter:primop-expr (call-closure-primop)
      (map (lambda: ((expr : source:expression)) (recur expr)) (cons fun args))))
    ((source:sequence exprs)
     (if (empty? exprs) (inter:primop-expr (unit-primop) empty)
      (let loop ((expr (first exprs)) (exprs (rest exprs)))
       (if (empty? exprs) (recur expr)
          (inter:sequence (recur expr)
           (loop (first exprs) (rest exprs)))))))
    ((source:assignment lvalue expr)
     (let ((val (recur expr)))
      (match lvalue
       ((source:identifier name)
        (inter:assignment name val))
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
    ((source:if-then-else c t f)
     (if f
         (inter:conditional (recur c) (recur t) (recur f))
         (error 'transform "One armed if remains")))
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
         (inter:bind var (lookup-type-reference type type-env) (recur body)
          ((trans (hash-set env var #t) type-env)  (source:binder (rest decls) expr))))
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
     (let ((env (hash-set env var #t)))
      (let ((recur (trans env type-env)))
       (inter:for-loop var (recur init) (recur final) (recur body)))))
    ((source:break) (inter:break))
    ((source:integer-literal num) (inter:primop-expr (integer-constant-primop num) empty))
    ((source:string-literal str) (inter:primop-expr (string-constant-primop str) empty))
    ((source:nil ref)
     (if ref
         (inter:primop-expr (nil-primop (lookup-type-reference ref type-env)) empty)
         (error 'transform "Untyped nil remains")))
    ))
  recur)
 
 (: lookup-type-reference (source:type-reference (HashTable Symbol inter:type) -> inter:type))
 (define (lookup-type-reference ref env) (lookup-type (source:type-reference-name ref) env))

 (: lookup-type (Symbol (HashTable Symbol inter:type) -> inter:type))
 (define (lookup-type name env)
  (hash-ref env name
   (lambda () (error 'transform "Unbound type refernece ~a in ~a" name env))))
 
 (: transform-function-declarations
  ((Listof source:function-declaration) (HashTable Symbol #t) (HashTable Symbol inter:type) ->
   (values (Listof (Pair Symbol inter:function)) (HashTable Symbol #t))))
 (define (transform-function-declarations fun-decs env type-env)
  (: add-symbol (Symbol (HashTable Symbol #t) -> (HashTable Symbol #t)))
  (define (add-symbol sym env) (hash-set env sym #t))

  (let* ((function-names (map source:function-declaration-name fun-decs))
         (env (foldl add-symbol env function-names)))
   (: transform-function-declaration (source:function-declaration -> (Pair Symbol inter:function)))
   (define (transform-function-declaration dec)
    (match dec
     ((source:function-declaration name args return body)
      (let ((arg-names (map (inst car Symbol source:type-reference) args))
            (arg-types (map (inst cdr Symbol source:type-reference) args)))
       (let ((env (foldl add-symbol env arg-names)))
        (let ((body ((trans env type-env) body))
              (return (if return (lookup-type-reference return type-env) inter:unit-type))
              (arg-types (map (lambda: ((ref : source:type-reference)) (lookup-type-reference ref type-env)) arg-types)))
         (cons name (inter:function (map (inst cons Symbol inter:type) arg-names arg-types) return body))))))))
   (values (map transform-function-declaration fun-decs) env)))
        


 (: extend-type-environment ((Listof source:type-declaration) (HashTable Symbol inter:type) -> (HashTable Symbol inter:type)))
 (define (extend-type-environment type-decs type-env)
  (: convert-type-declaration (source:type-declaration -> (Pair Symbol (U inter:proto-type inter:proto-ref-type))))
  (define (convert-type-declaration dec)
   (match dec
    ((source:type-declaration name ty)
     (cons name (convert-type ty)))))
  (: convert-type (case-lambda
                   (source:compound-type  ->  inter:proto-type)
                   (source:type-reference -> inter:proto-ref-type)
                   ((U source:compound-type source:type-reference) -> (U inter:proto-type inter:proto-ref-type))))

  (define (convert-type ty)
   (match ty
    ((source:type-reference name) name)
    ((source:record-type fields)
     (inter:proto-record-type
      (map
       (lambda: ((pair : (Pair Symbol source:type-reference))) (cons (car pair) (source:type-reference-name (cdr pair))))
       fields)))
    ((source:array-type ref) (inter:proto-array-type (source:type-reference-name ref)))
    ((source:function-type arg-types return-type)
     (inter:proto-function-type
      (map source:type-reference-name arg-types)
      (and return-type (source:type-reference-name return-type))))))

  (inter:fix-proto-types (map convert-type-declaration type-decs) type-env))



 ((trans (make-immutable-hash empty) global-type-env) prog))



  


