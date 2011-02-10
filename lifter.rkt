#lang typed/racket/base

(require racket/match racket/list)
(require (prefix-in source: "source-ast.rkt")
         (prefix-in lifted: "lifted-ast.rkt")
         "core-ast.rkt"
         "environment.rkt")
(provide source-ast->lifted-ast)


(struct: lift-environment ((funs : lifted:function-environment)
                           (types : lifted:type-environment)))


(: span (All (a b) ((a -> Any : b) (Listof a) -> (values (Listof b) (Listof a)))))
(define (span f list)
 (if (empty? list) (values empty empty)
  (let ((elem (first list)))
   (if (f elem)
       (let-values (((f r) (span f (rest list))))
        (values (cons elem f) r))
       (values empty list)))))



(: remove-all ((Listof Symbol) (Listof Symbol) -> (Listof Symbol)))
(define (remove-all bound symbols)
 (filter-not (lambda: ((id : Symbol)) (member id bound)) symbols))


(: find-free-variables (source:expression -> (Listof Symbol)))
(define (find-free-variables expr)

 (: remove-declarations ((Listof source:declaration)  (Listof Symbol) -> (Listof Symbol)))
 (define (remove-declarations decls symbols)
  (let ((bound 
   (for/fold: : (Listof Symbol) 
     ((syms : (Listof Symbol) empty))
     ((decl : source:declaration decls))
    (match decl
     ((source:type-declaration name ty) syms)
     ((source:function-declaration name type args body)
      (cons name syms))
     ((source:variable-declaration name type body)
      (cons name syms))
     ((source:untyped-variable-declaration name body)
      (cons name syms))))))
    (remove-all bound symbols)))


 (: recur ((U source:expression source:declaration) -> (Listof Symbol)))
 (define (recur prog)
  (match prog
   ((source:identifier sym) (list sym))
   ((source:field-ref base field) (recur base))
   ((source:array-ref base index) (append (recur base) (recur index)))
   ((source:binder declarations body)
     (remove-declarations declarations (append  (recur body) (append-map recur declarations))))
   ((source:sequence exprs) (append-map recur exprs))
   ((source:assignment value expr)
    (append (recur value) (recur expr)))
   ((source:if-then-else c t f)
    (append (recur c) (recur t) (if f (recur f) empty)))
   ((integer-literal v) empty)
   ((string-literal s) empty)
   ((nil) empty)
   ((source:negation expr) (recur expr))
   ((source:function-call fun args)
    (append (recur fun) (append-map recur args)))
   ((source:math op left right)
    (append (recur left) (recur right)))
   ((source:create-record type fields)
    (append-map recur (map (inst cdr Symbol source:expression) fields)))
   ((source:create-array type size value)
    (append (recur size) (recur value)))
   ((source:while-loop guard body)
    (append (recur guard) (recur body)))
   ((source:for-loop id init final body)
    (append (recur init) (recur final) (recur body)))
   ((source:break) empty)
   ((source:type-declaration name type) empty)
   ((source:function-declaration name args return-type body)
    (let ((arg-names (map (inst car Symbol value-type) args)))
     (remove-all arg-names (recur body))))
   ((source:variable-declaration sym type value)
    (filter-not (lambda: ((id : Symbol)) (equal? id sym)) (recur value)))
   ((source:untyped-variable-declaration sym value)
    (remove-all (list sym) (recur value)))))

 (recur expr))
       

(: add-function (Symbol lifted:lifted-function lift-environment -> lift-environment))
(define (add-function name fun env)
 (match env
  ((lift-environment funs types)
   (lift-environment (hash-set funs name fun) types))))


(: add-type (Symbol value-type lift-environment -> lift-environment))
(define (add-type name type env)
 (match env
  ((lift-environment funs types)
   (if (type-reference? type)
    (lift-environment funs (hash-set types name (hash-ref types (type-reference-name type))))
    (lift-environment funs (hash-set types name type))))))



(: source-ast->lifted-ast (source:expression -> lifted:lifted-program))
(define (source-ast->lifted-ast expr)
 (define-type id-environment (HashTable Symbol value-type))
 (: lift
  (case-lambda 
   (source:lvalue id-environment lift-environment -> (values lifted:lvalue lift-environment))
   (source:expression id-environment lift-environment -> (values lifted:expression lift-environment))))
 (define (lift expr id-env env)
  (match expr
   ((? constant? c) (values c env))
   ((source:identifier x) (values (lifted:identifier x) env))
   ((source:field-ref base name)
    (let-values (((base env) (lift base id-env env)))
     (values (lifted:field-ref base name) env)))
   ((source:binder decls body)
    (cond
     ((empty? decls) (lift body id-env env))
     (else
      (let ((dec (first decls)))
       (match dec
        ((source:variable-declaration name type var-body)
          (let*-values (((var-body env) (lift var-body id-env env))
                        ((body env) (lift (source:binder (rest decls) body) (hash-set id-env name type) env)))
           (values 
            (lifted:bind name var-body body)
            env)))
        ((source:type-declaration name type)
         (lift (source:binder (rest decls) body) id-env (add-type name type env)))
        ((source:function-declaration name args type dec-body)
         (let-values (((fun-decs other-decs) (span source:function-declaration? decls)))
          (let ((id-env
                 (foldl (lambda: ((dec : source:function-declaration) (id-env : id-environment))
                  (match dec
                   ((source:function-declaration name args type body)
                    (hash-set id-env name
                     (function-type (map (inst cdr Symbol type-reference) args) type))))) id-env fun-decs)))
            (let-values (((inner-body env) (lift (source:binder other-decs body)
                                                 id-env env)))
             (let-values (((closures env)
                (for/fold: : (values (Listof (Pair Symbol lifted:create-closure)) lift-environment)
                  ((closures : (Listof (Pair Symbol lifted:create-closure)) empty)
                   (env : lift-environment env))
                  ((dec : source:function-declaration fun-decs))
                 (match dec
                  ((source:function-declaration name args type body)
                   (let* ((fun-name (gensym name))
                          (arg-names (map (inst car Symbol type-reference) args))
                          (arg-types (map (inst cdr Symbol type-reference) args))
                          (free-vars (remove-all arg-names (find-free-variables body))))
                    (let ((id-env (foldl (lambda: ((name : Symbol) (type : value-type) (env : id-environment)) (hash-set env name type)) id-env arg-names arg-types)))
                     (let-values (((body env) (lift body id-env env)))
                      (values
                       (cons (cons name (lifted:create-closure fun-name free-vars)) closures)
                       (add-function
                        fun-name
                        (lifted:lifted-function
                         (function-type arg-types type)
                         arg-names
                         free-vars
                         (map (lambda: ((s : Symbol)) (hash-ref id-env s)) free-vars)
                         body)
                        env))))))))))
              (values (lifted:bind-rec closures inner-body) env))))))
        (else (error 'lift "Unsupported declaration ~a" dec)))))))
   ((source:math op l r)
    (let*-values (((l env) (lift l id-env env)) ((r env) (lift r id-env env)))
     (values (lifted:math op l r) env)))
   ((source:create-record type fields)
    (let-values (((reversed-fields env)
          (for/fold: : (values (Listof (Pair Symbol lifted:expression)) lift-environment)
            ((lifted-fields : (Listof (Pair Symbol lifted:expression)) empty)
             (env : lift-environment env))
            ((field : (Pair Symbol source:expression) fields))
           (let ((name (car field)) (expr (cdr field)))
            (let-values (((l-expr env) (lift expr id-env env)))
             (values (cons (cons name l-expr) lifted-fields) env))))))
      (values (lifted:create-record type (reverse reversed-fields))
              env)))
   ((source:function-call f args)
    (let*-values (((lf env) (lift f id-env env))
                  ((largs env)
        (for/fold: : (values (Listof lifted:expression) lift-environment)
         ((exprs : (Listof lifted:expression) empty)
          (env : lift-environment env))
         ((arg : source:expression args))
         (let-values (((expr env) (lift arg id-env env)))
          (values (cons expr exprs) env)))))
     (values (lifted:function-call lf (reverse largs)) env)))
   ((source:sequence exprs)
    (let-values (((exprs env)
                  (for/fold: : (values (Listof lifted:expression) lift-environment)
                   ((lexprs : (Listof lifted:expression) empty)
                    (env : lift-environment env))
                   ((expr : source:expression exprs))
                   (let-values (((lexpr env) (lift expr id-env env)))
                    (values (cons lexpr lexprs) env)))))
     (values (lifted:sequence (reverse exprs)) env)))
   ((source:if-then-else cond t-branch f-branch)
    (let*-values 
      (((cond env) (lift cond id-env env))
       ((t-branch env) (lift t-branch id-env env))
       ((f-branch env) (if f-branch (lift f-branch id-env env) (values (lifted:sequence empty) env))))
     (values (lifted:if-then-else cond t-branch f-branch) env)))
   (else (error 'lift "Not yet implemented ~a" expr))))

 (let-values (((expr env)
               (lift expr (ann (make-immutable-hash empty) (HashTable Symbol value-type))
                          (lift-environment (make-immutable-hash empty) (make-immutable-hash empty)))))
  (lifted:lifted-program
    (lift-environment-types env)
    (lift-environment-funs env)
    expr)))

