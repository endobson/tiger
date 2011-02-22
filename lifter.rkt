#lang typed/racket/base

(require racket/match racket/list )


(require
         (prefix-in lifted: "lifted-ast.rkt")
         (prefix-in ir: "ir-ast.rkt")
         "types.rkt" )
(provide lift)


(define-type lift-environment lifted:function-environment)


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


(: find-free-variables (ir:expression -> (Listof Symbol)))
(define (find-free-variables expr)
 (: recur (ir:expression -> (Listof Symbol)))
 (define (recur prog)
  (match prog
   ((ir:identifier sym) (list sym))
   ((ir:sequence first next) (append (recur first) (recur next)))
   ((ir:conditional c t f ty)
    (append (recur c) (recur t) (recur f)))
   ((ir:primop-expr op args)
    (append-map recur args))
   ((ir:bind v ty expr body)
    (append (recur expr) (remove-all (list v) (recur body))))
   ((ir:bind-rec functions body)
    (let ((fun-names (map (inst car Symbol ir:function) functions))
          (funs (map (inst cdr Symbol ir:function) functions)))
     (remove-all fun-names
      (append (recur body)
       (append-map (lambda: ((f : ir:function))
        (match f
         ((ir:function args ty body)
          (remove-all (map (inst car Symbol type) args)
                      (recur body))))) funs)))))))
 (recur expr))
       

(: add-function (Symbol lifted:lifted-function lift-environment -> lift-environment))
(define (add-function name fun env)
 (hash-set env name fun))





(: lift (ir:expression -> lifted:lifted-program))
(define (lift expr)
 (define-type id-environment (HashTable Symbol type))
 (: lift
   (ir:expression id-environment lift-environment -> (values lifted:expression lift-environment)))
 (define (lift expr id-env env)
  (match expr
   ((ir:identifier x) (values (lifted:identifier x) env))
   ((ir:bind var type expr body)
    (let*-values (((expr env) (lift expr id-env env))
                  ((body env) (lift body (hash-set id-env var type) env)))
     (values (lifted:bind var type expr body) env)))
   ((ir:bind-rec closure-decs body)
    (let ((id-env
           (foldl (lambda: ((dec : (Pair Symbol ir:function)) (id-env : id-environment))
              (hash-set id-env (car dec) (ir:function->function-type (cdr dec)))) id-env closure-decs)))
      (let-values (((body env) (lift body id-env env)))
       (let-values (((closures env)
          (for/fold: : (values (Listof (Pair Symbol lifted:create-closure)) lift-environment)
            ((closures : (Listof (Pair Symbol lifted:create-closure)) empty)
             (env : lift-environment env))
            ((dec : (Pair Symbol ir:function) closure-decs))
           (let ((name (car dec)))
            (match (cdr dec)
             ((ir:function args ty body)
              (let* ((fun-name (gensym name))
                     (arg-names (map (inst car Symbol type) args))
                     (arg-types (map (inst cdr Symbol type) args))
                     (free-vars (remove-all arg-names (find-free-variables body))))
               (let ((id-env (foldl (lambda: ((name : Symbol) (ty : type) (env : id-environment)) (hash-set env name ty))
                                    id-env arg-names arg-types)))
                (let-values (((body env) (lift body id-env env)))
                 (values
                  (cons (cons name (lifted:create-closure fun-name free-vars)) closures)
                  (add-function
                   fun-name
                   (lifted:lifted-function
                    (make-function-type arg-types ty)
                    arg-names
                    free-vars
                    (map (lambda: ((s : Symbol)) (hash-ref id-env s (lambda () (error 'lift "Cannot find free-variable ~a in ~a" s id-env)))) free-vars)
                    body)
                   env)))))))))))
          (values (lifted:bind-rec closures body) env)))))
   ((ir:primop-expr op args)
    (let*-values (((largs env)
        (for/fold: : (values (Listof lifted:expression) lift-environment)
         ((exprs : (Listof lifted:expression) empty)
          (env : lift-environment env))
         ((arg : ir:expression args))
         (let-values (((expr env) (lift arg id-env env)))
          (values (cons expr exprs) env)))))
     (values (lifted:primop-expr op (reverse largs)) env)))
   ((ir:sequence first rest)
    (let*-values (((first env) (lift first id-env env))
                  ((rest env) (lift rest id-env env)))
     (values (lifted:sequence first rest) env)))
   ((ir:conditional cond t-branch f-branch ty)
    (let*-values 
      (((cond env) (lift cond id-env env))
       ((t-branch env) (lift t-branch id-env env))
       ((f-branch env) (lift f-branch id-env env)))
     (values (lifted:conditional cond t-branch f-branch ty) env)))
   (else (error 'lift "Not yet implemented ~a" expr))))

 (let-values (((expr env)
               (lift expr (ann (make-immutable-hash empty) (HashTable Symbol type))
                          (ann (make-immutable-hash empty) lift-environment))))
  (lifted:lifted-program
    env
    expr)))

