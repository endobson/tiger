#lang typed/racket/base

(require racket/match racket/list )


(require
         (prefix-in lifted: "lifted-anf-ast.rkt")
         (prefix-in ir: "ir-anf-ast.rkt")
         "unique.rkt"
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



(: remove-all ((Listof unique) (Listof unique) -> (Listof unique)))
(define (remove-all bound symbols)
 (filter-not (lambda: ((id : unique)) (member id bound)) symbols))


(: find-free-variables (ir:expression -> (Listof unique)))
(define (find-free-variables expr)
 (: recur (ir:expression -> (Listof unique)))
 (define (recur prog)
  (match prog
   ((ir:return sym) (list sym))
   ((ir:conditional c t f ty)
    (cons c (append (recur t) (recur f))))
   ((ir:bind-primop var type op args expr)
    (append args (remove-all (list var) (recur expr))))
   ((ir:bind-rec functions body)
    (let ((fun-names (map (inst car unique ir:function) functions))
          (funs (map (inst cdr unique ir:function) functions)))
     (remove-all fun-names
      (append (recur body)
       (append-map (lambda: ((f : ir:function))
        (match f
         ((ir:function name args ty body)
          (remove-all (map (inst car unique type) args)
                      (recur body))))) funs)))))))
 (recur expr))
       

(: add-function (lifted:function lift-environment -> lift-environment))
(define (add-function fun env)
 (hash-set env (lifted:function-name fun) fun))





(: lift (ir:expression -> lifted:lifted-program))
(define (lift expr)
 (define-type id-environment (HashTable unique type))
 (: lift
   (ir:expression id-environment lift-environment -> (values lifted:expression lift-environment)))
 (define (lift expr id-env env)
  (match expr
   ((ir:return x) (values (lifted:return x) env))
   ((ir:bind-primop var type op args body)
    (let*-values (((body env) (lift body (hash-set id-env var type) env)))
     (values (lifted:bind-primop var type op args body) env)))
   ((ir:bind-rec closure-decs body)
    (let ((id-env
           (foldl (lambda: ((dec : (Pair unique ir:function)) (id-env : id-environment))
              (hash-set id-env (car dec) (ir:function->function-type (cdr dec)))) id-env closure-decs)))
      (let-values (((body env) (lift body id-env env)))
       (let-values (((closures env)
          (for/fold: : (values (Listof (Pair unique lifted:create-closure)) lift-environment)
            ((closures : (Listof (Pair unique lifted:create-closure)) empty)
             (env : lift-environment env))
            ((dec : (Pair unique ir:function) closure-decs))
           (let ((name (car dec)))
            (match (cdr dec)
             ((ir:function fun-name args ty body)
              (let* ((arg-names (map (inst car unique type) args))
                     (arg-types (map (inst cdr unique type) args))
                     (free-vars (remove-all arg-names (find-free-variables body))))
               (let ((id-env (foldl (lambda: ((name : unique) (ty : type) (env : id-environment)) (hash-set env name ty))
                                    id-env arg-names arg-types)))
                (let-values (((body env) (lift body id-env env)))
                 (values
                  (cons (cons name (lifted:create-closure fun-name free-vars)) closures)
                  (add-function
                   (lifted:function
                    fun-name
                    (make-function-type arg-types ty)
                    arg-names
                    free-vars
                    (map (lambda: ((s : unique)) (hash-ref id-env s (lambda () (error 'lift "Cannot find free-variable ~a in ~a" s id-env)))) free-vars)
                    body)
                   env)))))))))))
          (values (lifted:bind-rec closures body) env)))))
   ((ir:conditional cond t-branch f-branch ty)
    (let*-values 
      (((t-branch env) (lift t-branch id-env env))
       ((f-branch env) (lift f-branch id-env env)))
     (values (lifted:conditional cond t-branch f-branch ty) env)))))

 (let-values (((expr env)
               (lift expr (ann (make-immutable-hash empty) (HashTable unique type))
                          (ann (make-immutable-hash empty) lift-environment))))
  (lifted:lifted-program
    env
    expr)))

