#lang typed/racket/base

(require racket/match racket/list)
(require (prefix-in source: "source-ast.rkt")
         (prefix-in lifted: "lifted-ast.rkt")
         "core-ast.rkt")
(provide source-ast->lifted-ast)


(struct: lift-environment ((funs : lifted:function-environment) (types : lifted:type-environment)))

(: source-ast->lifted-ast (source:expression -> lifted:lifted-program))
(define (source-ast->lifted-ast expr)
 (: lift (source:expression lift-environment -> (values lifted:expression lift-environment)))
 (define (lift expr env )
  (match expr
   ((? constant? c) (values c env))
   ((source:identifier x) (values (lifted:identifier x) env))
   ((source:binder decls body)
    (cond
     ((empty? decls) (lift body env))
     (else
      (let ((dec (first decls)))
       (match dec
        ((source:variable-declaration name type var-body)
          (let*-values (((var-body env) (lift var-body env))
                        ((body env) (lift (source:binder (rest decls) body) env)))
           (values 
            (lifted:bind name var-body body)
            env)))
        ((source:type-declaration name type)
         (lift (source:binder (rest decls) body) env))
        (else (error 'lift "Unsupported declaration ~a" dec)))))))
   ((source:math op l r)
    (let*-values (((l env) (lift l env)) ((r env) (lift r env)))
     (values (lifted:math op l r) env)))
   ((source:function-call f args)
    (let*-values (((lf env) (lift f env))
                  ((largs env)
        (for/fold: : (values (Listof lifted:expression) lift-environment)
         ((exprs : (Listof lifted:expression) empty)
          (env : lift-environment env))
         ((arg : source:expression args))
         (let-values (((expr env) (lift arg env)))
          (values (cons expr exprs) env)))))
     (values (lifted:function-call lf largs) env)))
   ((source:sequence exprs)
    (let-values (((exprs env)
                  (for/fold: : (values (Listof lifted:expression) lift-environment)
                   ((lexprs : (Listof lifted:expression) empty)
                    (env : lift-environment env))
                   ((expr : source:expression exprs))
                   (let-values (((lexpr env) (lift expr env)))
                    (values (cons lexpr lexprs) env)))))
     (values (lifted:sequence exprs) env)))
   (else (error 'lift "Not yet implemented ~a" expr))))

 (let-values (((expr env)
               (lift expr (lift-environment (make-immutable-hash empty) (make-immutable-hash empty)))))
  (lifted:lifted-program
    (lift-environment-types env)
    (lift-environment-funs env)
    expr)))

