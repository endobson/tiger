#lang typed/racket/base

(require racket/match racket/list)
(require (prefix-in source: "source-ast.rkt")
         (prefix-in lifted: "lifted-ast.rkt")
         "core-ast.rkt")
(provide source-ast->lifted-ast)


(struct: lift-environment ((funs : lifted:function-environment) (types : lifted:type-environment)))

(: source-ast->lifted-ast (source:expression -> lifted:lifted-program))
(define (source-ast->lifted-ast expr)
 (: lift (lift-environment source:expression -> (values lift-environment lifted:expression)))
 (define (lift env expr)
  (match expr
   ((? constant? c) (values env c))
   (else (error 'lift "Not yet implemented ~a" expr))))

 (let-values (((env expr)
               (lift (lift-environment (make-immutable-hash empty) (make-immutable-hash empty)) expr)))
  (lifted:lifted-program
    (lift-environment-types env)
    (lift-environment-funs env)
    expr)))

