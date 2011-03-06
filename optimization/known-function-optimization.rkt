
#lang typed/racket/base

(require "../ir-anf-ast.rkt" "../primop.rkt" "../unique.rkt")
(require racket/match racket/list)



(provide known-function-optimization)

(: known-function-optimization (expression -> expression))
(define (known-function-optimization expr)
 (: env (HashTable unique (U runtime-primop unique)))
 (define env (make-hash))
 (: recur (expression -> expression))
 (define (recur expr)
  (match expr
   ((return name) expr)
   ((bind-primop var ty op args expr)
    (define (normal) (bind-primop var ty op args (recur expr)))
    (cond
     ((call-closure-primop? op) 
      (let ((arg1 (first args)))
       (let ((fun (hash-ref env arg1 (lambda () #f))))
         (cond
            ((unique? fun)
             (bind-primop var ty (call-known-function-primop (call-closure-primop-type op) fun) args (recur expr)))
            ((runtime-primop? fun)
             (match fun
              ((runtime-primop prim-ty name)
               (let ((undef (gen-uniq 'undef)))
                (bind-primop undef prim-ty (undefined-primop prim-ty) empty
                 (bind-primop var ty (call-known-runtime-primop prim-ty name) (cons undef (rest args)) (recur expr)))))))
            ((not fun) (normal))))))
     ((runtime-primop? op) 
      (hash-set! env var op)
      (normal))
     (else (normal))))
   ((bind-rec funs body)
    (for: ((p : (Pair unique function) funs))
     (hash-set! env (car p) (function-name (cdr p))))
    (bind-rec
     (map (lambda: ((p : (Pair unique function)))
      (cons (car p)
       (match (cdr p)
        ((function name args ret body)
         (function name args ret (recur body)))))) funs)
     (recur body)))
   ((conditional c t f ty)
    (conditional c (recur t) (recur f) ty))))
 (recur expr))
 
