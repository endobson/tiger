
#lang typed/racket/base

(require "ir-ast.rkt" "primop.rkt")
(require racket/match racket/list)



(provide known-function-optimization)

(: known-function-optimization (expression -> expression))
(define (known-function-optimization expr)
 (: env (HashTable Symbol Symbol))
 (define env (make-hash))
 (: recur (expression -> expression))
 (define (recur expr)
  (match expr
   ((identifier name) expr)
   ((primop-expr op args)
     (define (normal) (primop-expr op (map recur args)))
     (if (call-closure-primop? op) 
         (let ((arg1 (first args)))
          (if (identifier? arg1) 
              (let ((fun-name (hash-ref env (identifier-name arg1) (lambda () #f))))
               (if fun-name
                   (primop-expr (call-known-function-primop (call-closure-primop-type op) fun-name) (map recur args))
                   (normal)))
              (normal)))
         (normal)))
   ((bind var ty expr body)
    (bind var ty (recur expr) (recur body)))
   ((bind-rec funs body)
    (for: ((p : (Pair Symbol function) funs))
     (hash-set! env (car p) (function-name (cdr p))))
    (bind-rec
     (map (lambda: ((p : (Pair Symbol function)))
      (cons (car p)
       (match (cdr p)
        ((function name args ret body)
         (function name args ret (recur body)))))) funs)
     (recur body)))
   ((sequence first next)
    (sequence (recur first) (recur next)))
   ((conditional c t f ty)
    (conditional (recur c) (recur t) (recur f) ty))
   (else (error 'remove-extra-variable-bindings "Missing case ~a" expr))))
 (recur expr))
 
