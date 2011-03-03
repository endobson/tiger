#lang typed/racket/base

(require "../ir-anf-ast.rkt")
(require racket/list racket/match)

(provide remove-empty-bind-rec)

(: remove-empty-bind-rec (expression -> expression))
(define (remove-empty-bind-rec expr)
 (let ((recur remove-empty-bind-rec))
  (match expr
   ((return name) expr)
   ((bind-primop var ty op args expr) (bind-primop var ty op args (recur expr)))
   ((bind-rec funs body)
    (if (empty? funs) (recur body)
        (bind-rec
         (map (lambda: ((p : (Pair unique function)))
          (cons (car p)
           (match (cdr p)
            ((function name args ret body)
             (function name args ret (recur body)))))) funs)
         (recur body))))
   ((conditional c t f ty)
    (conditional c (recur t) (recur f) ty)))))
 
