#lang typed/racket/base

(require "../ir-ast.rkt")
(require racket/list racket/match)

(provide remove-empty-bind-rec)

(: remove-empty-bind-rec (expression -> expression))
(define (remove-empty-bind-rec expr)
 (let ((recur remove-empty-bind-rec))
  (match expr
   ((identifier name) expr)
   ((primop-expr op args) (primop-expr op (map recur args)))
   ((bind var ty expr body)
    (bind var ty (recur expr) (recur body)))
   ((bind-rec funs body)
    (if (empty? funs) (recur body)
        (bind-rec
         (map (lambda: ((p : (Pair Symbol function)))
          (cons (car p)
           (match (cdr p)
            ((function name args ret body)
             (function name args ret (recur body)))))) funs)
         (recur body))))
   ((sequence first next)
    (sequence (recur first) (recur next)))
   ((conditional c t f ty)
    (conditional (recur c) (recur t) (recur f) ty))
   (else (error 'remove-empty-bind-rec "Missing case ~a" expr)))))
 
