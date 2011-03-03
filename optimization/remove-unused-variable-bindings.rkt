#lang typed/racket/base

(require "../ir-anf-ast.rkt" "side-effects.rkt" "../primop.rkt")
(require racket/match racket/list)


(provide remove-unused-variable-bindings)

(: remove-unused-variable-bindings (expression -> expression))
(define (remove-unused-variable-bindings expr)
 (: counts (HashTable unique Natural))
 (define counts (make-hash))

 (: update-count (unique -> Void))
 (define (update-count name)
  (hash-update! counts name add1
   (lambda () (error 'remove-unused-variable-bindings "Unbound identifier ~a in ~a" name counts))))

 (: recur (expression -> expression))
 (define (recur expr)
  (match expr
   ((return name)
    (update-count name)
    expr)
   ((bind-primop var ty op args expr)
    (for-each update-count args)
    (when (call-known-function-primop? op)
     (update-count (call-known-function-primop-name op)))
    (hash-set! counts var 0)
    (let ((expr (recur expr)))
     (if (and (= 0 (hash-ref counts var)) (not (primop->side-effect op)))
         expr
         (bind-primop var ty op args expr))))
   ((bind-rec funs body)
    (for: ((p : (Pair unique function) funs))
     (hash-set! counts (car p) 0)
     (hash-set! counts (function-name (cdr p)) 0)
     (for: ((arg : (Pair unique Any) (function-args (cdr p))))
      (hash-set! counts (car arg) 0)))
    (bind-rec
     (map (lambda: ((p : (Pair unique function)))
      (cons (car p)
       (match (cdr p)
        ((function name args ret body)
         (function name args ret (recur body)))))) funs)
     (recur body)))
   ((conditional c t f ty)
    (update-count c)
    (conditional c (recur t) (recur f) ty))))
 (recur expr))
 
