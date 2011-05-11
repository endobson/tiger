#lang typed/racket/base

(require "../ir-anf-ast.rkt" "side-effects.rkt" "../primop.rkt")
(require "../ir-anf-printable-ast.rkt" racket/pretty)
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
    (let ((body (recur body))
          (bodies 
           (map (lambda: ((p : (Pair unique function)))
            (recur (function-body (cdr p)))) funs)))
     (bind-rec
      (filter-map (lambda: ((p : (Pair unique function)) (new-body : expression))
       (match (cdr p)
        ((function name args ret old-body)
         (and (> (+ (hash-ref counts name) (hash-ref counts (car p))) 0)
          (cons (car p)
           (function name args ret new-body)))))) funs bodies)
      body)))
   ((conditional c t f ty)
    (update-count c)
    (conditional c (recur t) (recur f) ty))))
 (recur expr))
 
