#lang typed/racket/base

(require "../ir-anf-ast.rkt" "../types.rkt" "../unique.rkt" "../primop.rkt")
(require racket/match)


(provide alpha-rename)

(: alpha-rename (expression -> expression))
(define (alpha-rename expr)
 (: changed (HashTable unique unique))
 (define changed (make-hash))
 (: add-change (unique unique -> Void))
 (define (add-change old new)
  (hash-set! changed old new))
 (: rename (unique -> unique))
 (define (rename id) (hash-ref changed id (lambda () id)))

 (: primop-rename (primop -> primop))
 (define (primop-rename op)
  (match op
   ((call-known-function-primop ty name) (call-known-function-primop ty (rename name)))
   (else op)))

 (: recur (expression -> expression))
 (define (recur expr)
  (match expr
   ((conditional c t f ty)
    (conditional (rename c) (recur t) (recur f) ty))
   ((return name) (return (rename name)))
   ((bind-primop var ty op args expr)
    (let ((new-var (re-uniq var)))
     (hash-set! changed var new-var)
     (bind-primop new-var ty (primop-rename op) (map rename args) (recur expr))))
   ((bind-rec funs body)
    (let* ((old-clos-names (map (inst car unique function) funs))
           (old-funs (map (inst cdr unique function) funs))
           (old-fun-names (map function-name old-funs))
           (old-arg-names (map (lambda: ((args : (Listof (Pair unique type)))) (map (inst car unique type) args)) (map function-args old-funs)))
           (new-clos-names (map re-uniq old-clos-names))
           (new-fun-names (map re-uniq old-fun-names))
           (new-arg-names (map (lambda: ((args : (Listof unique))) (map re-uniq args)) old-arg-names)))

     (for-each add-change old-clos-names new-clos-names)
     (for-each add-change old-fun-names new-fun-names)
     (for-each
      (lambda: ((old-args : (Listof unique)) (new-args : (Listof unique)))
       (for-each add-change old-args new-args))
      old-arg-names new-arg-names)
     (bind-rec
      (map (lambda: ((p : (Pair unique function)))
       (cons (rename (car p))
        (match (cdr p)
         ((function name args ty body)
          (function (rename name) (map (lambda: ((p : (Pair unique type))) (cons (rename (car p)) (cdr p))) args) ty (recur body)))))) funs)
     (recur body))))))
 (recur expr))
