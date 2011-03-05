#lang typed/racket/base

(require "types.rkt" "ir-anf-ast.rkt" "primop.rkt" "unique.rkt")
(require racket/list racket/match)

(provide remove-units)

(: remove-units (expression -> expression))
(define (remove-units expr)
 (define-type environment (HashTable unique #t))
 (: fix-function (environment -> ((Pair unique function) -> (Pair unique function))))
 (define ((fix-function env) pair)
  (cons (car pair)
   (match (cdr pair)
    ((function name args return body)
     (let* ((bad-args (filter-map (lambda: ((arg : (Pair unique type))) (and (unit-type? (cdr arg)) (car arg))) args))
            (good-args (filter (lambda: ((arg : (Pair unique type))) (not (unit-type? (cdr arg)))) args))
            (env (for/fold: : environment ((env : environment env)) ((arg : unique bad-args)) (hash-set env arg #t))))
      (function name good-args return ((fix env) body)))))))


 (: fix (environment -> (expression -> expression)))
 (define (fix env)
  (: recur (expression -> expression))
  (define (recur expr)
   (match expr
    ((return name)
     (if (hash-has-key? env name)
         (let ((unit-name (gen-uniq 'unit)))
           (bind-primop unit-name unit-type (unit-primop) empty (return unit-name))) expr))
    ((bind-primop var bind-type op args expr)
     (match op
      ((or (call-closure-primop ty) (call-known-function-primop ty _))
       (let ((ty (assert ty function-type?)))
        (let* ((arg-types (function-type-arg-types ty))
               (unit-slots (map unit-type? arg-types))
               (new-arg-types (filter-map (lambda: ((u : Boolean) (type : type)) (if u #f type)) unit-slots arg-types))
               (new-args (filter-map (lambda: ((u : Boolean) (arg : unique)) (if u #f arg)) unit-slots (rest args))))
         (bind-primop var bind-type (call-closure-primop (make-function-type new-arg-types (function-type-return-type ty))) (cons (first args) new-args) (recur expr)))))
      (else
       (bind-primop var bind-type op args (recur expr)))))
    ((bind-rec funs body)
     (bind-rec (map (fix-function env) funs) (recur body)))
    ((conditional c t f ty)
     (conditional c (recur t) (recur f) ty))))
  recur)
 
  ((fix (make-immutable-hash empty)) expr))
