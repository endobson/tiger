#lang typed/racket/base


(require "ir-ast.rkt")
(require racket/match racket/list)

(provide remove-extra-variable-bindings)

(: remove-extra-variable-bindings (expression -> expression))
(define (remove-extra-variable-bindings expr)
 (define-type environment (HashTable Symbol Symbol))
 (: remove (environment -> (expression -> expression)))
 (define (remove env)
  (: recur (expression -> expression))
  (define (recur expr)
   (match expr
    ((identifier name) (identifier (hash-ref env name (lambda () name))))
    ((primop-expr op args) (primop-expr op (map recur args)))
    ((bind var ty expr body)
     (if (identifier? expr)
         ((remove (hash-set env var (identifier-name expr))) body)
         (bind var ty (recur expr) (recur body))))
    ((bind-rec funs body)
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
  recur)
 ((remove (make-immutable-hash empty)) expr))
 
