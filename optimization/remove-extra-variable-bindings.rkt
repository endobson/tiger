#lang typed/racket/base


(require "../ir-ast.rkt")
(require racket/match racket/list)


(require "../ir-printable-ast.rkt")
(require/typed racket/pretty
 (pretty-write (Any -> Void)))


(provide remove-extra-variable-bindings)

(: remove-extra-variable-bindings (expression -> expression))
(define (remove-extra-variable-bindings expr)
 (define-type environment (HashTable Symbol Symbol))

 (: lookup-name (Symbol environment -> Symbol))
 (define (lookup-name name env)
  (hash-ref env name (lambda () name)))
 (: remove (environment -> (expression -> expression)))
 (define (remove env)
  (: recur (expression -> expression))
  (define (recur expr)
   (match expr
    ((identifier name) (identifier (lookup-name name env)))
    ((primop-expr op args) (primop-expr op (map recur args)))
    ((bind var ty expr body)
     (if (identifier? expr)
         ((remove (hash-set env var (lookup-name (identifier-name expr) env))) body)
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
 
