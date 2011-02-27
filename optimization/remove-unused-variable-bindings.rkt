#lang typed/racket/base

(require "../ir-ast.rkt" "../primop.rkt")
(require racket/match racket/list)

(require "../ir-printable-ast.rkt")
(require/typed racket/pretty
 (pretty-write (Any -> Void)))

(provide remove-unused-variable-bindings)

(: remove-unused-variable-bindings (expression -> expression))
(define (remove-unused-variable-bindings expr)
 (: counts (HashTable Symbol Natural))
 (define counts (make-hash))
 (: recur (expression -> expression))
 (define (recur expr)
  (match expr
   ((identifier name)
    (hash-update! counts name add1
     (lambda () (error 'remove-unused-variable-bindings "Unbound identifier ~a in ~a" name counts)))
    expr)
   ((primop-expr op args)
    (when (call-known-function-primop? op)
     (let ((name (call-known-function-primop-name op)))
      (hash-update! counts name add1
       (lambda () (error 'remove-unused-variable-bindings "Unknown function ~a in ~a" name counts)))))
    (primop-expr op (map recur args)))
   ((bind var ty expr body)
    (hash-set! counts var 0)
    (let ((expr (recur expr)) (body (recur body)))
     (if (= 0 (hash-ref counts var))
         (sequence expr body)
         (bind var ty expr body))))
   ((bind-rec funs body)
    (for: ((p : (Pair Symbol function) funs))
     (hash-set! counts (car p) 0)
     (hash-set! counts (function-name (cdr p)) 0)
     (for: ((arg : (Pair Symbol Any) (function-args (cdr p))))
      (hash-set! counts (car arg) 0)))
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
 
