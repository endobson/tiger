#lang typed/racket/base

(require "types.rkt" "ir-ast.rkt" "primop.rkt")
(require racket/list racket/match)

(provide remove-units)

(: remove-units (expression -> expression))
(define (remove-units expr)
 (define-type environment (HashTable Symbol #t))
 (: fix-function (environment -> ((Pair Symbol function) -> (Pair Symbol function))))
 (define ((fix-function env) pair)
  (cons (car pair)
   (match (cdr pair)
    ((function name args return body)
     (let* ((bad-args (filter-map (lambda: ((arg : (Pair Symbol type))) (and (unit-type? (cdr arg)) (car arg))) args))
            (good-args (filter (lambda: ((arg : (Pair Symbol type))) (not (unit-type? (cdr arg)))) args))
            (env (for/fold: : environment ((env : environment env)) ((arg : Symbol bad-args)) (hash-set env arg #t))))
      (function name good-args return ((fix env) body)))))))

 (: unit expression)
 (define unit (primop-expr (unit-primop) empty))

 (: fix (environment -> (expression -> expression)))
 (define (fix env)
  (: recur (expression -> expression))
  (define (recur expr)
   (match expr
    ((identifier name) (if (hash-has-key? env name) unit expr))
    ((primop-expr op args)
     (match op
      ((or (call-closure-primop ty) (call-known-function-primop ty _))
       (let* ((arg-types (function-type-arg-types ty))
              (unit-slots (map unit-type? arg-types))
              (has-unit (ormap (inst values Boolean) unit-slots))
              (new-arg-types (filter-map (lambda: ((u : Boolean) (type : type))
                                            (if u #f type)) unit-slots arg-types)))
        (if has-unit
            (let ((fun (recur (first args))) (args (rest args)))
             (let* ((names (map (lambda (t) (gensym 'temp)) arg-types))
                    (good-names (filter-map (lambda: ((u : Boolean) (name : Symbol))
                                             (if u #f name)) unit-slots names)))
              (for/fold: : expression
               ((expr : expression
                 (primop-expr (call-closure-primop (make-function-type new-arg-types
                                                     (function-type-return-type ty)))
                              (cons fun (map identifier good-names)))))
               ((name : Symbol (reverse names))
                (unit? : Boolean (reverse unit-slots))
                (ty : type (reverse arg-types))
                (arg : expression (reverse args)))
               (if unit?
                   (sequence (recur arg) expr)
                   (bind name ty (recur arg) expr)))))
            (primop-expr op (map recur args)))))
      (else (primop-expr op (map recur args)))))
    ((bind var ty expr body)
     (if (unit-type? ty)
         (sequence (recur expr) ((fix (hash-set env var #t)) body))
         (bind var ty (recur expr) (recur body))))
    ((bind-rec funs body)
     (bind-rec (map (fix-function env) funs) (recur body)))
    ((sequence first next)
     (sequence (recur first) (recur next)))
    ((conditional c t f ty)
     (conditional (recur c) (recur t) (recur f) ty))))
  recur)
 ((fix (make-immutable-hash empty)) expr))
