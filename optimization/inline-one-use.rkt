#lang typed/racket


(require "../types.rkt" "../ir-ast.rkt" "../primop.rkt")

(require racket/match racket/list)

(provide inline-once-used)



(: inline (Symbol function expression -> expression))
(define (inline var fun expr)
 (: recur (expression -> expression))
 (define (recur expr)
  (match expr
   ((sequence first next)
    (sequence (recur first) (recur next)))
   ((bind var ty expr body)
    (bind var ty (recur expr) (recur body)))
   ((bind-rec funs body)
    (bind-rec
     (map (lambda: ((p : (Pair Symbol function)))
      (cons (car p)
       (match (cdr p)
        ((function name args ty body)
         (function name args ty (recur body)))))) funs)
     (recur body)))
   ((primop-expr op args)
    (if (and (or (call-known-function-primop? op) (call-closure-primop? op)) (let ((arg1 (first args))) (and (identifier? arg1) (equal? (identifier-name arg1) var))))
        (match fun
         ((function name fun-args return body)
          (foldr (lambda: ((arg : expression) (desc : (Pair Symbol type)) (expr : expression))
                  (bind (car desc) (cdr desc) arg expr)) body (map recur (rest args)) fun-args)))
        (primop-expr op (map recur args))))
   ((conditional c t f ty)
    (conditional (recur c) (recur t) (recur f) ty))
   ((identifier name) expr)
   (else (error 'inline "Missing Case ~a" expr))))

 
 (recur expr))


(: inline-once-used (expression -> expression))
(define (inline-once-used prog)
 (: called-once? (Symbol expression -> Boolean))
 (define (called-once? var expr)
  (define-type trit (U 'yes 'not-used 'no))
  (: merge-trit (trit trit -> trit))
  (define (merge-trit t1 t2)
   (case t2
    ((not-used) t1)
    ((no) 'no)
    ((yes) (case t1
            ((no) 'no)
            ((yes) 'no)
            ((not-used) 'yes)))))
  (: merge-trits ((Listof trit) -> trit))
  (define (merge-trits trits) (foldl merge-trit 'not-used trits))

  (: called-once? (expression -> trit))
  (define (called-once? expr)
   (match expr
    ((sequence first next)
     (merge-trit (called-once? first) (called-once? next)))
    ((bind var ty expr body)
     (merge-trit (called-once? expr) (called-once? body)))
    ((bind-rec funs body)
     (merge-trit (called-once? body)
      (merge-trits (map (lambda: ((p : (Pair Symbol function))) (called-once? (function-body (cdr p)))) funs))))
    ((primop-expr op args)
     (if (and (or (call-closure-primop? op) (call-known-function-primop? op))
          (let ((arg1 (first args)))
           (and (identifier? arg1) (equal? (identifier-name arg1) var))))
         (merge-trit 'yes (merge-trits (map called-once? (rest args))))
         (merge-trits (map called-once? args))))
    ((conditional c t f ty)
     (merge-trit (called-once? c)
      (merge-trit (called-once? t) (called-once? f))))
    ((identifier name)
     (if (equal? name var) 'no 'not-used))
    (else (error 'called-once? "Missing Case ~a" expr))))
  (equal? (called-once? expr) 'yes))

 (: recur (expression -> expression))
 (define (recur expr)
  (match expr
   ((sequence first next)
    (sequence (recur first) (recur next)))
   ((bind var ty expr body)
    (bind var ty (recur expr) (recur body)))
   ((bind-rec funs body)
    (let-values (((inlined expr)
     (for/fold: : (values (Listof Symbol) bind-rec)
      ((inlined-functions : (Listof Symbol) empty)
       (expr : bind-rec (bind-rec funs body)))
      ((fun : (Pair Symbol function) funs))
       (if (called-once? (car fun) expr)
           (values (cons (car fun) inlined-functions) (assert (inline (car fun) (cdr fun) expr) bind-rec?))
           (values inlined-functions expr)))))
     (match expr
      ((bind-rec funs body)
       (bind-rec 
        (map (lambda: ((p : (Pair Symbol function)))
         (cons (car p)
          (match (cdr p)
           ((function name args ty body)
            (function name args ty (recur body)))))) (filter (lambda: ((p : (Pair Symbol function))) (not (member (car p) inlined))) funs))
        (recur body))))))
   ((primop-expr op args)
    (primop-expr op (map recur args)))
   ((conditional c t f ty)
    (conditional (recur c) (recur t) (recur f) ty))
   ((identifier name) expr)
   (else (error 'inline-once-used "Missing Case ~a" expr))))
 
 (recur prog))



