#lang typed/racket


(require "../types.rkt" "../ir-anf-ast.rkt" "../primop.rkt" "inline.rkt")

(require racket/match racket/list)

(provide inline-once-used)













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



(: tail-called-once? (unique expression -> Boolean))
(define (tail-called-once? var expr)

 (: called-once? (expression -> trit))
 (define (called-once? expr)
  (match expr
   ((bind-rec funs body)
    (merge-trit (called-once? body)
     (merge-trits (map (lambda: ((p : (Pair unique function))) (called-once? (function-body (cdr p)))) funs))))
   ((bind-primop bind-var ty op args expr)
    (if (and (or (call-closure-primop? op) (call-known-function-primop? op))
             (equal? (first args) var))
        (if (member var (rest args))
            'no
            (cond
             ((and (return? expr) (equal? (return-name expr) bind-var))
              (merge-trit 'yes (called-once? expr)))
             (else 'no)))
        (merge-trit (if (member var args) 'no 'not-used)  (called-once? expr))))
   ((conditional c t f ty)
     (merge-trit (called-once? t) (called-once? f)))
   ((return name)
    (if (equal? name var) 'no 'not-used))))

 
 (equal? (called-once? expr) 'yes))



(: called-once? (unique expression -> Boolean))
(define (called-once? var expr)

 (: called-once? (expression -> trit))
 (define (called-once? expr)
  (match expr
   ((bind-rec funs body)
    (merge-trit (called-once? body)
     (merge-trits (map (lambda: ((p : (Pair unique function))) (called-once? (function-body (cdr p)))) funs))))
   ((bind-primop bind-var ty op args expr)
    (if (and (or (call-closure-primop? op) (call-known-function-primop? op))
             (equal? (first args) var))
        (if (member var (rest args))
            'no
            (merge-trit 'yes (called-once? expr)))
        (merge-trit (if (member var args) 'no 'not-used)  (called-once? expr))))
   ((conditional c t f ty)
     (merge-trit (called-once? t) (called-once? f)))
   ((return name)
    (if (equal? name var) 'no 'not-used))))

 
 (equal? (called-once? expr) 'yes))






(: inline-once-used (expression -> expression))
(define (inline-once-used prog)

 (: fix (type -> (expression -> expression)))
 (define (fix return-type)
  (: recur (expression -> expression))
  (define (recur expr)
   (match expr
    ((bind-rec funs body)
     (let-values (((inlined expr)
      (for/fold: : (values (Listof unique) bind-rec)
       ((inlined-functions : (Listof unique) empty)
        (expr : bind-rec (bind-rec funs body)))
       ((fun : (Pair unique function) funs))
        (let ((do-inline (lambda () (values (cons (car fun) inlined-functions) (assert (inline (car fun) (cdr fun) expr return-type) bind-rec?))))
              (do-inline-and-call (lambda () (values (cons (car fun) inlined-functions) (assert (inline-and-call (car fun) (cdr fun) expr return-type) bind-rec?))))
              (dont-do-inline (lambda () (values inlined-functions expr))))
        (cond
         ((tail-called-once? (car fun) expr) (do-inline))
         ((called-once? (car fun) expr)
          (if (single-return-function? (cdr fun))
              (do-inline)
              (do-inline-and-call)))
         (else (dont-do-inline)))))))
      (match expr
       ((bind-rec funs body)
        (bind-rec 
         (map (lambda: ((p : (Pair unique function)))
          (cons (car p)
           (match (cdr p)
            ((function name args ty body)
             (function name args ty ((fix ty) body)))))) (filter (lambda: ((p : (Pair unique function))) (not (member (car p) inlined))) funs))
         (recur body))))))
    ((bind-primop var ty op args expr)
     (bind-primop var ty op args (recur expr)))
    ((conditional c t f ty)
     (conditional c (recur t) (recur f) ty))
    ((return name) expr)
    (else (error 'inline-once-used "Missing Case ~a" expr))))
  recur)
 
 ((fix unit-type) prog))



