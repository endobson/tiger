#lang typed/racket


(require "../types.rkt" "../ir-anf-ast.rkt" "../primop.rkt")

(require racket/match racket/list)

(provide inline-tail-once-used)




(: replace-all (expression (HashTable Symbol Symbol) -> expression))
(define (replace-all expr env)
 (: rename (Symbol -> Symbol))
 (define (rename sym)
  (hash-ref env sym (lambda () sym)))
 (: recur (expression -> expression))
 (define (recur expr)
  (match expr
   ((conditional c t f ty)
    (conditional (rename c) (recur t) (recur f) ty))
   ((return name) (return (rename name)))
   ((bind-primop var ty op args expr)
    (bind-primop var ty op (map rename args) (recur expr)))
   ((bind-rec funs body)
    (bind-rec
     (map (lambda: ((p : (Pair Symbol function)))
      (cons (car p)
       (match (cdr p)
        ((function name args ty body)
         (function name args ty (recur body)))))) funs)
     (recur body)))))
 (recur expr))


(: inline-tail (Symbol function expression -> expression))
(define (inline-tail var fun expr)
 (: recur (expression -> expression))
 (define (recur expr)
  (match expr
   ((bind-rec funs body)
    (bind-rec
     (map (lambda: ((p : (Pair Symbol function)))
      (cons (car p)
       (match (cdr p)
        ((function name args ty body)
         (function name args ty (recur body)))))) funs)
     (recur body)))
   ((bind-primop bind-var ty op args expr)
    (if (and (or (call-known-function-primop? op) (call-closure-primop? op))
             (equal? (first args) var)
             (and (return? expr) (equal? bind-var (return-name expr))))
        (match fun
         ((function name fun-args return body)
          (replace-all body (make-immutable-hash (map (inst cons Symbol Symbol) (map (inst car Symbol type) fun-args) (rest args))))))
        (bind-primop bind-var ty op args (recur expr))))
   ((conditional c t f ty)
    (conditional c (recur t) (recur f) ty))
   ((return name) expr)
   (else (error 'inline "Missing Case ~a" expr))))

 (recur expr))




(: inline-tail-once-used (expression -> expression))
(define (inline-tail-once-used prog)
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
    ((bind-rec funs body)
     (merge-trit (called-once? body)
      (merge-trits (map (lambda: ((p : (Pair Symbol function))) (called-once? (function-body (cdr p)))) funs))))
    ((bind-primop bind-var ty op args expr)
     (if (and (or (call-closure-primop? op) (call-known-function-primop? op))
              (equal? (first args) var))
         (if (member var (rest args))
             'no
             (if (and (return? expr) (equal? (return-name expr) bind-var))
                 (merge-trit 'yes (called-once? expr))
                 'no))
         (merge-trit (if (member var args) 'no 'not-used)  (called-once? expr))))
    ((conditional c t f ty)
      (merge-trit (called-once? t) (called-once? f)))
    ((return name)
     (if (equal? name var) 'no 'not-used))
    (else (error 'called-once? "Missing Case ~a" expr))))

  
  (equal? (called-once? expr) 'yes))


 (: recur (expression -> expression))
 (define (recur expr)
  (match expr
   ((bind-rec funs body)
    (let-values (((inlined expr)
     (for/fold: : (values (Listof Symbol) bind-rec)
      ((inlined-functions : (Listof Symbol) empty)
       (expr : bind-rec (bind-rec funs body)))
      ((fun : (Pair Symbol function) funs))
       (if (called-once? (car fun) expr)
           (values (cons (car fun) inlined-functions) (assert (inline-tail (car fun) (cdr fun) expr) bind-rec?))
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
   ((bind-primop var ty op args expr)
    (bind-primop var ty op args (recur expr)))
   ((conditional c t f ty)
    (conditional c (recur t) (recur f) ty))
   ((return name) expr)
   (else (error 'inline-once-used "Missing Case ~a" expr))))
 
 (recur prog))



