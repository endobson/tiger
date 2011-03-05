#lang typed/racket


(require "../types.rkt" "../ir-anf-ast.rkt" "../primop.rkt" "alpha-rename.rkt" "../unique.rkt")

(require racket/match racket/list)

(provide inline-tail-once-used)




(: replace-all (expression (HashTable unique unique) -> expression))
(define (replace-all expr env)
 (: rename (unique -> unique))
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
     (map (lambda: ((p : (Pair unique function)))
      (cons (car p)
       (match (cdr p)
        ((function name args ty body)
         (function name args ty (recur body)))))) funs)
     (recur body)))))
 (recur expr))


(: inline-and-call (unique function expression type -> expression))
(define (inline-and-call sym fun expr return-type)
 (: recur (expression -> expression))
 (define (recur expr)
  (match expr
   ((bind-rec funs body)
    (bind-rec
     (map (lambda: ((p : (Pair unique function)))
      (cons (car p)
       (match (cdr p)
        ((function name args ty body)
         (function name args ty (recur body)))))) funs)
     (recur body)))
   ((bind-primop bind-var ty op args expr)
    (if (and (or (call-known-function-primop? op) (call-closure-primop? op))
             (equal? (first args) sym))
        (match fun
         ((function name fun-args f-return body)
          (let ((new-bind-var (re-uniq bind-var)) (clos-name (gen-uniq 'rest)) (fun-name (gen-uniq 'rest)) (ret-val (gen-uniq 'return)))
           (bind-rec (list (cons clos-name
                                 (function 
                                  fun-name
                                  (list (cons bind-var ty))
                                  return-type
                                  (recur expr))))
            (bind-expression
             new-bind-var
             (alpha-rename
              (replace-all body
               (make-immutable-hash
                (map (inst cons unique unique)
                 (map (inst car unique type) fun-args)
                      (rest args)))))
             (bind-primop ret-val return-type (call-known-function-primop (make-function-type (list ty) return-type) fun-name) (list clos-name new-bind-var) (return ret-val)))))))
        (bind-primop bind-var ty op args (recur expr))))
   ((conditional c t f ty)
    (conditional c (recur t) (recur f) ty))
   ((return name) expr)))

 (recur expr))


(: inline (unique function expression -> expression))
(define (inline sym fun expr)
 (: recur (expression -> expression))
 (define (recur expr)
  (match expr
   ((bind-rec funs body)
    (bind-rec
     (map (lambda: ((p : (Pair unique function)))
      (cons (car p)
       (match (cdr p)
        ((function name args ty body)
         (function name args ty (recur body)))))) funs)
     (recur body)))
   ((bind-primop bind-var ty op args expr)
    (if (and (or (call-known-function-primop? op) (call-closure-primop? op))
             (equal? (first args) sym))
        (match fun
         ((function name fun-args return body)
          (bind-expression
           bind-var
           (alpha-rename
            (replace-all body
             (make-immutable-hash
              (map (inst cons unique unique)
               (map (inst car unique type) fun-args)
                    (rest args)))))
           (recur expr))))
        (bind-primop bind-var ty op args (recur expr))))
   ((conditional c t f ty)
    (conditional c (recur t) (recur f) ty))
   ((return name) expr)))

 (recur expr))




(: bind-expression (unique expression expression -> expression))
(define (bind-expression sym expr1 expr2)
 (: recur (expression -> expression))
 (define (recur expr)
  (match expr
   ((return name) (fix name))
   ((bind-primop var ty op args expr)
    (bind-primop var ty op args (recur expr)))
   ((bind-rec funs expr) (bind-rec funs (recur expr)))
   ((conditional c t f ty)
    (conditional c (recur (alpha-rename t)) (recur (alpha-rename f)) ty))))

  
  
 (: fix (unique -> expression))
 (define (fix sym2)
  (: rename (unique -> unique))
  (define (rename var) (if (equal? var sym) sym2 var))
  (: recur (expression -> expression))
  (define (recur expr)
   (match expr
    ((return name) (return (rename name)))
    ((bind-primop var ty op args expr)
     (bind-primop var ty op (map rename args) (recur expr)))
    ((bind-rec funs body) 
     (bind-rec
      (map (lambda: ((p : (Pair unique function)))
       (cons (car p)
        (match (cdr p)
         ((function name args ty body)
          (function name args ty (recur body)))))) funs)
      (recur body)))
    ((conditional c t f ty)
     (conditional (rename c) (recur t) (recur f) ty))))
  
  
  (recur expr2))

 (recur expr1))


(: single-return-function? (function -> Boolean))
(define (single-return-function? fun)
 (match fun
  ((function name args ty body)
   (single-return-expression? body))))

(: single-return-expression? (expression -> Boolean))
(define (single-return-expression? expr)
 (match expr
  ((return name) #t)
  ((bind-primop var ty op args expr) (single-return-expression? expr))
  ((bind-rec funs expr) (single-return-expression? expr))
  ((conditional c t f ty) #f)))



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






(: inline-tail-once-used (expression -> expression))
(define (inline-tail-once-used prog)

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
        (let ((do-inline (lambda () (values (cons (car fun) inlined-functions) (assert (inline (car fun) (cdr fun) expr) bind-rec?))))
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



