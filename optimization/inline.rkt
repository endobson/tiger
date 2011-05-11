#lang typed/racket/base

(require "../types.rkt" "../ir-anf-ast.rkt" "../primop.rkt" "alpha-rename.rkt" "../unique.rkt")

(require "../ir-anf-printable-ast.rkt")

(require/typed racket/pretty
 (pretty-write (Any -> Void)))

(require racket/match racket/list)


(provide inline-and-call inline single-return-function? smart-inline smart-inline-except-self)

(: smart-inline (unique function expression type -> expression))
(define (smart-inline sym fun expr return-type)
 ((if (single-return-function? fun)
      inline inline-and-call) sym fun expr return-type))

(: smart-inline-except-self (unique function bind-rec type -> bind-rec))
(define (smart-inline-except-self sym fun expr return-type)
 (inline-except-self sym fun expr return-type 
   (if (single-return-function? fun)
      inline inline-and-call)))



(: inline-except-self (unique function bind-rec type
                       (unique function expression type -> expression)
                       -> bind-rec))
(define (inline-except-self sym fun expr return-type inliner)
 (match expr
  ((bind-rec funs body)
   (bind-rec
    (map (lambda: ((p : (Pair unique function)))
     (if (equal? (car p) sym) p
      (cons (car p)
       (match (cdr p)
        ((function name args ty body)
         (function name args ty (inliner sym fun body ty))))))) funs)
    (inliner sym fun body return-type)))))


(: inline-and-call (unique function expression type -> expression))
(define (inline-and-call sym fun expr return-type)
 (: fix (type -> (expression -> expression)))
 (define (fix return-type)
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
      ((fix return-type) body)))
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
              (bind-primop ret-val return-type
                           (call-known-function-primop (make-function-type (list ty) return-type) fun-name)
                           (list clos-name new-bind-var) (return ret-val))
              return-type)))))
         (bind-primop bind-var ty op args (recur expr))))
    ((conditional c t f ty)
     (conditional c (recur t) (recur f) return-type))
    ((return name) expr)))
  recur)

 ((fix return-type) expr))


(: inline (unique function expression type -> expression))
(define (inline sym fun expr return-type)
 (: fix (type -> (expression -> expression)))
 (define (fix return-type)
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
      ((fix return-type) body)))
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
            (recur expr)
            return-type)))
         (bind-primop bind-var ty op args (recur expr))))
    ((conditional c t f ty)
     (conditional c (recur t) (recur f) ty))
    ((return name) expr)))
  recur)

 ((fix return-type) expr))



(: bind-expression (unique expression expression type -> expression))
(define (bind-expression sym expr1 expr2 return-type)
 (: recur (expression -> expression))
 (define (recur expr)
  (match expr
   ((return name) (fix name))
   ((bind-primop var ty op args expr)
    (bind-primop var ty op args (recur expr)))
   ((bind-rec funs expr) (bind-rec funs (recur expr)))
   ((conditional c t f ty)
    (conditional c (recur t) (recur f) return-type))))

  
  
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
  
  
  (recur (alpha-rename expr2)))

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





