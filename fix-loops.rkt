#lang typed/racket/base

(require "intermediate-ast.rkt" "types.rkt" "primop.rkt" "unique.rkt")
(require racket/match racket/list)
(require racket/pretty)

(provide (rename-out (fix-loops-top fix-loops)))


(define-type type-environment (HashTable unique type))
 

(: continuation-type (type -> function-type))
(define (continuation-type ty)
 (make-function-type (list ty) unit-type))


(: add-function-types ((Listof (Pair unique function)) type-environment -> type-environment))
(define (add-function-types funs env)
 (for/fold: : type-environment
  ((env : type-environment env))
  ((fun : (Pair unique function) funs))
  (hash-set env (car fun) (function->function-type (cdr fun)))))


(: app (continuation expression -> expression))
(define (app k val)
 (primop-expr (call-closure-primop (continuation-ty k)) (list (continuation-expr k) val)))

(define-struct: continuation ((expr : expression) (ty : function-type)))

(: cps (expression continuation type-environment -> expression))
(define (cps expr cont env)
 ;Not needed inner version. Maybe useful for extensions
 (: cps (expression continuation type-environment -> expression))
 (define (cps expr cont env)
  (match expr
   ((bind v ty expr body)
    (let* ((fun-name (gen-uniq 'cps-fun)) (env (hash-set env fun-name (continuation-type ty))))
     (bind-rec
      (list (cons fun-name (function (list (cons v ty)) unit-type
                                     (cps body cont (hash-set env v ty)))))
      (cps expr (continuation (identifier fun-name) (continuation-type ty)) env))))
   ((bind-rec funs body)
    (bind-rec funs (cps body cont (add-function-types funs env))))
   ((assignment name expr) (error 'cps "Unremoved assignment operator"))
   ((identifier name)
    (app cont expr))
   ((while-loop cond body)
    (error 'cps "Unremoved while loop"))
   ((for-loop id init final body)
    (error 'cps "Unremoved for-loop"))
   ((break) (primop-expr (unit-primop) empty))
   ((conditional c t f ty)
    (let* ((fun-name (gen-uniq 'cps-fun))
           (cont-name (gen-uniq 'cont-cond-fun))
           (val-name (gen-uniq 'condition-val))
           (cont-val-name (gen-uniq 'cont-val))
           (expr-type ty)
           (env (hash-set (hash-set env fun-name (continuation-type int-type)) cont-name (continuation-type expr-type))))
     (bind-rec (list (cons fun-name
                           (function
                            (list (cons val-name int-type))
                            unit-type
                            (conditional (identifier val-name)
                              (cps t (continuation (identifier cont-name) (continuation-type ty))
                                     (hash-set env val-name int-type))
                              (cps f (continuation (identifier cont-name) (continuation-type ty))
                                     (hash-set env val-name int-type)) unit-type)))
                     (cons cont-name
                           (function
                            (list (cons cont-val-name expr-type))
                            unit-type
                            (app cont (identifier cont-val-name)))))
      (cps c (continuation (identifier fun-name) (continuation-type int-type)) env))))
  ((primop-expr op exprs)
   (let ((names (map (lambda: ((e : expression)) (gen-uniq 'primop-arg)) exprs)))
    (for/fold: : expression
     ((final-expr : expression (app cont (primop-expr op (map identifier names)))))
     ((name : unique (reverse names))
      (expr : expression (reverse exprs)))
     (let ((fun-name (gen-uniq 'cps-fun)) (e-type (type-of expr env)))
      (bind-rec (list (cons fun-name (function (list (cons name e-type)) unit-type final-expr)))
       (cps expr (continuation (identifier fun-name) (continuation-type e-type)) env))))))))

        
 (cps expr cont env)) 
 
(: fix-for-loop (for-loop type-environment -> expression))
(define (fix-for-loop loop env)
 (match loop
  ((for-loop var init final body)
   (let ((init-name (gen-uniq 'init))
         (final-name (gen-uniq 'final))
         (fun-name (gen-uniq 'for-loop))
         (cont-name (gen-uniq 'continue)))
    (bind init-name int-type init 
     (bind final-name int-type final 
      (bind-rec (list (cons fun-name
                       (function
                        (list (cons var int-type))
                        unit-type
                        (bind-rec (list (cons cont-name 
                                         (function (list (cons (gen-uniq 'ignored) unit-type))
                                          unit-type
                                          (primop-expr (call-closure-primop (continuation-type int-type))
                                           (list
                                            (identifier fun-name)
                                            (primop-expr (math-primop '+)
                                              (list
                                               (primop-expr (integer-constant-primop 1) empty)
                                               (identifier var))))))))
                          (conditional (primop-expr (comparison-primop '<= int-type) (list (identifier var) (identifier final-name)))
                            (cps body (continuation (identifier cont-name) (continuation-type unit-type)) (hash-set env var int-type))
                            (primop-expr (unit-primop) empty)
                            unit-type)))))
       (primop-expr (call-closure-primop (continuation-type int-type)) (list (identifier fun-name) (identifier init-name))))))))))

(: fix-while-loop (while-loop type-environment -> expression))
(define (fix-while-loop loop env)
 (match loop
  ((while-loop cond body)
   (let ((fun-name  (gen-uniq 'while-loop)))
    (bind-rec (list (cons fun-name
                     (function (list (cons (gen-uniq 'ignored) unit-type))
                      unit-type
                      (conditional cond
                       (cps body (continuation (identifier fun-name) (continuation-type unit-type)) env)
                       (primop-expr (unit-primop) empty)
                       unit-type))))
     (primop-expr (call-closure-primop (continuation-type unit-type)) (list (identifier fun-name) (primop-expr (unit-primop) empty))))))))

(: fix-loops-top (expression -> expression))
(define (fix-loops-top expr) (fix-loops expr (make-immutable-hash empty)))


(: fix-loops (expression type-environment -> expression))
(define (fix-loops expr env)


 (: fix-loops-fun (type-environment -> ((Pair unique function) -> (Pair unique function))))
 (define ((fix-loops-fun env) fun)
  (cons (car fun)
   (match (cdr fun)
    ((function args return body)
     (function args return
      (fix-loops body
       (for/fold: : type-environment
        ((env : type-environment env))
        ((arg : (Pair unique type) args))
        (hash-set env (car arg) (cdr arg)))))))))
 (match expr
  ((bind v ty expr body)
   (bind v ty (fix-loops expr env) (fix-loops body (hash-set env v ty))))
  ((bind-rec funs body)
   (let ((env (add-function-types funs env)))
    (bind-rec (map (fix-loops-fun env) funs) (fix-loops body env))))
  ((assignment name expr) (error 'cps "Unremoved assignment operator"))
  ((identifier name) expr)
  ((while-loop cond body)
   (fix-while-loop (while-loop (fix-loops cond env) (fix-loops body env)) env))
  ((for-loop id init final body)
   (fix-for-loop
    (for-loop id
     (fix-loops init env)
     (fix-loops final env)
     (fix-loops body (hash-set env id int-type))) env))
  ((break) expr)
  ((conditional c t f ty)
   (conditional (fix-loops c env) (fix-loops t env) (fix-loops f env) ty))
  ((primop-expr op exprs)
   (primop-expr op (map (lambda: ((e : expression)) (fix-loops e env)) exprs)))))
                      
   
 


