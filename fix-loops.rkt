#lang typed/racket/base

(require "intermediate-ast.rkt" "types.rkt" "primop.rkt")
(require racket/match racket/list)

(provide (rename-out (fix-loops-top fix-loops)))


(define-type type-environment (HashTable Symbol type))
(: type-of (expression type-environment -> type))
(define (type-of expr env)
 (match expr 
  ((identifier id) (hash-ref env id (lambda () (error 'cps "Unbound identifier ~a in ~a" id env))))
  ((primop-expr op args) 
   (match op
    ((call-closure-primop ty) (function-type-return-type ty))
    ((math-primop sym) int-type)
    ((equality-primop eql type) int-type)
    ((unit-primop) unit-type)
    ((integer-constant-primop val) int-type)
    ((string-constant-primop val) string-type) 
    ((nil-primop ty) ty)
    ((runtime-primop ty name) ty) 
    ((box-set!-primop ty) unit-type)
    ((array-set!-primop ty) unit-type) 
    ((field-set!-primop ty field) unit-type)

    ((box-ref-primop ty) (box-type-elem-type ty)) 
    ((array-ref-primop ty) (array-type-elem-type ty))
    ((field-ref-primop ty field) (record-type-field-type ty field))
    
    ((create-box-primop ty) ty)
    ((create-record-primop ty) ty)
    ((create-array-primop ty) ty) 

    (else (error 'type-of "Not yet implemented primop ~a" op))))
  
  (else (error 'type-of "Not yet implemented ~a" expr))))
 

(: continuation-type (type -> function-type))
(define (continuation-type ty)
 (make-function-type (list ty) unit-type))


(: add-function-types ((Listof (Pair Symbol function)) type-environment -> type-environment))
(define (add-function-types funs env)
 (for/fold: : type-environment
  ((env : type-environment env))
  ((fun : (Pair Symbol function) funs))
  (hash-set env (car fun) (function->type (cdr fun)))))

(: function->type (function -> function-type))
(define (function->type fun)
 (make-function-type (map (inst cdr Symbol type) (function-args fun)) (function-return-type fun)))

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
    (let* ((fun-name (gensym 'cps-fun)) (env (hash-set env fun-name (continuation-type ty))))
     (bind-rec
      (list (cons fun-name (function (list (cons v ty)) unit-type
                                     (cps body cont (hash-set env v ty)))))
      (cps expr (continuation (identifier fun-name) (continuation-type ty)) env))))
   ((bind-rec funs body)
    (bind-rec funs (cps body cont (add-function-types funs env))))
   ((sequence e1 e2)
    (cps (bind (gensym 'ignored) (type-of e1 env) e1 e2) cont env))
   ((assignment name expr) (error 'cps "Unremoved assignment operator"))
   ((identifier name)
    (app cont expr))
   ((while-loop cond body)
    (error 'cps "Unremoved while loop"))
   ((for-loop id init final body)
    (error 'cps "Unremoved for-loop"))
   ((break) (primop-expr (unit-primop) empty))
   ((conditional c t f ty)
    (let* ((fun-name (gensym 'cps-fun))
           (cont-name (gensym 'cont-cond-fun))
           (val-name (gensym 'condition-val))
           (cont-val-name (gensym 'cont-val))
           (expr-type ty)
           (env (hash-set (hash-set env fun-name (continuation-type int-type)) cont-name (continuation-type expr-type))))
     (bind-rec (list (cons fun-name
                           (function
                            (list (cons val-name int-type))
                            unit-type
                            (if val-name (cps t (continuation (identifier cont-name) (continuation-type ty))
                                                (hash-set env val-name int-type))
                                         (cps f (continuation (identifier cont-name) (continuation-type ty))
                                                (hash-set env val-name int-type)))))
                     (cons cont-name
                           (function
                            (list (cons cont-val-name expr-type))
                            unit-type
                            (app cont (identifier cont-val-name)))))
      (cps c (continuation (identifier fun-name) (continuation-type int-type)) env))))
  ((primop-expr op exprs)
   (let ((names (map (lambda: ((e : expression)) (gensym 'primop-arg)) exprs)))
    (for/fold: : expression
     ((final-expr : expression (app cont (primop-expr op (map identifier names)))))
     ((name : Symbol (reverse names))
      (expr : expression (reverse exprs)))
     (let ((fun-name (gensym 'cps-fun)) (e-type (type-of expr env)))
      (bind-rec (list (cons fun-name (function (list (cons name e-type)) unit-type (continuation-expr cont))))
       (cps expr (continuation (identifier fun-name) (continuation-type e-type)) env))))))))

        
 (cps expr cont env)) 
 
(: fix-for-loop (for-loop type-environment -> expression))
(define (fix-for-loop loop env)
 (match loop
  ((for-loop var init final body)
   (let ((init-name (gensym 'init))
         (final-name (gensym 'final))
         (fun-name (gensym 'for-loop))
         (cont-name (gensym 'continue)))
    (bind init-name int-type init 
     (bind final-name int-type final 
      (bind-rec (list (cons fun-name
                       (function
                        (list (cons var int-type))
                        unit-type
                        (bind-rec (list (cons cont-name 
                                         (function (list (cons (gensym 'ignored) unit-type))
                                          unit-type
                                          (primop-expr (call-closure-primop (continuation-type int-type))
                                           (list
                                            (identifier fun-name)
                                            (primop-expr (math-primop '+)
                                              (list
                                               (primop-expr (integer-constant-primop 1) empty)
                                               (identifier var))))))))
                          (conditional (primop-expr (equality-primop #t int-type) (list (identifier var) (identifier final-name)))
                            (cps body (continuation (identifier cont-name) (continuation-type unit-type)) (hash-set env var int-type))
                            (primop-expr (unit-primop) empty)
                            unit-type)))))
       (primop-expr (call-closure-primop (continuation-type int-type)) (list (identifier fun-name) (identifier init-name))))))))))

(: fix-while-loop (while-loop type-environment -> expression))
(define (fix-while-loop loop env)
 (match loop
  ((while-loop cond body)
   (let ((fun-name  (gensym 'while-loop)))
    (bind-rec (list (cons fun-name
                     (function (list (cons (gensym 'ignored) unit-type))
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
 (: fix-loops-fun (type-environment -> ((Pair Symbol function) -> (Pair Symbol function))))
 (define ((fix-loops-fun env) fun)
  (cons (car fun)
   (match (cdr fun)
    ((function args return body)
     (function args return
      (fix-loops body
       (for/fold: : type-environment
        ((env : type-environment env))
        ((arg : (Pair Symbol type) args))
        (hash-set env (car arg) (cdr arg)))))))))
 (match expr
  ((bind v ty expr body)
   (bind v ty (fix-loops expr env) (fix-loops body (hash-set env v ty))))
  ((bind-rec funs body)
   (let ((env (add-function-types funs env)))
    (bind-rec (map (fix-loops-fun env) funs) (fix-loops body env))))
  ((sequence e1 e2)
   (sequence (fix-loops e1 env) (fix-loops e2 env)))
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
                      
   
 


