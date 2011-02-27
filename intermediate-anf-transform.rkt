#lang typed/racket/base


(require racket/match racket/list)
(require (prefix-in anf: "ir-anf-ast.rkt")
         (prefix-in inter: "intermediate-ast.rkt")
         "primop.rkt"
         "types.rkt")

(provide transform)

(struct: context
 ((done : (anf:expression -> anf:expression))
  (current-sym : Symbol)
  (current-type : type)
  (todo : ((anf:expression -> anf:expression) -> anf:expression))))


(: transform (inter:expression type -> anf:expression))
(define (transform expr ty)
  (: renamed (HashTable Symbol Symbol))
  (define renamed (make-hash))
  
  (: pos-rename (Symbol -> Symbol))
  (define (pos-rename sym)
   (hash-ref renamed sym (lambda () sym)))

  (: fill-context (context (U Symbol (Pair primop (Listof Symbol))) -> anf:expression))
  (define (fill-context ctx arg)
   (match ctx
    ((context done current-sym current-ty todo)
     (if (symbol? arg)
         (begin
          (hash-set! renamed current-sym arg)
          (todo done))
         (todo (lambda: ((expr : anf:expression))
                (done (anf:bind-primop current-sym current-ty (first arg) (rest arg) expr))))))))
  (: process-function ((Pair Symbol inter:function) -> (Pair Symbol anf:function)))
  (define (process-function p)
   (cons (car p)
    (match (cdr p)
     ((inter:function args ret body)
      (anf:function (gensym (car p)) args ret (top-process body ret))))))
    

  (: top-process (inter:expression type -> anf:expression))
  (define (top-process expr ty)
   (let ((return (gensym 'retval)))
    (process expr 
     (context (inst values anf:expression) return ty (lambda: ((cont : (anf:expression -> anf:expression))) (cont (anf:return (pos-rename return))))))))


  (: process (inter:expression context -> anf:expression))
  (define (process expr ctx)
   (match expr
    ((inter:identifier name) (fill-context ctx name))
    ((inter:primop-expr op args)
     (match ctx
      ((context done sym ty todo)
       (: arg-processor ((Listof Symbol) (Listof inter:expression) (Listof type) -> (anf:expression -> anf:expression) -> anf:expression))
       (define ((arg-processor done-args next-args next-arg-types) cont)
        (if (empty? next-args)
            (fill-context (context cont sym ty todo) (cons op (reverse done-args)))
            (let ((next-sym (gensym 'primop-arg)))
             (process (first next-args) (context cont next-sym (first next-arg-types) (arg-processor (cons next-sym done-args) (rest next-args) (rest next-arg-types)))))))
            
       ((arg-processor empty args (primop-arg-types op)) done))))
    ((inter:conditional c t f ty)
     (let ((clos-name (gensym 'cond)) (arg-name (gensym 'cond-arg)) (result-name (gensym 'result)))
      (match ctx
       ((context done sym ty todo)
        (process c (context (lambda: ((expr : anf:expression))
                              (anf:bind-rec (list (cons clos-name
                               (anf:function (gensym 'cond-fun)
                                             (list (cons arg-name int-type))
                                             ty
                                             (anf:conditional arg-name (top-process t ty) (top-process f ty) ty))))
                               (anf:bind-primop sym ty (call-closure-primop (make-function-type (list int-type) ty)) (list (pos-rename result-name)) expr)))
                            result-name
                            int-type
                            todo))))))
    ((inter:bind var ty expr body)
     (match ctx
      ((context done sym type todo)
       (process
        expr
        (context done var ty (lambda: ((cont : (anf:expression -> anf:expression))) (process body (context cont sym type todo))))))))
    ((inter:bind-rec funs body)
     (match ctx
      ((context done sym type todo)
       (process body (context (lambda: ((expr : anf:expression)) (anf:bind-rec (map process-function funs) expr)) sym type todo)))))
    (else
     (error 'transform "Unsupported remaining form ~a" expr))))
  (top-process expr ty))
  

