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
          (hash-set! renamed current-sym (pos-rename arg))
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
  (define (top-process expr function-return-type)

   (: process (inter:expression type context -> anf:expression))
   (define (process expr return-type ctx)
    (match expr
     ((inter:identifier name) (fill-context ctx name))
     ((inter:primop-expr op args)
      (match ctx
       ((context done sym ty todo)
        (: arg-processor ((Listof Symbol) (Listof inter:expression) (Listof type) -> (anf:expression -> anf:expression) -> anf:expression))
        (define ((arg-processor done-args next-args next-arg-types) cont)
         (if (empty? next-args)
             (fill-context (context cont sym ty todo) (cons op (map pos-rename (reverse done-args))))
             (let ((next-sym (gensym 'primop-arg)))
              (process (first next-args) return-type (context cont next-sym (first next-arg-types) (arg-processor (cons next-sym done-args) (rest next-args) (rest next-arg-types)))))))
             
        ((arg-processor empty args (primop-arg-types op)) done))))
     ((inter:conditional c t f ty)
      (let ((clos-name (gensym 'condcont)) (arg-name (gensym 'condcont-arg)) (result-name (gensym 'if-result)))
       (match ctx
        ((context done sym ty todo)
         (process c return-type
                    (context (lambda: ((expr : anf:expression))
                               (done
                                (anf:bind-rec
                                    (list
                                    
                                     (cons clos-name
                                 (anf:function (gensym 'condcont-fun)
                                               (list (cons arg-name ty))
                                               return-type
                                               (fill-context (context (inst values anf:expression) sym ty todo) arg-name))))
                                 expr)))
                             result-name
                             int-type
                             (lambda: ((finish : (anf:expression -> anf:expression))) 
                              (: proc (inter:expression -> anf:expression))
                              (define (proc expr)
                               (let ((tf-name (gensym 'cond-val)))
                                (process expr return-type
                                              (context (inst values anf:expression) tf-name ty
                                               (lambda: ((cont : (anf:expression -> anf:expression)))
                                                (let ((result-name (gensym 'result)))
                                                 (cont (anf:bind-primop result-name return-type (call-closure-primop (make-function-type (list ty) return-type))
                                                        (list clos-name (pos-rename tf-name)) (anf:return result-name)))))))))
                              (finish (anf:conditional (pos-rename result-name) (proc t) (proc f) return-type)))))))))
     ((inter:bind var ty expr body)
      (match ctx
       ((context done sym type todo)
        (process
         expr
         return-type
         (context done var ty (lambda: ((cont : (anf:expression -> anf:expression))) (process body return-type (context cont sym type todo))))))))
     ((inter:bind-rec funs body)
      (match ctx
       ((context done sym type todo)
        (process body return-type (context (lambda: ((expr : anf:expression)) (done (anf:bind-rec (map process-function funs) expr))) sym type todo)))))
     (else
      (error 'transform "Unsupported remaining form ~a" expr))))

   (let ((return (gensym 'retval)))
    (process expr function-return-type
     (context (inst values anf:expression)
              return
              function-return-type
              (lambda: ((cont : (anf:expression -> anf:expression)))
               (cont (anf:return (pos-rename return))))))))


  (top-process expr ty))
  

