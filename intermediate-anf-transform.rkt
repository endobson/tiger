#lang typed/racket/base


(require racket/match racket/list)
(require (prefix-in anf: "ir-anf-ast.rkt")
         (prefix-in inter: "intermediate-ast.rkt")
         "primop.rkt" "unique.rkt"
         "types.rkt")

(provide transform)

(struct: context
 ((done : (anf:expression -> anf:expression))
  (current-sym : unique)
  (temporary-name : Boolean)
  (current-type : type)
  (todo : ((anf:expression -> anf:expression) -> anf:expression))))


(: transform (inter:expression type -> anf:expression))
(define (transform expr ty)
  (: renamed (HashTable unique unique))
  (define renamed (make-hash))
  
  (: pos-rename (unique -> unique))
  (define (pos-rename sym)
   (hash-ref renamed sym (lambda () sym)))

  (: fill-context (context (U unique (Pair primop (Listof unique))) -> anf:expression))
  (define (fill-context ctx arg)
   (match ctx
    ((context done current-sym temporary-name current-ty todo)
     (if (unique? arg)
         (begin
          (hash-set! renamed current-sym (pos-rename arg))
          (todo done))
         (begin
          (let ((new-sym (if temporary-name (gen-uniq (primop-name (first arg))) current-sym)))
            (when temporary-name (hash-set! renamed current-sym new-sym))
            (todo (lambda: ((expr : anf:expression))
                    (done (anf:bind-primop new-sym current-ty (first arg) (rest arg) expr))))))))))

  (: process-function ((Pair unique inter:function) -> (Pair unique anf:function)))
  (define (process-function p)
   (cons (car p)
    (match (cdr p)
     ((inter:function args ret body)
      (anf:function (re-uniq (car p)) args ret (top-process body ret))))))
    

  (: top-process (inter:expression type -> anf:expression))
  (define (top-process expr function-return-type)

   (: process (inter:expression type context -> anf:expression))
   (define (process expr return-type ctx)
    (match expr
     ((inter:identifier name) (fill-context ctx name))
     ((inter:primop-expr op args)
      (match ctx
       ((context done sym temp ty todo)
        (: arg-processor ((Listof unique) (Listof inter:expression) (Listof type) -> (anf:expression -> anf:expression) -> anf:expression))
        (define ((arg-processor done-args next-args next-arg-types) cont)
         (if (empty? next-args)
             (fill-context (context cont sym temp ty todo) (cons op (map pos-rename (reverse done-args))))
             (let ((next-sym (gen-uniq (primop-name op))))
              (process (first next-args) return-type (context cont next-sym #t (first next-arg-types) (arg-processor (cons next-sym done-args) (rest next-args) (rest next-arg-types)))))))
             
        ((arg-processor empty args (primop-arg-types op)) done))))
     ((inter:conditional c t f ty)
      (let ((clos-name (gen-uniq 'condcont)) (arg-name (gen-uniq 'condcont-arg)) (result-name (gen-uniq 'if-result)))
       (match ctx
        ((context done sym temp ty todo)
         (process c return-type
                    (context (lambda: ((expr : anf:expression))
                               (done
                                (anf:bind-rec
                                    (list
                                    
                                     (cons clos-name
                                 (anf:function (gen-uniq 'condcont-fun)
                                               (list (cons arg-name ty))
                                               return-type
                                               (fill-context (context (inst values anf:expression) sym temp ty todo) arg-name))))
                                 expr)))
                             result-name
                             #t
                             int-type
                             (lambda: ((finish : (anf:expression -> anf:expression))) 
                              (: proc (inter:expression -> anf:expression))
                              (define (proc expr)
                               (let ((tf-name (gen-uniq 'cond-val)))
                                (process expr return-type
                                              (context (inst values anf:expression) tf-name #t ty
                                               (lambda: ((cont : (anf:expression -> anf:expression)))
                                                (let ((result-name (gen-uniq 'result)))
                                                 (cont (anf:bind-primop result-name return-type (call-closure-primop (make-function-type (list ty) return-type))
                                                        (list clos-name (pos-rename tf-name)) (anf:return result-name)))))))))
                              (finish (anf:conditional (pos-rename result-name) (proc t) (proc f) return-type)))))))))
     ((inter:bind var ty expr body)
      (match ctx
       ((context done sym temp type todo)
        (process
         expr
         return-type
         (context done var #f ty (lambda: ((cont : (anf:expression -> anf:expression))) (process body return-type (context cont sym temp type todo))))))))
     ((inter:bind-rec funs body)
      (match ctx
       ((context done sym temp type todo)
        (process body return-type (context (lambda: ((expr : anf:expression)) (done (anf:bind-rec (map process-function funs) expr))) sym temp type todo)))))
     (else
      (error 'transform "Unsupported remaining form ~a" expr))))

   (let ((return (gen-uniq 'retval)))
    (process expr function-return-type
     (context (inst values anf:expression)
              return
              #t
              function-return-type
              (lambda: ((cont : (anf:expression -> anf:expression)))
               (cont (anf:return (pos-rename return))))))))


  (let ((ign (gen-uniq 'ignored)))
   (top-process (inter:bind ign ty expr (inter:primop-expr (unit-primop) empty)) unit-type)))
  

