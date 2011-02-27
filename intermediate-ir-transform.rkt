#lang typed/racket/base

(require racket/match racket/list)
(require (prefix-in ir: "ir-ast.rkt")
         (prefix-in inter: "intermediate-ast.rkt")
         "types.rkt")

(provide transform)

(: transform-functions ((Listof (Pair Symbol inter:function)) -> (Listof (Pair Symbol ir:function))))
(define (transform-functions funs)
 (map (inst cons Symbol ir:function)
   (map (inst car Symbol inter:function) funs)
   (map transform-function funs)))

(: transform-function ((Pair Symbol inter:function) -> ir:function))
(define (transform-function pair)
 (match (cdr pair)
  ((inter:function args ty body)
   (ir:function (gensym (car pair)) args ty (transform body)))))

(: transform (inter:expression -> ir:expression))
(define (transform expr)
 (match expr
  ((inter:identifier name) (ir:identifier name))
  ((inter:primop-expr op args)
   (ir:primop-expr op (map transform args)))
  ((inter:conditional c t f ty)
   (ir:conditional (transform c) (transform t) (transform f) ty))
  ((inter:bind v ty expr body)
   (ir:bind v ty (transform expr) (transform body)))
  ((inter:bind-rec funs body)
   (ir:bind-rec (transform-functions funs) (transform body)))
  (else
   (error 'transform "Unsupported remaining form ~a" expr))))
  

