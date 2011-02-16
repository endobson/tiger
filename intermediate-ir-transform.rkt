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
   (map transform-function (map (inst cdr Symbol inter:function) funs))))

(: transform-function (inter:function -> ir:function))
(define (transform-function fun)
 (match fun
  ((inter:function args ty body)
   (ir:function args ty (transform body)))))

(: transform (inter:expression -> ir:expression))
(define (transform expr)
 (match expr
  ((inter:identifier name) (ir:identifier name))
  ((inter:primop-expr op args)
   (ir:primop-expr op (map transform args)))
  ((inter:conditional c t f)
   (ir:conditional (transform c) (transform t) (transform f)))
  ((inter:bind v ty expr body)
   (ir:bind v ty (transform expr) (transform body)))
  ((inter:bind-rec funs body)
   (ir:bind-rec (transform-functions funs) (transform body)))
  ((inter:sequence first next)
   (ir:sequence (transform first) (transform next)))
  (else
   (error 'transform "Unsupported remaining form ~a" expr))))
  
