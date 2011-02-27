#lang typed/racket/base

(require racket/match)
(require
 (only-in "types.rkt" type record-type-fields)
 (rename-in "ir-ast.rkt" (expression ir:expression))
 (rename-in "primop.rkt" (primop primop:primop)))

(provide ir->printable)

(define-type primop
 (U
  '+ '* '- '/ '< '<= '= '<> '>= '> '& '\|
  (List 'field-ref Symbol)
  (List 'field-set! Symbol)
  'array-ref
  'array-set!
  'call
  'box-ref
  'box-set!
  'unit
  Integer
  String
  'nil
  (List 'call-known Symbol)
  (List 'runtime Symbol)
  'create-box
  'create-array
  (Pair 'create-record (Listof Symbol))))




(define-type expression (Rec expression
 (U
  (Pair primop (Listof expression))
  (List 'seq expression expression)
  (List 'let (List Symbol expression) expression)
  (List 'if expression expression expression)
  (List 'letrec (Listof (List Symbol (List 'function Symbol (Listof Symbol) expression))) expression)
  Symbol)))

(define-type function-declaration (List Symbol (List 'function Symbol (Listof Symbol) expression)))



(: ir->printable (ir:expression -> expression))
(define (ir->printable ir)
 (match ir
  ((sequence f n)
   (list 'seq (ir->printable f) (ir->printable n)))
  ((conditional c t f ty)
   (list 'if (ir->printable c) (ir->printable t) (ir->printable f)))
  ((bind var type expr body)
   (list 'let (list var (ir->printable expr)) (ir->printable body)))
  ((identifier name) name)
  ((primop-expr op args) (cons (primop->printable op) (map ir->printable args)))
  ((bind-rec funs body)
   (list 'letrec (map function->printable funs) (ir->printable body)))
  (else (error 'ir->printable "Not handled ~a" ir))))


(: primop->printable (primop:primop -> primop))
(define (primop->printable op)
 (match op
  ((math-primop sym) sym)
  ((equality-primop eql ty) (if eql '= '<>))
  ((unit-primop) 'unit)
  ((call-closure-primop ty) 'call)
  ((call-known-function-primop ty name) (list 'call-known name))
  ((integer-constant-primop v) v)
  ((string-constant-primop v) v)
  ((nil-primop ty) 'nil)
  ((runtime-primop ty name) (list 'runtime name))
  ((box-set!-primop ty) 'box-set!)
  ((box-ref-primop ty) 'box-ref)
  ((array-set!-primop ty) 'array-set!)
  ((array-ref-primop ty) 'array-ref)
  ((field-set!-primop ty name) (list 'field-set! name))
  ((field-ref-primop ty name) (list 'field-ref name))
  ((create-box-primop ty) 'create-box)
  ((create-array-primop ty) 'create-array)
  ((create-record-primop ty) (cons 'create-record (map (inst car Symbol type) (record-type-fields ty))))
  (else (error 'primop->printable "Not handled ~a" op))))

(: function->printable ((Pair Symbol function) -> function-declaration))
(define (function->printable pair)
 (list (car pair)
  (match (cdr pair)
   ((function name args return body)
    (list 'function name (map (inst car Symbol type) args) (ir->printable body))))))
