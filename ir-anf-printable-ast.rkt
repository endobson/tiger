#lang typed/racket/base

(require racket/match)
(require
 "unique.rkt"
 (only-in "types.rkt" type record-type-fields)
 (rename-in "ir-anf-ast.rkt" (expression ir:expression))
 (rename-in "primop.rkt" (primop primop:primop)))

(provide (rename-out (ir->printable anf->printable)))

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
  'undef
  Integer
  String
  'nil
  (List 'call-known Symbol)
  (List 'call-runtime Symbol)
  (List 'runtime Symbol)
  'create-box
  'create-array
  (Pair 'create-record (Listof Symbol))))




(define-type print-expression (Rec expression
 (U
  (List 'let (List Symbol (Pair primop (Listof Symbol))) expression)
  (List 'if Symbol expression expression)
  (List 'letrec (Listof (List Symbol (List 'function Symbol (Listof Symbol) expression))) expression)
  Symbol)))

(define-type function-declaration (List Symbol (List 'function Symbol (Listof Symbol) print-expression)))



(: ir->printable (ir:expression -> print-expression))
(define (ir->printable ir)
 (match ir
  ((conditional c t f ty)
   (list 'if (unique->symbol c) (ir->printable t) (ir->printable f)))
  ((bind-primop var type op args body)
   (list 'let (list (unique->symbol var) ((inst cons primop (Listof Symbol)) (primop->printable op) (map unique->symbol args))) (ir->printable body)))
  ((return name) (unique->symbol name))
  ((bind-rec funs body)
   (list 'letrec (map function->printable funs) (ir->printable body)))
  (else (error 'ir->printable "Not handled ~a" ir))))


(: primop->printable (primop:primop -> primop))
(define (primop->printable op)
 (match op
  ((math-primop sym) sym)
  ((equality-primop eql ty) (if eql '= '<>))
  ((unit-primop) 'unit)
  ((undefined-primop ty) 'undef)
  ((call-closure-primop ty) 'call)
  ((call-known-function-primop ty name) (list 'call-known (unique->symbol name)))
  ((call-known-runtime-primop ty name) (list 'call-runtime name))
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

(: function->printable ((Pair unique function) -> function-declaration))
(define (function->printable pair)
 (list (unique->symbol (car pair))
  (match (cdr pair)
   ((function name args return body)
    (list 'function (unique->symbol name) (map unique->symbol (map (inst car unique type) args)) (ir->printable body))))))
