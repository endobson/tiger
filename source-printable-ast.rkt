#lang typed/racket/base


(define-type binary-operator (U '+ '* '- '/ '< '<= '= '<> '>= '> '& '\|))

(define-type type-declaration 
 (List Symbol
  (Rec type
   (U Symbol
      (List 'array Symbol)
      (List 'record (Listof (List Symbol Symbol)))
      (List type '-> type)
      (List (Listof type) '-> type)))))

(define-type function-declaration
 (U 
  (List Symbol (Listof (List Symbol Symbol)) expression)
  (List Symbol (Listof (List Symbol Symbol)) ': Symbol expression)))

(define-type expression (Rec expression
 (U
  (List binary-operator expression expression)
  (List '- expression)
  (Pair 'seq (Listof expression))
  (List 'while expression expression)
  (List 'for Symbol expression expression expression)
  (List ':= expression expression)
  (List 'if expression expression expression)
  (List 'field-ref Symbol expression)
  (List 'array-ref expression expression)
  (List 'funcall expression (Listof expression))
  (List 'break)
  (List 'let (List Symbol expression) expression)
  (List 'let (List Symbol ': Symbol expression) expression)
  (List 'let-types (Listof type-declaration) expression)
  (List 'letrec (Listof function-declaration) expression)
  Symbol)))
