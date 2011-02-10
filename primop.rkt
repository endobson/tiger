#lang typed/racket/base

(provide (all-defined-out))




(define-type primop
 (U math-primop call-closure-primop unit-primop integer-constant-primop string-constant-primop))

(struct: math-primop ((symbol : (U '+ '- '* '/ '= '<> '<= '>= '< '> '& '\|))))
(struct: unit-primop ())
(struct: call-closure-primop ())
(struct: integer-constant-primop (val : Exact-Integer))
(struct: string-constant-primop (val : String))
(struct: nil-primop ())
