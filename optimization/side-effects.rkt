#lang typed/racket/base

(require "../primop.rkt")
(require racket/match)

(provide primop->side-effect)

(struct: side-effect ())

(: primop->side-effect (primop -> (Option side-effect)))
(define (primop->side-effect op)
 (match op
  ((integer-constant-primop n) #f)
  ((string-constant-primop str) #f)
  ((unit-primop) #f)
  ((nil-primop ty) #f)
  ((runtime-primop ty name) #f) ;This just returns the primop
  ((call-known-runtime-primop ty name) (side-effect)) ;Over approximation
  ((math-primop sym) #f)
  ((equality-primop eql ty) #f)
  ((call-closure-primop ty) (side-effect)) ;Over approximation
  ((call-known-function-primop ty name) (side-effect)) ;Over approximation
  ((create-box-primop ty) #f)
  ((create-array-primop ty) #f)
  ((create-record-primop ty) #f)
  ((box-ref-primop ty) #f)
  ((array-ref-primop ty) #f)
  ((field-ref-primop ty name) #f)
  ((box-set!-primop ty) (side-effect))
  ((array-set!-primop ty) (side-effect))
  ((field-set!-primop ty name) (side-effect))
  (else (error 'primop->side-effect "Not yet implemented ~a" op))))

