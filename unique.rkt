#lang typed/racket/base


(require racket/match)
(provide unique gen-uniq re-uniq unique->symbol unique?)

(define-struct: unique ((name : Symbol) (index : Exact-Positive-Integer)) #:transparent)



(: gen-uniq (Symbol -> unique))
(define (gen-uniq sym)
 (unique sym (get-index!)))

(: re-uniq (unique -> unique))
(define (re-uniq unq)
 (match unq
  ((unique name old-index)
   (unique name (get-index!)))))



(: unique->symbol (unique -> Symbol))
(define (unique->symbol unq)
 (match unq
  ((unique name index)
   (string->symbol (string-append (symbol->string name) (number->string index))))))


(define get-index!
 (let: ((counter : Exact-Positive-Integer  1))
  (lambda ()
   (begin0 counter
    (set! counter (add1 counter))))))


