#lang racket/base

(require "tiger-parser.rkt" "semantic-checks.rkt" "lifter.rkt" "code-gen.rkt")

(provide full-compile)

(define (check-semantics ast)
 (let ((ast (rename-variables ast global-environment)))
  (let ((ast (type-check ast global-type-environment)))
   (unless (break-check ast)
    (error 'check-semantics "Break with no enclosing loop"))
   ast)))



(define (full-compile s/p)
 (compile-program (source-ast->lifted-ast (check-semantics (parse s/p)))))

