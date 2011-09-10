#lang racket/base

(require (for-syntax racket/base))

(require
 "tiger-parser.rkt"
 "semantic-checks.rkt"
 "type-checker.rkt"
 "environment.rkt"
 "fix-loops.rkt"
 "fix-assignment.rkt"
 "fix-units-anf.rkt"
 "ir-anf-printable-ast.rkt"
 "types.rkt"
 "unique.rkt"
 "optimization/inline-one-use.rkt"
 "optimization/zero-cfa.rkt"
 "optimization/inline-trivial.rkt"
 "optimization/remove-empty-bind-rec.rkt"
 "optimization/remove-unused-variable-bindings.rkt"
 "optimization/known-function-optimization.rkt"
 (prefix-in ir: "anf-typechecker.rkt")
 (only-in "intermediate-ast.rkt" (type-of inter:type-of))
 
 )

(require "anf-lifter.rkt" "code-gen.rkt")

(require (prefix-in source->inter: "source-intermediate-transform.rkt"))
(require (prefix-in inter->anf: "intermediate-anf-transform.rkt"))

(require racket/file racket/system racket/pretty)

(provide full-compile compile-llvm)



(define-syntax (with-temporary-file stx)
 (syntax-case stx ()
  ((_ id body bodies ...)
   #'(let ((id (make-temporary-file)))
      (dynamic-wind
       (let ((first #t)) (lambda () (if first (set! first #f) (error 'run-program "Re-entering protected region"))))
       (lambda () body bodies ...)
       (lambda ()
        (when (file-exists? id)
         (delete-file id))))))))




(define (check-semantics ast)
 (let ((ast (rename-variables ast global-environment)))
  (let ((ast (type-check ast global-type-environment)))
   (unless (break-check ast)
    (error 'check-semantics "Break with no enclosing loop"))
   ast)))


(define (simplify ast)
 (let ((inter
   (fix-loops
     (remove-assignment
      (source->inter:transform 
       ast
       source->inter:global-env
       source->inter:global-type-env)))))
  (let ((ir (inter->anf:transform inter (inter:type-of inter))))
  ;(eprintf "Checking CPS types~n")
  ;(pretty-write inter)
  ;(pretty-write (anf->printable ir))
  (ir:type-check ir)
  ;(eprintf "CPS types Passed~n")
  (let ((ir (remove-units ir)))
   ;(eprintf "Checking unit removed types~n")
   ;(pretty-write (anf->printable ir))
   (ir:type-check ir)
   ;(eprintf "Unit removed types passed~n")
   ir))))

(define (optimize ir)
 (define (simple-optimize ir)
  (let ((ir (remove-units ir)))
   (let ((ir (inline-once-used ir)))
    (let ((ir (inline-trivial ir)))
     (let ((ir (remove-empty-bind-rec ir)))
      (let ((ir (remove-unused-variable-bindings ir)))
       (let ((ir (known-function-optimization ir)))
       ;(ir:type-check ir)
       ir)))))))

 (let loop ((ir ir))
  (let ((new-ir (simple-optimize ir)))
   (if (equal? new-ir ir)
       new-ir
       (loop new-ir)))))

(define (analyze ir)
 
 (let ((zcfa-results (zero-cfa ir)))
  (for (((variable functions) zcfa-results))
   (printf "~a: ~a~n" (unique->symbol variable) (hash-map functions (lambda (k t) (unique->symbol k))))))

 ir)


(define (source->ir s/p)
 (simplify (check-semantics (parse s/p))))

(define (full-compile s/p mode)
 (case mode
  ((llvm-opt) (optimize-llvm (compile-program (lift (optimize (source->ir s/p))))))
  ((llvm) (compile-program (lift (optimize (source->ir s/p)))))
  ((lifted) (lift (optimize (source->ir s/p))))
  ((ir) (optimize (source->ir s/p)))
  (else (error 'full-compile "Unknown mode ~a" mode))))



(define (compile-llvm program exe-path-string)
 (define exe-path 
  (cond
   ((string? exe-path-string) (string->path exe-path-string))
   ((path? exe-path-string) exe-path-string)))
 (with-temporary-file bitcode
  (with-temporary-file assembly
   (with-temporary-file object
    (write-program program bitcode)
    (system* "/usr/bin/env" "llc" "-O2" "-disable-cfi" "-march" "x86" "-o" (path->string assembly) (path->string bitcode))
    (case (system-type 'os)
     ((macosx)
      (system* "/usr/bin/env" "as" "-arch" "i686" "-o" (path->string object) (path->string assembly)))
     ((unix)
      (system* "/usr/bin/env" "as" "-march" "i686" "-o" (path->string object) (path->string assembly)))
     (else (error 'compile-llvm "Unknown System type")))
    (system* "/usr/bin/env" "clang" (path->string object) "-Wl,-no_pie" "-l" "m"  "-arch" "i386" "-o" (path->string exe-path))))))




