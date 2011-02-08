#lang racket/base

(require (for-syntax racket/base))

(require "tiger-parser.rkt" "semantic-checks.rkt" "lifter.rkt" "code-gen.rkt" "type-checker.rkt" "environment.rkt")

(require racket/file racket/system)

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



(define (full-compile s/p)
 (compile-program (source-ast->lifted-ast (check-semantics (parse s/p)))))


(define (compile-llvm program exe-path-string)
 (define exe-path 
  (cond
   ((string? exe-path-string) (string->path exe-path-string))
   ((path? exe-path-string) exe-path-string)))
 (with-temporary-file bitcode
  (with-temporary-file assembly
   (with-temporary-file object
    (if (zero? (write-program program bitcode))
        (and
         (system* "/usr/bin/env" "llc" "-O2" "-o" (path->string assembly) (path->string bitcode))
         (case (system-type 'os)
          ((macosx)
           (system* "/usr/bin/env" "as" "-arch" "i686" "-o" (path->string object) (path->string assembly)))
          ((unix)
           (system* "/usr/bin/env" "as" "-march" "i686" "-o" (path->string object) (path->string assembly)))
          (else (error 'compile-llvm "Unknown System type")))
         (system* "/usr/bin/env" "clang" (path->string object) "-o" (path->string exe-path)))
        #f)))))




