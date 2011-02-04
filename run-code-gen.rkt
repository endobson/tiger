#lang racket/base

(require racket/file racket/system)
(require (planet endobson/llvm/llvm) "driver.rkt")

(provide run-program write-program)

(define (run-program program)
 (define tmp-bitcode (make-temporary-file "llvm~a.bc"))
 (define tmp-executable (make-temporary-file "llvm~a"))
 (dynamic-wind
  (let ((first #t)) (lambda () (if first (set! first #f) (error 'run-program "Re-entering protected region"))))
  (lambda ()
   (when (zero? (LLVMWriteBitcodeToFile program tmp-bitcode))
    (when (system* "/usr/bin/env" "llvmc" "-clang" "-o" (path->string tmp-executable) (path->string tmp-bitcode))
     (system*/exit-code (path->string tmp-executable)))))
  (lambda ()
   (when (file-exists? tmp-bitcode)
    (delete-file tmp-bitcode))
   (when (file-exists? tmp-executable)
    (delete-file tmp-executable)))))

(define (write-program program path)
 (LLVMWriteBitcodeToFile program path))

 
