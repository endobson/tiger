#!/usr/bin/env racket
#lang racket/base

(require (for-syntax racket/base))
(require "driver.rkt" "ir-anf-printable-ast.rkt")
(require racket/cmdline racket/pretty)

(require (planet endobson/llvm/llvm-simple))

(define-values (source-path destination-path output-mode) 
 (let ((source-path #f) (destination-path 'default) (output-mode 'binary))
  (command-line
   #:once-each
   ("-o" output-file "Set the output file" (set! destination-path output-file))
   #:once-any
   ("--binary" "outputs the executable" (set! output-mode 'binary))
   ("--llvm" "outputs llvm bitcode" (set! output-mode 'llvm))
   ("--ir" "outputs ir representation" (set! output-mode 'ir))
   ("--lifted" "outputs lifted representation" (set! output-mode 'lifted))
   #:args (source-file)
    (set! source-path source-file)
    (values source-path destination-path output-mode))))



(define-syntax (set-output-file stx)
 (syntax-case stx ()
  ((_ exprs ...)
   #'((lambda (thunk)
       (if (equal? destination-path 'default) (thunk)
         (with-output-to-file destination-path (thunk)))) (lambda () exprs ...)))))


(define program
 (let ((mode (if (equal? 'binary output-mode) 'llvm output-mode)))
  (call-with-input-file source-path (lambda (port) (full-compile port mode)))))

(if (equal? output-mode 'binary) (void (compile-llvm program (if (equal? destination-path 'default) "a.out" destination-path)))
 (set-output-file
  (case output-mode
   ((llvm) (display (llvm-module-description program)))
   ((ir) (pretty-write (anf->printable program)))
   ((lifted) (pretty-write program)))))

