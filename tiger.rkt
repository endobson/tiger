#!/usr/bin/env racket
#lang racket/base

(require "driver.rkt")
(require racket/cmdline)

(define-values (source-path destination-path) 
 (let ((source-path #f) (destination-path "a.out"))
  (command-line
   #:once-each
   ("-o" output-file "Set the output file" (set! destination-path output-file))
   #:args (source-file)
    (set! source-path source-file)
    (values source-path destination-path))))


(define program (full-compile (open-input-file source-path)))

(unless (compile-llvm program destination-path)
 (exit 1))

