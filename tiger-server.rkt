#!/usr/bin/env racket
#lang racket/base

(require racket/match)
(require "driver.rkt")

(let loop ()
 (let ((input (read)))
  (unless (eof-object? input)
   (match input
    ((list source-file binary-file output-file error-file)
     (call-with-output-file error-file #:exists 'truncate
      (lambda (error-port)
       (parameterize ((current-error-port error-port))
        (write
         (with-handlers ((exn:fail? (lambda (exn) ((error-display-handler) (exn-message exn) exn) 1)))
          (call-with-output-file output-file #:exists 'truncate
           (lambda (output-port)
            (parameterize ((current-output-port output-port))
             (call-with-input-file source-file
              (lambda (input-port)
               (let ((program (full-compile input-port)))
                (if (compile-llvm program binary-file) 0 1)))))))))
        (newline)
        (flush-output)
        (flush-output error-port))))))
   (loop))))
                
               
             
             



       
