#!/usr/bin/env racket
#lang racket/base

(require
 "tiger-parser.rkt"
 "semantic-checks.rkt"
 "lifter.rkt")


(define source-path (vector-ref (current-command-line-arguments) 2))

(define source-program
 (type-check
  (rename-variables (parse (open-input-file source-path)) global-environment)
  global-type-environment))

(unless (break-check source-program)
 (eprintf "Exposed break~n")
 (exit 1))


