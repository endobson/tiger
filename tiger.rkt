#!/usr/bin/env racket
#lang racket/base

(require
 "tiger-parser.rkt"
 "semantic-checks.rkt"
 "lifter.rkt")


(define source-program
 (type-check
  (rename-variables (parse (current-input-port)) global-environment)
  global-type-environment))

(unless (break-check source-program)
 (eprintf "Exposed break~n")
 (exit 1))


