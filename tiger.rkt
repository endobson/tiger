#!/usr/bin/env racket
#lang racket/base

(require
 "tiger-parser.rkt"
 "semantic-checks.rkt")


(define source-program
 (type-check (rename-variables (parse (current-input-port)))))



