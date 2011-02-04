#!/usr/bin/env racket
#lang racket/base

(require "driver.rkt")


(define source-path (vector-ref (current-command-line-arguments) 2))
(define destination-path (vector-ref (current-command-line-arguments) 1))

(define program (full-compile source-path))

(write-program program destination-path)

