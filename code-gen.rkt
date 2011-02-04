#lang racket/base

(require (planet endobson/llvm/llvm-simple) (planet endobson/llvm/llvm) (planet endobson/llvm/llvm-simple-base)) 
(require "lifted-ast.rkt")

(provide compile-program)

(define (compile-program prog)
 (define context (LLVMContextCreate))
 (define module (llvm-create-module "program" #:context context))
 (enter-module/32 context module
  (define main-function
    (llvm-add-function
     (llvm-fun-type (llvm-int-type)
      (llvm-int-type) (llvm-ptr-type (llvm-ptr-type (llvm-int8-type))))
     "main"))
  (define exit-function
   (llvm-add-function
    (llvm-fun-type (llvm-void-type) (llvm-int-type))
    "exit"))
  (define main-entry (LLVMAppendBasicBlockInContext (current-context) main-function "entry"))
  (LLVMPositionBuilderAtEnd (current-builder) main-entry)
  (llvm-call exit-function 2)
  (llvm-ret 1)
  )
 module)

