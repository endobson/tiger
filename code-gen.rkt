#lang racket/base

(require
 (planet endobson/llvm/llvm-simple)
 (planet endobson/llvm/llvm)
 (planet endobson/llvm/llvm-simple-base)) 

(require racket/match racket/list)
(require "lifted-ast.rkt" "core-ast.rkt")

(provide compile-program write-program)




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
  (compile-expr (lifted-program-expr prog))
  (llvm-ret 0)


  )
 module)


(define (compile-expr expr)
 (define (compile env)
  (define (recur expr)
   (match expr
    ((integer-literal x) x)
    ((string-literal s) (error 'compile "Not Yet Implemented: string-literal"))
    ((negation x) (llvm- 0 (recur x)))
    ((math op left right)
     (let ((l (recur left)) (r (recur right)))
      (compile-math op l r)))
    ((sequence exprs)
     (foldr (lambda (e acc) (recur e)) #f exprs))
    (else
     (error 'compile "Not yet implemented: ~a" expr))))
  recur)

 (define (compile-math op l r)
  (error 'compile-math "Not Yet implemented"))

 ((compile #f) expr))


(define (write-program program path)
 (LLVMWriteBitcodeToFile program path))


